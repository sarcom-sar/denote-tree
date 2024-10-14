;;; denote-tree.el --- Visualize your notes as a tree -*- lexical-binding: t -*-

;; Copyright 2024, Sararin
;; Created: 2024-09-15 Sun
;; Version: 0.5.0
;; Keywords: convenience
;; URL: http://github.com/sarcom-sar/denote-tree.el
;; Package-Requires: ((emacs "27.2") (compat "29.1"))

;; This file is not part of GNU Emacs.

;; denote-tree is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.

;; denote-tree is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; denote-tree.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; denote-tree visualizes your notes as a tree.  It starts from current buffer
;; or, if prompted, from any buffer and allows you to move within it with
;; standard GNU Emacs movement keys and enter notes as necessary.
;;
;; Visualization:
;;
;; '-* Lorem ipsum dolor sit amet, consectetuer adipiscing elit.
;;   +-* Phasellus at dui in ligula mollis ultricies.
;;   | '-* Nullam libero mauris, consequat quis, varius et, dictum id, arcu.
;;   |   '-* Nulla posuere.
;;   +-* Nunc aliquet, augue nec adipiscing interdum,
;;   | '-* Donec vitae dolor.
;;   '-* Proin quam nisl, tincidunt et, mattis eget, convallis nec, purus.
;;   | +-* Nam euismod tellus id erat.
;;   | '-* Donec pretium posuere tellus.
;;   |   '-* Nullam libero mauris, consequat quis, varius et, dictum id, arcu.
;;   +-* Vestibulum convallis, lorem a tempus semper.
;;   '-* Dui dui euismod elit, vitae placerat urna tortor vitae lacus.
;;
;; That is pretty much it.  It is able to handle cyclical nodes and provides a
;; mechanism to move between those cyclical nodes (called "teleportations") by
;; default.  As a drawback, it is pretty stupid and has to redraw entire thing
;; from scratch if anything changes.
;;
;; User can customize `denote-tree-node-face' and
;; `denote-tree-circular-node-face' to make them more visible.  With a bit of
;; hacking it is also feasible to implement colored node titles.
;;
;; The package as of right now hard-depends on denote (duh).
;;

;;; Code:

(eval-when-compile
  (require 'denote)
  (require 'compat))


;; Faces and Custom

(defface denote-tree-node-face '((t :inherit link))
  "Default face used for nodes.")

(defface denote-tree-circular-node-face '((t :inherit link-visited))
  "Default face used for circular nodes.")

(defcustom denote-tree-buffer-name "*denote-tree*"
  "Name of the buffer denote-tree will be built in.")

(defcustom denote-tree-title-colorize-function #'denote-tree--default-props
  "Function accepting one argument STR.
Returns propertied string STR.")


;; Vars and consts

(defconst denote-tree-lower-knee "'-")
(defconst denote-tree-tee "+-")
(defconst denote-tree-space "  ")
(defconst denote-tree-pipe "| ")
(defconst denote-tree-node "* ")

(defvar denote-tree--cyclic-trees '()
  "List of all partial trees that contain cycles.")

(defvar denote-tree--visited-buffers '()
  "List of already created buffers.")

(defvar denote-tree--cyclic-buffers '()
  "List of buffers that are cyclic nodes.")

(defvar-local denote-tree--mark-tree '()
  "Tree of positions used by denote-tree buffer.
Used directly to traverse the tree structure.")

(defvar-local denote-tree--pointer '()
  "Node the point is at.")

(defvar-local denote-tree--node-stack '()
  "Stack of parent nodes.")

(defvar-local denote-tree--pos-stack '()
  "Stack of point position in parent nodes.")

(defvar-local denote-tree--closure nil
  "Closure of current instance of `denote-tree--movement-maker'.")


;; Mode and interactive functions

(defvar-keymap denote-tree-mode-map
  :parent special-mode-map
  :doc "Keymap for denote-tree-mode."
  "n" #'denote-tree-next-node
  "p" #'denote-tree-prev-node
  "f" #'denote-tree-child-node
  "b" #'denote-tree-parent-node
  "RET" #'denote-tree-enter-node)

(define-derived-mode denote-tree-mode special-mode "denote-tree"
  "Visualize your denote notes as a tree.

Denote-tree visualizes every note linked to the root note in a buffer."
  :interactive nil)

;;;###autoload
(defun denote-tree (&optional buffer)
  "Draw hierarchy between denote files as a tree.

The function uses either the current buffer, if called from a function
or a BUFFER provided by the user."
  (interactive)
  (when (get-buffer denote-tree-buffer-name)
    (kill-buffer denote-tree-buffer-name))
  (unwind-protect
      (progn
        (or buffer (setq buffer (denote-tree--collect-keyword (current-buffer)
                                                              "identifier")))
        (denote-tree--open-link-maybe buffer)
        (with-current-buffer-window denote-tree-buffer-name nil nil
          (let ((inhibit-read-only t))
            (denote-tree-mode)
            (setq denote-tree--closure
                  (denote-tree--movement-maker 1 0)) ; root never has siblings
            (setq denote-tree--pointer nil)
            (setq denote-tree--mark-tree
                  (denote-tree--draw-tree (denote-tree--walk-links buffer)))
            (setq denote-tree--mark-tree
                  (denote-tree--re-circularize-tree denote-tree--mark-tree))))
        (set-window-point (get-buffer-window denote-tree-buffer-name)
                          (goto-char (1+ (length denote-tree-lower-knee)))))
    (denote-tree--clean-up)))

(defun denote-tree-enter-node ()
  "Enter node at point in other window."
  (interactive)
  (find-file-other-window
   (denote-get-path-by-id
    (get-text-property (point) 'denote--id))))

(defun denote-tree-child-node (&optional arg)
  "Move the point to the child node of a current node ARG times.
If ARG is omitted or nil, move to the child of a current node."
  (interactive "p")
  (or arg (setq arg 1))
  (let (total
        node-stack)
    ;; pointer is at root right now, bootstrap it.
    (when (null denote-tree--pointer)
      (setq denote-tree--pointer denote-tree--mark-tree))
    (dotimes (total arg)
      (when (cadr denote-tree--pointer)
        (push denote-tree--pointer denote-tree--node-stack)
        (push (funcall denote-tree--closure 0) denote-tree--pos-stack)
        (setq denote-tree--pointer (cadr denote-tree--pointer))
        (unless (setq node-stack (cdar denote-tree--node-stack))
          (setq node-stack '(1)))
        (setq denote-tree--closure (denote-tree--movement-maker
                                    (length node-stack)
                                    0))
        (goto-char (car denote-tree--pointer))))))

(defun denote-tree-parent-node (&optional arg)
  "Move the point to the parent node of a current node ARG times.
If ARG is omitted or nil, move to the parent of a current node."
  (interactive "p")
  (or arg (setq arg 1))
  (let (total
        node-stack)
    (dotimes (total arg)
      (when denote-tree--node-stack
        (setq denote-tree--pointer (pop denote-tree--node-stack))
        (unless (setq node-stack (cdar denote-tree--node-stack))
          (setq node-stack '(1)))
        (setq denote-tree--closure (denote-tree--movement-maker
                                    (length node-stack)
                                    (pop denote-tree--pos-stack)))
        (goto-char (car denote-tree--pointer))))))

(defun denote-tree-next-node (&optional arg)
  "Move the point to the next child node ARG times.
If ARG is omitted or nil, move to the next child node."
  (interactive "p")
  (or arg (setq arg 1))
  (when denote-tree--node-stack
    (setq denote-tree--pointer (nth (funcall denote-tree--closure arg)
                                    (cdar denote-tree--node-stack)))
    (goto-char (car denote-tree--pointer))))

(defun denote-tree-prev-node (&optional arg)
  "Move the point to the previous child node ARG times.
If ARG is omitted or nil, move to the previous child node."
  (interactive "p")
  (or arg (setq arg 1))
  (denote-tree-next-node (- arg)))


;; Tree traversal
;; it is a good idea to merge those functions

(defun denote-tree--walk-links (buffer)
  "Return a tree of denote links starting with current BUFFER.

The function uses basic version of 3 color DFS.  Any node that isn't
opened as a buffer is white. Buffers opened `with-current-buffer' are
gray nodes, while nodes that were previously opened as buffers are black.
The discovery of white nodes happens using `denote-tree--collect-links'."
  (let ((links-in-buffer (denote-tree--collect-links buffer)))
    (with-current-buffer buffer
      ; if no links return a buffer
      (if (null links-in-buffer)
          (list buffer)
        (let ((lst (list (denote-tree--collect-keyword buffer "identifier"))))
          ;; if links go deeper
          (dolist (el links-in-buffer lst)
            ;; this essentially checks if next node is a colored in black
            (if (and (get-buffer el)
                     (add-to-list 'denote-tree--cyclic-buffers el))
                (setq lst (append lst (list (list el))))
              (setq lst (append lst (list (denote-tree--walk-links el)))))))))))

(defun denote-tree--draw-tree (node)
  "Draw a tree in current buffer starting with NODE.

The function calls a helper `denote-tree--draw-tree-helper'."
  (denote-tree--draw-tree-helper node "" t))

(defun denote-tree--draw-tree-helper (node indent last-child-p)
  "Insert INDENT and current NODE into the buffer.
If dealing with LAST-CHILD-P of NODE, alter pretty printing."
  (let* ((point-and-indent (denote-tree--draw-node (car node)
                                                   indent
                                                   last-child-p))
         (point-star-loc (car point-and-indent))
         (indent (cdr point-and-indent))
         (lst (list point-star-loc))
         lastp)
    (dolist (el (cdr node) lst)
      (setq lastp (equal el (car (last node))))
      (setq lst (append lst (list (denote-tree--draw-tree-helper el
                                                                 indent
                                                                 lastp)))))))

(defun denote-tree--draw-node (node-name indent last-child-p)
  "Draw NODE-NAME according to INDENT in current buffer.

Insert the current line as follows INDENT `denote-tree-node' title of
current denote note.  Face of `denote-tree-node' is either
`denote-tree-circular-node-face' if current NODE-NAME is a member of
`denote-tree--cyclic-buffers' or `denote-tree-node-face' if it's not.

Return location of a point where the node starts and the current indent."
  (let ((circularp (member node-name denote-tree--cyclic-buffers))
        point-star-loc)
    (insert indent)
    (cond
     (last-child-p
      (setq indent (concat indent denote-tree-space))
      (insert denote-tree-lower-knee))
     (t
      (setq indent (concat indent denote-tree-pipe))
      (insert denote-tree-tee)))
    (setq point-star-loc (point))
    (insert (propertize denote-tree-node
                        'denote--id node-name
                        'face (if circularp
                                  'denote-tree-circular-node-face
                                'denote-tree-node-face))
            (funcall denote-tree-title-colorize-function
                     (denote-tree--collect-keyword node-name "title"))
            "\n")
    (cons point-star-loc indent)))

(defun denote-tree--re-circularize-tree (node)
  "Return the tree with cyclical structure of the original.

If a current node is in `denote-tree--cyclic-buffers', save the entire
thing to `denote-tree--cyclic-trees'.  If a current node matches the
`car' of a node, set it's `cdr' to the `cdr' of
`denote-tree--cyclic-trees'.  Return modified tree."
  (if (not (listp node))
      node
    (let* ((lst-and-skip (denote-tree--check-cyclic node (list (car node))))
           skip-this-loop lst)
      (setq skip-this-loop (car lst-and-skip))
      (setq lst (cdr lst-and-skip))
      (dolist (el (cdr node) lst)
        (unless skip-this-loop
          (setq lst (append lst (list (denote-tree--re-circularize-tree el)))))))))

(defun denote-tree--check-cyclic (node lst)
  "When NODE is cyclic, modify the LST and skip further traversal.

If NODE is encountered in `denote-tree--cyclic-buffers', then add the current
structure to `denote-tree--cyclic-trees'.  During further traversal, if the NODE
matches `car' of one of the nodes in `denote-tree--cyclic-trees' `setcdr' that
node to the `cdr' of the same element in `denote-tree--cyclic-trees'.  If that
happens return the flag skip set to t."
  (let* ((pos-to-id (get-text-property (car node) 'denote--id))
         (char-pos-of-cyclic-trees (mapcar #'car denote-tree--cyclic-trees))
         (checked-element (denote-tree--check
                           pos-to-id
                           (mapcar (lambda (el)
                                     (denote-tree--get-text-property el 'denote--id))
                                   char-pos-of-cyclic-trees)))
         skip)
    (cond
     (checked-element
      (setcdr node (cdr (nth checked-element denote-tree--cyclic-trees)))
      (setq skip t)
      (setq lst (append lst (cdr node))))
     ((denote-tree--check pos-to-id denote-tree--cyclic-buffers)
      (setq denote-tree--cyclic-trees
            (append denote-tree--cyclic-trees (list node)))))
    (cons skip lst)))

;; Helpers for Links and Buffers

(defun denote-tree--collect-links (buffer)
  "Collect all links of type denote in BUFFER."
  (setq buffer (denote-tree--open-link-maybe buffer))
  (let (found-ids)
    (with-current-buffer buffer
      (goto-char (point-min))
      (while (search-forward-regexp denote-id-regexp nil t)
        (push (concat (match-string-no-properties 1)
                      (match-string-no-properties 2))
              found-ids))
      ;; first element is /always/ the buffer's id
      (cdr (nreverse found-ids)))))

(defun denote-tree--collect-keyword (buffer keyword)
  "Return denote KEYWORD from BUFFER.
Return nil if none is found."
  (when-let ((filetype (denote-tree--find-filetype buffer)))
    (with-current-buffer buffer
      (goto-char (point-min))
      ;; if it matches anything, return that substring
      (when (cond
             ((string= keyword "title")
              (re-search-forward (plist-get filetype :title-key-regexp)
                                 nil t))
             ((string= keyword "identifier")
              (re-search-forward denote-id-regexp
                                 nil t)
              (backward-word-strictly))
             ((string= keyword "keywords")
              (re-search-forward (plist-get filetype :keywords-key-regexp)
                                 nil t))
             (t nil))
        (denote-trim-whitespace
         (buffer-substring-no-properties (point) (line-end-position)))))))

(defun denote-tree--find-filetype (buffer)
  "Guess the filetype in BUFFER and return it as a symbol."
  (let ((types denote-file-types))
    (with-current-buffer buffer
      (goto-char (point-min))
      (cdr (seq-find
            (lambda (type)
              (re-search-forward
               (plist-get (cdr type) :title-key-regexp) nil t))
            types)))))

(defun denote-tree--open-link-maybe (element)
  "Return ELEMENT buffer, create if necessary."
  (unless (member element denote-tree--visited-buffers)
    (add-to-list 'denote-tree--visited-buffers element)
    (get-buffer-create element)
    (with-current-buffer element
      (erase-buffer)
      (insert-file-contents (denote-get-path-by-id element))))
  element)

(defun denote-tree--get-text-property (element property)
  "Get text property PROPERTY at char-pos ELEMENT."
  (when element
    (get-text-property element 'denote--id)))


;; Helper functions and one closure

(defun denote-tree--movement-maker (len-list init-val)
  "Return values from 0 to LEN-LIST."
  (let ((pos init-val)
        (len len-list)
        (val))
    (lambda (direction)
      (setq pos (+ pos direction))
      (setq val (mod pos len))
      val)))

(defun denote-tree--clean-up ()
  "Clean up buffers created during the tree walk."
  (dolist (el denote-tree--visited-buffers)
    (kill-buffer el))
  (setq denote-tree--visited-buffers nil)
  (setq denote-tree--cyclic-trees nil)
  (setq denote-tree--cyclic-buffers nil))

(defun denote-tree--default-props (str)
  "Default function returning STR with properties."
  (propertize str))

(defun denote-tree--check (el lst)
  "Return the position of EL in LST if it exists.
Return nil otherwise."
  (let ((iter lst)
        (num 0))
    (while (not (or (equal el (car iter))
                    (null (setq iter (cdr iter)))))
      (setq num (1+ num)))
    (unless (equal iter nil)
      num)))

(provide 'denote-tree)
;;; denote-tree.el ends here
