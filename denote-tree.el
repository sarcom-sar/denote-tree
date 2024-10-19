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

(defvar denote-tree--visited-buffers '()
  "List of already created buffers.")

(defvar denote-tree--cyclic-buffers '()
  "List of buffers that are cyclic nodes.")


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
            (denote-tree--draw-tree buffer)))
        (set-window-point (get-buffer-window denote-tree-buffer-name)
                          (goto-char (1+ (length denote-tree-lower-knee)))))
    (denote-tree--clean-up)))

(defun denote-tree-enter-node ()
  "Enter node at point in other window."
  (interactive)
  (find-file-other-window
   (denote-get-path-by-id
    (get-text-property (point) 'denote-tree--me))))

(denote-tree--movement-generator child)
(denote-tree--movement-generator parent)
(denote-tree--movement-generator next)
(denote-tree--movement-generator prev)


;; Tree traversal
;; it is a good idea to merge those functions

(defun denote-tree--draw-tree (buffer)
  "Draw a tree in current buffer starting with BUFFER."
  (denote-tree--walk-links buffer nil "" t)
  (denote-tree--add-props-to-cycles))

(defun denote-tree--walk-links (buffer parent indent last-child-p)
  "Walk along the links starting from BUFFER.

Draw the current buffer as a node in `denote-tree-buffer-name'.  Set it's
properties. Collect all the links and call `denote-tree--walk-links' on
them recursively.  If one of the buffers was already visited do not iterate
over it."
  (let ((links-in-buffer (denote-tree--collect-links buffer))
        pos-and-indent pos lastp node-children)
    ;; draw node in buffer,
    ;; extract position of point at node
    ;; carry over the indent
    (with-current-buffer denote-tree-buffer-name
      (setq pos-and-indent (denote-tree--draw-node buffer indent last-child-p))
      (setq pos (car pos-and-indent))
      (setq indent (cdr pos-and-indent)))
    ;; traverse the buffer structure
    ;; if current buffer is in denote-tree--cyclic-buffers
    ;; do not go deeper, because you enter a cycle
    (cond
     ((assoc buffer denote-tree--cyclic-buffers #'string=)
      (setcdr (assoc buffer denote-tree--cyclic-buffers)
              (append (cdr (assoc buffer denote-tree--cyclic-buffers))
                      (list pos))))
     (t
      (dolist (el links-in-buffer)
        (when (get-buffer el)
          (add-to-list 'denote-tree--cyclic-buffers
                       (list el)
                       nil
                       (lambda (a b) (string= (car a) (car b)))))
        (setq lastp (eq el (car (last links-in-buffer))))
        (setq node-children
              (append node-children
                      (denote-tree--walk-links el buffer indent lastp))))))
    ;; add props to children of a buffer
    (with-current-buffer denote-tree-buffer-name
      (denote-tree--propertize-node pos buffer)
      (denote-tree--add-props-to-children node-children pos))
    (list pos)))

(defun denote-tree--add-props-to-cycles ()
  (with-current-buffer denote-tree-buffer-name
    (let (child-prop)
      (dolist (el denote-tree--cyclic-buffers)
        (goto-char (point-min))
        (text-property-search-forward 'denote-tree--me (car el))
        (setq child-prop (get-text-property (point) 'denote-tree--child))
        (dolist (le (cdr el))
          (goto-char le)
          (add-text-properties (pos-bol)
                               (pos-eol)
                               (list 'denote-tree--child child-prop
                                     'denote-tree--me    (car el))))))))

(defun denote-tree--draw-node (node-name indent last-child-p)
  "Draw NODE-NAME according to INDENT in current buffer.

Insert the current line as follows INDENT `denote-tree-node' title of
the current denote note.  Face of `denote-tree-node' is either
`denote-tree-circular-node-face' if current NODE-NAME is a member of
`denote-tree--cyclic-buffers' or `denote-tree-node-face' if it's not.
Call `denote-tree-title-colorize-function' on title.

Return location of a point where the node starts and the current indent."
  (let ((circularp (assoc node-name denote-tree--cyclic-buffers))
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
                        'face (if circularp
                                  'denote-tree-circular-node-face
                                'denote-tree-node-face))
            (funcall denote-tree-title-colorize-function
                     (denote-tree--collect-keyword node-name "title"))
            "\n")
    (cons point-star-loc indent)))

(defun denote-tree--propertize-node (position buffer)
  "Add properties for BUFFER at POSITION.

Create a map of local neighbors for the POSITION, so the movement commands
\"know\" where to move next.  Properties to be set is `denote-tree--me'."
  (set-text-properties position
                       (+ position (length denote-tree-node))
                       (append (text-properties-at position)
                               (list 'fontified t
                                     'denote-tree--me buffer))))

(defun denote-tree--add-props-to-children (node-children parent)
  "Iterate over NODE-CHILDREN to set node's props. Keep node's PARENT.

Every node contains props denote-tree--next, denote-tree--prev and
denote-tree--parent which contain point's position to go to get to
previous/next sibling node or a parent.  This function sets those
positions."
  (when node-children
    (save-excursion
      (goto-char parent)
      (add-text-properties (pos-bol)
                           (pos-eol)
                           (list 'denote-tree--child (car node-children)))))
  (let ((prev (car (last node-children)))
        (tail node-children))
    (dolist (el node-children)
      (setq tail (cdr tail))
      ;; if tail is null, then we are at last element,
      ;; fetch start of child nodes
      (let ((next (if (null (car tail)) (car node-children) (car tail))))
        (save-excursion
          (goto-char el)
          (add-text-properties (pos-bol)
                               (pos-eol)
                               (list 'denote-tree--next next
                                     'denote-tree--prev prev
                                     'denote-tree--parent parent))))
      (setq prev el))))



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


;; Helper functions and macro closure

(defun denote-tree--clean-up ()
  "Clean up buffers created during the tree walk."
  (dolist (el denote-tree--visited-buffers)
    (kill-buffer el))
  (setq denote-tree--visited-buffers nil)
  (setq denote-tree--cyclic-buffers nil))

(defun denote-tree--default-props (str)
  "Default function returning STR with properties."
  (propertize str))

(defmacro denote-tree--movement-generator (prop)
  "Generate defuns that move the point to PROP."
  (declare (indent 1))
  `(defun ,(intern (format "denote-tree-%s-node" prop)) (&optional arg)
     ,(concat "Move the point to the " (symbol-name prop) " node of a current node ARG times.
If ARG is omitted or nil, move to the " (symbol-name prop) " of a current node.")
     (interactive "p")
     (or arg (setq arg 1))
     (dotimes (el arg)
       (when-let ((next-point
                   (get-text-property (point)
                                      ',(intern (format "denote-tree--%s"
                                                        prop)))))
         (goto-char next-point)))))

(provide 'denote-tree)
;;; denote-tree.el ends here
