;;; denote-tree.el --- Visualize your notes as a tree -*- lexical-binding: t -*-

;; Copyright 2024, Sararin
;; Created: 2024-09-15 Sun
;; Version: 0.5.0
;; Keywords: convenience
;; URL: http://github.com/sarcom-sar/denote-tree.el
;; Package-Requires: ((emacs "25.1"))

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

(defgroup denote-tree-faces ()
  "Faces for `denote-tree'."
  :group 'faces)

(defface denote-tree-node '((t :inherit link))
  "Default face used for nodes."
  :group 'denote-tree-faces)

(defface denote-tree-circular-node '((t :inherit link-visited))
  "Default face used for circular nodes."
  :group 'denote-tree-faces)

(defgroup denote-tree ()
  "Visualise your notes as a tree."
  :group 'convenience)

(defcustom denote-tree-buffer-name "*denote-tree*"
  "Name of the buffer `denote-tree' will be built in."
  :group 'denote-tree
  :type 'string)

(defcustom denote-tree-title-colorize-function #'denote-tree--default-props
  "Function accepting one argument STR.
Returns propertied string STR."
  :group 'denote-tree
  :type 'function)

(defcustom denote-tree-max-traversal-depth t
  "Maximum traversal depth of `denote-tree'.
If t traverse all the way, if num, traverse n nodes deep."
  :group 'denote-tree
  :type '(choice symbol natnum))

(defcustom denote-tree-include-from-front-matter '(title)
  "Elements of front matter to include, when drawing a node."
  :group 'denote-tree
  :type '(set (choice (const title)
                      (const identifier)
                      (const keywords))))


;; Vars and consts

(defconst denote-tree-lower-knee "'-")
(defconst denote-tree-tee "+-")
(defconst denote-tree-space "  ")
(defconst denote-tree-pipe "| ")
(defconst denote-tree-node "* ")

(defvar denote-tree--visited-buffers '()
  "List of already created buffers.  Used for clean up.")

(defvar denote-tree--cyclic-buffers '()
  "List of buffers that are cyclic nodes.

`car' of the element of `denote-tree--cyclic-buffers' is denote ID
that appears cyclically over the buffer.  `cdr' of that variable is
set to the list of positions at which that denote ID is present.")


;; Mode and interactive functions

(defvar denote-tree-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n" #'denote-tree-next-node)
    (define-key map "p" #'denote-tree-prev-node)
    (define-key map "f" #'denote-tree-child-node)
    (define-key map "b" #'denote-tree-parent-node)
    (define-key map "g" #'denote-tree-redraw)
    map)
  "Keymap for `denote-tree-mode'.")

(define-derived-mode denote-tree-mode special-mode "denote-tree"
  "Major mode for output from \\[denote-tree].
Move point to one of the nodes in the buffer, then press a button to
open it.
\\{denote-tree-mode-map\}"
  :interactive nil)

;;;###autoload
(defun denote-tree (&optional buffer)
  "Draw hierarchy between denote files as a tree.

The function uses either the current buffer, if called from a function
or a BUFFER provided by the user."
  (interactive)
  (unwind-protect
      (progn
        (or buffer (setq buffer (denote-tree--collect-keywords (current-buffer)
                                                               '(identifier))))
        (denote-tree--open-link-maybe buffer)
        (let ((inhibit-read-only t))
          (with-current-buffer (get-buffer-create denote-tree-buffer-name)
            (erase-buffer)
            (denote-tree-mode)
            (denote-tree--draw-tree buffer)))
        (pop-to-buffer denote-tree-buffer-name)
        (goto-char (1+ (length denote-tree-lower-knee))))
    (denote-tree--clean-up)))

(defun denote-tree-enter-node (&optional button)
  "Enter node at point in other window.
BUTTON is pased as node's ID."
  (interactive)
  (when button
    (find-file-other-window
     (denote-get-path-by-id button))))

(defun denote-tree-redraw (&optional arg)
  "Redraw the entire tree.
With universal argument ARG, redraw from node at point."
  (interactive "P")
  (unless (equal arg '(4))
    (goto-char (1+ (length denote-tree-node))))
  (when-let ((current-node (get-text-property (point) 'button-data)))
    (denote-tree current-node)))

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

(denote-tree--movement-generator child)
(denote-tree--movement-generator parent)
(denote-tree--movement-generator next)
(denote-tree--movement-generator prev)


;; Tree traversal
;; it is a good idea to merge those functions

(defun denote-tree--draw-tree (buffer)
  "Draw and propertize a tree in current buffer starting with BUFFER."
  (denote-tree--walk-links buffer nil "" t denote-tree-max-traversal-depth)
  (denote-tree--add-props-to-cycles))

(defun denote-tree--walk-links (buffer parent indent lastp depth)
  "Walk along the links starting from BUFFER.

Draw the current buffer as a node in `denote-tree-buffer-name'.  Set it's
properties. Collect all the links and call `denote-tree--walk-links' on
them recursively.  If one of the buffers was already visited do not iterate
over it.

Argument PARENT - parent of current node.
Argument INDENT - state of INDENT between traversals.
Argument LASTP  - is the node the last child of parent node?
Argument DEPTH  - maximum depth of the traversal."
  ;; draw node in buffer,
  ;; extract position of point at node
  ;; carry over the indent
  (let* ((links-in-buffer (denote-tree--collect-links buffer))
         (pos-and-indent (denote-tree--draw-node buffer indent lastp))
         (pos (car pos-and-indent))
         (indent (cdr pos-and-indent))
         (cyclical-node (assoc buffer denote-tree--cyclic-buffers #'string=))
         (depth (if (symbolp depth) depth (if (= (1- depth) 0) nil (1- depth))))
         node-children)
    ;; traverse the buffer structure
    ;; if current buffer is in denote-tree--cyclic-buffers
    ;; do not go deeper, because you enter a cycle
    (cond
     (cyclical-node
      (setcdr cyclical-node (append (cdr cyclical-node) (list pos))))
     (t
      (dolist (el links-in-buffer)
        (when (get-buffer el)
          (add-to-list 'denote-tree--cyclic-buffers
                       (list el)
                       nil
                       (lambda (a b) (string= (car a) (car b)))))
        (when depth
          (setq lastp (eq el (car (last links-in-buffer))))
          (push (denote-tree--walk-links el buffer indent lastp depth)
                node-children)))))
    ;; add props to current node and it's children
    (denote-tree--set-button pos buffer)
    (denote-tree--add-props-to-children (nreverse node-children) pos)
    pos))

(defun denote-tree--add-props-to-cycles ()
  "Add denote-tree--child prop to elements of `denote-tree--cyclic-buffers'.

Find first element with button-data set to the car of
`denote-tree--cyclic-buffers' (since DFS is in effect, the first found match
is guaranteed to be the most expanded one), then save it's position and set
that position as denote-tree--child of all the cyclic nodes."
  (let (child-prop)
    (dolist (el denote-tree--cyclic-buffers)
      (goto-char (point-min))
      (text-property-search-forward 'button-data (car el))
      (setq child-prop (get-text-property (point) 'denote-tree--child))
      (dolist (le (cdr el))
        (goto-char le)
        (add-text-properties (line-beginning-position)
                             (line-end-position)
                             (list 'denote-tree--child child-prop))))))

(defun denote-tree--draw-node (node-name indent lastp)
  "Draw NODE-NAME according to INDENT in current buffer.

Insert the current line as follows INDENT `denote-tree-node' title of
the current denote note.  Face of `denote-tree-node' is either
`denote-tree-circular-node-face' if current NODE-NAME is a member of
`denote-tree--cyclic-buffers' or `denote-tree-node-face' if it's not.
Call `denote-tree-title-colorize-function' on title.

Return location of a point where the node starts and the current indent.
Argument LASTP is the current node last child of parent."
  (let ((circularp (assoc node-name denote-tree--cyclic-buffers))
        (keywords denote-tree-include-from-front-matter)
        point-star-loc)
    (insert indent)
    (cond
     (lastp
      (setq indent (concat indent denote-tree-space))
      (insert denote-tree-lower-knee))
     (t
      (setq indent (concat indent denote-tree-pipe))
      (insert denote-tree-tee)))
    (setq point-star-loc (point))
    (insert (propertize denote-tree-node
                        'face (if circularp
                                  'denote-tree-circular-node
                                'denote-tree-node))
            (funcall denote-tree-title-colorize-function
                     (denote-tree--collect-keywords node-name keywords))
            "\n")
    (cons point-star-loc indent)))

(defun denote-tree--set-button (position buffer)
  "Add button to visit BUFFER at POSITION."
  (make-text-button position
                    (+ position (length denote-tree-node))
                    'action #'denote-tree-enter-node
                    'button-data buffer))

(defun denote-tree--add-props-to-children (node-children parent)
  "Iterate over NODE-CHILDREN to set node's props. Keep node's PARENT.

Every node contains props denote-tree--next, denote-tree--prev and
denote-tree--parent which contain point's position to go to get to
previous/next sibling node or a parent."
  (when node-children
    (save-excursion
      (goto-char parent)
      (add-text-properties (line-beginning-position)
                           (line-end-position)
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
          (add-text-properties (line-beginning-position)
                               (line-end-position)
                               (list 'denote-tree--next next
                                     'denote-tree--prev prev
                                     'denote-tree--parent parent))))
      (setq prev el))))



;; Helpers for Links and Buffers

(defun denote-tree--collect-links (buffer)
  "Collect all denote style identifiers in BUFFER.
Return as a list sans BUFFER own identifiers."
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

(defun denote-tree--collect-keywords (buffer keywords)
  "Return denote KEYWORDS from BUFFER.
Return nil if none is found."
  (let ((filetype (denote-tree--find-filetype buffer))
        lst)
  (when filetype
    (with-current-buffer buffer
      (dolist (el keywords)
        (goto-char (point-min))
        (when (cond
               ((eq el 'title)
                (re-search-forward (plist-get filetype :title-key-regexp)
                                   nil t))
               ((eq el 'identifier)
                (re-search-forward denote-id-regexp
                                   nil t)
                (backward-word-strictly))
               ((eq el 'keywords)
                (re-search-forward (plist-get filetype :keywords-key-regexp)
                                   nil t))
               (t nil))
          (push (denote-trim-whitespace
                 (buffer-substring-no-properties (point) (line-end-position)))
                lst)
          (push " " lst)))))
  (apply #'concat (nreverse (cdr lst)))))

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
  "Return ELEMENT buffer, create if necessary.
Add ELEMENT to `denote-tree--visited-buffers' to delete it after
`denote-tree' initialization."
    (unless (get-buffer element)
      (add-to-list 'denote-tree--visited-buffers element)
      (with-current-buffer (get-buffer-create element)
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

(provide 'denote-tree)
;;; denote-tree.el ends here
