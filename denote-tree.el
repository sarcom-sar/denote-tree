;;; denote-tree.el --- Visualize your notes as a tree -*- lexical-binding: t -*-

;; Copyright 2024, Sararin
;; Created: 2024-09-15 Sun
;; Version: 0.8.6
;; Keywords: convenience
;; URL: http://github.com/sarcom-sar/denote-tree.el
;; Package-Requires: ((emacs "28.1") (denote "3.0.1"))

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
;; Parts of front matter to include are customizable via
;; `denote-tree-node-description'.  It is able to handle cyclical nodes and
;; provides a mechanism to move between those cyclical nodes (called
;; "teleportations") by default (customizable via
;; `denote-tree-preserve-teleports-p').
;;
;; User can customize `denote-tree-node' and `denote-tree-circular-node' to make
;; them more visible.  With a bit of hacking it is also feasible to implement
;; colored node titles via `denote-tree-node-colorize-function'.
;;
;; For performance reasons `denote-tree-max-traversal-depth' can be reduced.
;;

;;; Code:

(require 'denote)
(require 'seq)
(require 'let-alist)
(require 'cl-lib)

(declare-function #'denote-tree-edit-mode "./denote-tree-edit.el")


;;;; Faces and Custom

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

(defcustom denote-tree-buffer-prefix "denote-tree"
  "Prefix of the buffer `denote-tree' will be built in.

Every `denote-tree' buffer has a unique name made from this prefix and
root node of it's tree."
  :group 'denote-tree
  :type 'string)

(defcustom denote-tree-node-colorize-function #'denote-tree--default-props
  "Add properties to information from the node according to type.

Function accepts two arguments STR and TYPE.  Choosen string from
front-matter is propertized according to type from
`denote-tree-node-description'."
  :group 'denote-tree
  :type 'function)

(defcustom denote-tree-max-traversal-depth t
  "Maximum traversal depth of `denote-tree'.
If t traverse all the way, if num, traverse num nodes deep."
  :group 'denote-tree
  :type '(choice symbol natnum))

(defcustom denote-tree-node-description '(title)
  "Elements of front matter to include in node's description.

User can also extend denote's front matter by any arbitrary element, but
they have to add corresponding regex and file type to
`denote-tree-extend-filetype-with' for `denote-tree' to recognize it.
That user variable also supports arbitrary strings.

Denote's default front matter elements:
- title
- identifier
- keywords
- signature
- date
- symbol
- string"
  :group 'denote-tree
  :type '(set (choice (const title)
                      (const identifier)
                      (const keywords)
                      (const signature)
                      (const date)
                      symbol
                      string)))

(defcustom denote-tree-preserve-teleports-p t
  "Teleport back when accessing cyclical node from it's child.
When nil, always move to \"real\" parent of a node."
  :group 'denote-tree
  :type 'boolean)

(defcustom denote-tree-fancy-edit nil
  "If t, use fancy editing with widgets (experimental).
If nil fall back to the thin `denote-rename-file' wrapper."
  :group 'denote-tree
  :type 'boolean)

(defcustom denote-tree-extend-filetype-with
  '((:identifier-key-regexp
     org "^#\\+identifier\\s-*:"
     markdown-yaml "^identifier\\s-*:"
     markdown-toml "^identifier\\s-*="
     text "^identifier\\s-*:")
    (:signature-key-regexp
     org "^#\\+signature\\s-*:"
     markdown-yaml "^signature\\s-*:"
     markdown-toml "^signature\\s-*="
     text "^signature\\s-*:")
    (:date-key-regexp
     org "^#\\+date\\s-*:"
     markdown-yaml "^date\\s-*:"
     markdown-toml "^date\\s-*="
     text "^date\\s-*:"))
  "Alist of keys where values are plists of filetype regex value.
User can extend it in format of (KEY TYPE VALUE)."
  :group 'denote-tree
  :type
  '(alist
    :key-type symbol
    :value-type (plist :key-type symbol :value-type string)))


;;;; Vars and consts

(defconst denote-tree-lower-knee "'-")
(defconst denote-tree-tee "+-")
(defconst denote-tree-space "  ")
(defconst denote-tree-pipe "| ")
(defconst denote-tree-node "* ")

(defvar-local denote-tree--visited-buffers '()
  "List of already created buffers.  Used for clean up.")

(defvar-local denote-tree--tree-alist '()
  "Alist of all the nodes in the buffer.")

(defvar denote-tree--extended-filetype nil
  "Full filetype alist.")

(defvar-local denote-tree--teleport-stack '()
  "Stack of point positions denoting WHERE-TO jump FROM-WHERE.
FROM-WHERE is a positions of first child node.  WHERE-TO is a point
position of cyclical parent node.")

(defvar-local denote-tree--buffer-name ""
  "Actual name of the buffer with denote-tree's tree.")


;;;; Mode and interactive functions

(defvar denote-tree-mode-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "n" #'denote-tree-next-node)
    (keymap-set map "p" #'denote-tree-prev-node)
    (keymap-set map "f" #'denote-tree-child-node)
    (keymap-set map "b" #'denote-tree-parent-node)
    (keymap-set map "g" #'denote-tree-redraw)
    (keymap-set map "e" #'denote-tree-edit-node)
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

The function uses either the current buffer, if called interactively or
a BUFFER provided by the user."
  (interactive)
  (let (buffer-name)
    (unwind-protect
        (progn
          (setq denote-tree--extended-filetype
                (denote-tree--build-extended-filetype
                 denote-file-types denote-tree-extend-filetype-with))
          (setq buffer
                (denote-retrieve-filename-identifier-with-error
                 (or (and (bufferp buffer) (buffer-file-name buffer))
                     buffer
                     (buffer-file-name))))
          (setq buffer-name
                (concat "*" denote-tree-buffer-prefix " " buffer "*"))
          (let ((inhibit-read-only t))
            (with-current-buffer (get-buffer-create buffer-name)
              (erase-buffer)
              (denote-tree-mode)
              (denote-tree--open-link-maybe buffer)
              (denote-tree--draw-tree buffer)
              (setq denote-tree--buffer-name buffer-name)))
          (pop-to-buffer buffer-name)
          (goto-char (1+ (length denote-tree-lower-knee))))
      (denote-tree--clean-up))))

(defun denote-tree-enter-node (&optional button)
  "Enter node at point in other window.
BUTTON is pased as node's ID."
  (interactive)
  (when button
    (find-file-other-window
     ;; no need to file-check, since if it's drawn
     ;; then it's good to show
     (denote-get-path-by-id button))))

(defun denote-tree-redraw (&optional arg)
  "Redraw ARG parts of a tree.

Without \\[universal-argument], redraw the current node deepening it.
With \\[universal-argument] draw current node in a new window.
With \\[universal-argument] \\[universal-argument], redraw the entire tree."
  (interactive "P")
  (cond
   ((equal arg '(4))
    (denote-tree (denote-tree--get-prop 'button-data)))
   ((or (equal arg '(16)) (not (null arg)))
    (denote-tree (denote-tree--get-prop 'button-data 1)))
   (t
    (seq-let (pos alist) (denote-tree--deepen-traversal denote-tree--tree-alist)
      (setq denote-tree--tree-alist alist)
      (goto-char pos)))))

(defun denote-tree-child-node (&optional arg)
  "Move the point to the child of a node ARG times.

If ARG is negative move to the parent of a node ARG times.  If ARG is
ommited, nil or zero, move once.  With \\[universal-argument] reverse
`denote-tree-preserve-teleports-p' one time.

If `denote-tree-preserve-teleports-p' is set to t, preserve the parent
node position for future backtracking."
  (interactive "P")
  (or arg (setq arg 1))
  (let ((preserve-teleport-p denote-tree-preserve-teleports-p)
        (alist denote-tree--tree-alist)
        next-point curr-point)
    (cond
     ((listp arg)
      (setq preserve-teleport-p (not preserve-teleport-p))
      (setq arg 1))
     ((eq arg '-)
      (setq arg -1)))
    (if (< arg 0)
        (denote-tree-parent-node (- arg))
      (dotimes (_ arg next-point)
        (let ((node-id (get-text-property (point) 'denote-tree--identifier)))
          (setq next-point (denote-tree--nested-value
                            alist node-id :children :pos))
          (setq curr-point (denote-tree--get-node-pos))
          (when (and preserve-teleport-p
                     next-point
                     (not (eq node-id (denote-tree--nested-value
                                       alist node-id :true-name))))
            (push (list (set-marker (make-marker) curr-point) next-point)
                  denote-tree--teleport-stack))
          (when next-point
            (goto-char next-point)))))))

(defun denote-tree-parent-node (&optional arg)
  "Move the point to the parent of a node ARG times.

If ARG is negative move to the child of a node ARG times.  If ARG is
ommited, nil or zero, move once.

If `denote-tree-preserve-teleports-p' is set to t, teleport to the
parent the point came from."
  (interactive "p")
  (or arg (setq arg 1))
  (if (< arg 0)
      (denote-tree-child-node (- arg))
    (let ((alist denote-tree--tree-alist)
          next-point canon-point current-teleport)
      (dotimes (_ arg next-point)
        (let ((node-id (get-text-property (point) 'denote-tree--identifier)))
          (setq next-point (denote-tree--nested-value
                            alist node-id :parent :pos))
          (setq canon-point
                (denote-tree--nested-value
                 alist
                 (get-text-property next-point 'denote-tree--identifier)
                 :children :pos))
          (when-let* ((current-teleport (car denote-tree--teleport-stack))
                      ((equal canon-point (cadr current-teleport))))
            (setq next-point (marker-position (car current-teleport)))
            (set-marker (car current-teleport) nil)
            (pop denote-tree--teleport-stack))
          (goto-char next-point))))))

(defun denote-tree-next-node (&optional arg)
  "Move the point to the next sibling node ARG times.

If ARG is negative move to the prev sibling node ARG times.  If ARG is
omitted, nil or zero, move once."
  (interactive "p")
  (or arg (setq arg 1))
  (let ((direction (if (<= arg 0) :prev :next))
        (arg (abs arg))
        next-point)
    (dotimes (_ arg next-point)
      (let ((node-id (get-text-property (point) 'denote-tree--identifier)))
        (setq next-point
              (denote-tree--nested-value
               denote-tree--tree-alist node-id direction :pos))
        (goto-char next-point)))))

(defun denote-tree-prev-node (&optional arg)
  "Move the point to the prev sibling node ARG times.

If ARG is negative move to the nextv sibling node ARG times.  If ARG is
omitted, nil or zero, move once."
  (interactive "p")
  (or arg (setq arg 1))
  (denote-tree-next-node (- arg)))

(defun denote-tree-edit-node ()
  "Edit node's front matter.

What is editable is dependent on `denote-prompts'.  If
`denote-tree-edit-mode' is loaded and `denote-tree-fancy-edit' is set to
t, use it's UI."
  (interactive)
  (if denote-tree-fancy-edit
      (progn
        (require 'denote-tree-edit)
        (denote-tree-edit-mode))
    (let* ((identifier (denote-tree--get-prop 'button-data))
           (buffer (denote-tree--edit-node (denote-get-path-by-id identifier))))
      (save-excursion
        (goto-char (point-min))
        ;; edit all occurences of that buffer
        (while (text-property-search-forward
                'button-data identifier t)
          (setq denote-tree--tree-alist
                (denote-tree--redraw-node
                 buffer denote-tree--tree-alist (point))))))))


;;;; Utilities for node editing

(defun denote-tree--edit-node (buffer)
  "Call `denote-rename-file' interactively to edit BUFFER.

Return current buffer object."
  (let ((denote-save-buffers t))
    (with-current-buffer (find-file-noselect buffer)
      (call-interactively #'denote-rename-file)
      (current-buffer))))

(defun denote-tree--redraw-node (buffer alist pos)
  "Redraw node based on BUFFER's front matter at POS.

Include only elements from `denote-tree-node-description'.  Preserve
properties."
  (let ((inhibit-read-only t)
        (node-name (get-text-property pos 'denote-tree--identifier))
        (new-alist alist)
        (new-descp (denote-tree--collect-keywords-as-string
                    buffer denote-tree-node-description)))
    (delete-region pos (line-end-position))
    (insert new-descp)
    (setf (alist-get node-name new-alist)
          (plist-put (alist-get node-name new-alist) :descp new-descp))
    new-alist))


;;;; Tree traversal

(defun denote-tree--draw-tree (buffer)
  "Draw and propertize a tree in current buffer starting with BUFFER."
  (let ((progress (make-progress-reporter "Building denote-tree buffer...")))
    (setq denote-tree--tree-alist
          (denote-tree--fix-children-in-alist
           (denote-tree--walk-links-iteratively
            buffer "" t denote-tree-max-traversal-depth)))
    (denote-tree--draw-node-list denote-tree--tree-alist (intern buffer))
    (progress-reporter-done progress)))

(defun denote-tree--traverse-structure
    (element alist info stack call-fn &optional other-fn)
  "Traverse the structure calling CALL-FN or OTHER-FN, return the ALIST.

CALL-FN and OTHER-FN are user supplied functions with 4 arguments which
are in charge of maintaining the stack.  ELEMENT is a current element under
traversal.  ALIST stores the general information, INFO should store the
specific information, while STACK maintains the elements to traverse further.

If CALL-FN returns nil, OTHER-FN is called instead.  These functions should
return a list of four elements each."
  (let ((new-alist (copy-sequence alist)))
    (while element
      (seq-setq (element new-alist info stack)
                (or (funcall call-fn element new-alist info stack)
                    (funcall other-fn new-alist info stack))))
    new-alist))

(defun denote-tree--walk-links-iteratively
    (buffer indent lastp depth &optional parent)
  "Walk links from BUFFER with starting INDENT."
  (let ((node (intern buffer)))
    (denote-tree--traverse-structure
     node
     (list
      (denote-tree--node-plist
       (cons node node) node node parent indent lastp depth))
     nil
     (list node)
     #'denote-tree--grow-alist-and-stack
     (lambda (alist info stack)
       (list (cadr stack)
             alist
             nil
             (cdr stack))))))

(defun denote-tree--fix-children-in-alist (alist)
  "Copy :children of true node to the same prop of duplicate node in ALIST."
  (let (new-alist)
    (seq-do
     (lambda (x)
       (push
        (let* ((true-name (plist-get (cdr x) :true-name))
               (children (denote-tree--nested-value alist true-name :children)))
          (if (not (eq (car x) true-name))
              (append (list (car x)) (plist-put (cdr x) :children children))
            x))
        new-alist))
     alist)))

(defun denote-tree--draw-node-list (alist initial-node)
  "Draw every node in ALIST starting from INITIAL-NODE."
  (denote-tree--traverse-structure
   initial-node alist (alist-get initial-node alist) (list initial-node)
   #'denote-tree--draw-node-list-helper
   (lambda (alist info stack)
     (list (cadr stack)
           alist
           (alist-get (cadr stack) alist)
           (cdr stack)))))

(defun denote-tree--draw-node-list-helper (node alist node-plist stack)
  "Set the current NODE in NODE-PLIST and advance the STACK.

This function calls `denote-tree--draw-node-foo' to do an actual drawing.
Besides delegating the drawing part it also advances the stack
and sets up everything for next iteration."
  (let ((point (denote-tree--draw-node-foo
                node node-plist (denote-tree--nested-value
                                 alist node :parent :next-indent)))
        (copy-stack (copy-sequence stack)))
    (plist-put node-plist :pos point)
    (when (eq node (denote-tree--nested-value alist node :true-name))
      (setq copy-stack (append (plist-get node-plist :children) (cdr copy-stack)))
      (list (car copy-stack)
            alist
            (alist-get (car copy-stack) alist)
            copy-stack))))

(defun denote-tree--draw-node-foo (node plist next-indent)
  "Draw NODE with NEXT-INDENT according to PLIST."
  (let ((point 0))
    (insert
     (or next-indent
         "")
     (if (plist-get plist :last)
         denote-tree-lower-knee
       denote-tree-tee))
    (setq point (point-marker))
    (insert
     (if (eq node (plist-get plist :true-name))
         (propertize denote-tree-node 'face 'denote-tree-node)
       (propertize denote-tree-node 'face 'denote-tree-circular-node))
     (plist-get plist :descp))
    (put-text-property
     (line-beginning-position) (line-end-position)
     'denote-tree--identifier node)
    (denote-tree--set-button point
                  (symbol-name (plist-get plist :true-name)))
    (insert "\n")
    point))

(defun denote-tree--grow-alist-and-stack (node alist info stack)
  "Add NODE to ALIST, fetch more nodes for STACK."
  (when-let* (((eq node (denote-tree--nested-value alist node :true-name)))
              (depth (denote-tree--nested-value alist node :depth)))
    (let* ((node (denote-tree--open-link-maybe (symbol-name node)))
           (indent (denote-tree--nested-value alist node :next-indent))
           (new-depth (cond
                       ((symbolp depth) depth)
                       ((and (numberp depth) (< 0 (1- depth))) (1- depth))
                       ((and (numberp depth) (= 0 (1- depth))) nil)
                       (t t)))
           (uniq-links-in-node
            (mapcar (lambda (x)
                      (denote-tree--unique-nodes x (alist-get x alist)))
                    (save-excursion
                      (denote-tree--collect-links (symbol-name node)))))
           (last-children-node (caar (last uniq-links-in-node)))
           (keys (mapcar #'car uniq-links-in-node))
           (new-alist
            (append
             (mapcar (lambda (x)
                       (denote-tree--node-plist
                        x
                        (denote-tree--next-sibling (car x) keys)
                        (denote-tree--next-sibling (car x) (reverse keys))
                        node
                        indent
                        (eq (car x) last-children-node)
                        new-depth))
                     uniq-links-in-node)
             alist))
           (new-stack (append keys (cdr stack))))
      (setf (alist-get node alist)
            (plist-put (alist-get node alist) :children keys))
      (list (car new-stack) new-alist info new-stack))))

(defun denote-tree--unique-nodes (node existsp)
  "Return a pair new id of NODE and NODE symbol itself.

If EXISTSP, return an unique identifier."
  (cons (if existsp (gensym node) node)
        node))

(defun denote-tree--node-plist (x &optional next prev parent indent lastp depth)
  "Build full plist for X.

  Argument NEXT - next sibling
  Argument PREV - previous sibling
Argument PARENT - parent node
Argument INDENT - next indent
 Argument LASTP - is the node last node."
  (let* ((node (car x))
         (true-node (cdr x))
         (indent (denote-tree--calculate-indent indent lastp)))
    (denote-tree--open-link-maybe (symbol-name true-node))
    (list
     node
     :next-indent indent
     :true-name true-node
     :next next
     :prev prev
     :descp (denote-tree--collect-keywords-as-string
             (symbol-name true-node) denote-tree-node-description)
     :children nil
     :parent parent
     :last lastp
     :depth depth)))

(defun denote-tree--next-sibling (x siblings)
  "Return the :next SIBLING of X."
  (let ((next (copy-tree siblings)))
    (setcdr (last next) next)
    (cadr (memq x next))))

(defun denote-tree--nested-value (alist initial-key &rest nested-value)
  "Iteratively return NESTED-VALUE of INITIAL-KEY in ALIST."
  (let* ((prop (car nested-value))
         (value (plist-get (alist-get initial-key alist) prop)))
    (dolist (trio (cdr nested-value) value)
      (seq-find
       (lambda (x) (setq value (plist-get (alist-get x alist) trio)))
       (if (listp value) value (list value))))))

(defun denote-tree--calculate-indent (indent lastp)
  (concat indent (if lastp denote-tree-space denote-tree-pipe)))

(defun denote-tree--set-button (position buffer)
  "Add button to visit BUFFER at POSITION."
  (make-text-button position (+ position (length denote-tree-node))
                    'action #'denote-tree-enter-node
                    'button-data buffer))

(defun denote-tree--walk-region (func)
  "Step through every line of region and apply FUNC to it.

Return a payload."
  (let ((payload '()))
    (while (< (point) (point-max))
      (setq payload (append (list (funcall func)) payload))
      (forward-line))
    payload))

(defun denote-tree--deepen-traversal (alist)
  "Retraverse current node under point with ALIST.

Especially useful, if `denote-tree-max-traversal-depth' is set to very
low value."
  (let* ((inhibit-read-only t)
         (current-pos (point))
         (current-node (get-text-property (point) 'denote-tree--identifier))
         (args-for-walking (denote-tree--args-for-walking current-node alist))
         (new-alist alist))
    ;; trying to redraw from cyclical node, wth?
    (when (car args-for-walking)
      (unwind-protect
          (setq new-alist
                (denote-tree--fix-children-in-alist
                 (apply #'denote-tree--walk-links-iteratively
                        args-for-walking)))
        (save-restriction
          (apply #'narrow-to-region
                 (denote-tree--determine-node-bounds
                  current-node alist))
          (setq new-alist (denote-tree--unite-alists new-alist alist))
          (delete-region (point-min) (point-max))
          (goto-char (point-min))
          (denote-tree--draw-node-list new-alist current-node)
          (delete-region (1- (point-max)) (point-max)))
        (denote-tree--clean-up)))
    (list current-pos new-alist)))

(defun denote-tree--args-for-walking (node alist)
  "Return NODE information from ALIST.

To be more specific, the function returns a list of:

- true-name;
- indent;
- last;
- traversal-depth;
- parent."
  (let ((name (if (eq node (denote-tree--nested-value alist node :true-name))
                  (symbol-name node)
                nil)))
    (list name
          (buffer-substring-no-properties
           (line-beginning-position)
           (- (denote-tree--get-node-pos) (length denote-tree-node)))
          (denote-tree--nested-value alist node :last)
          denote-tree-max-traversal-depth
          (denote-tree--nested-value alist node :parent))))

(defun denote-tree--unite-alists (new-alist old-alist)
  "Return modified OLD-ALIST with elements from NEW-ALIST."
  (let* ((nodes-in-region
          (denote-tree--walk-region
           (lambda ()
             (get-text-property
              (point) 'denote-tree--identifier))))
         (alist-from-region
          (seq-reduce
           (lambda (payload el)
             (setq payload (append (assq el old-alist) payload)))
           nodes-in-region
           '()))
         (alist-sans-region
          (seq-difference old-alist alist-from-region)))
    (seq-union new-alist old-alist)))

(defun denote-tree--link-next-and-prev-node (pos)
  "Nodes in vicinity of node at POS point at nearest neighbor.

If node points at node at POS with \\='denote-tree--child prop set
marker to nil."
  ;; (when-let* ((next (get-text-property pos 'denote-tree--next))
  ;;             (next-prev (get-text-property next 'denote-tree--prev))
  ;;             (prev (get-text-property pos 'denote-tree--prev))
  ;;             (prev-next (get-text-property prev 'denote-tree--next))
  ;;             (parent (get-text-property pos 'denote-tree--parent))
  ;;             (parent-child (get-text-property parent 'denote-tree--child)))
  ;;   ;; handle neighbors
  ;;   (cond
  ;;    ;; there are /at max/ two nodes
  ;;    ((equal next prev)
  ;;     (set-marker next-prev next)
  ;;     (set-marker prev-next prev))
  ;;    ;; there is more than two nodes
  ;;    (t
  ;;     (set-marker next-prev prev)
  ;;     (set-marker prev-next next)))
  ;;   ;; handle parent
  ;;   (let ((next-next (get-text-property next 'denote-tree--next)))
  ;;     (cond
  ;;      ((and (= next-next pos)
  ;;            (= parent-child pos))
  ;;       (set-marker parent-child nil))
  ;;      ((= parent-child pos)
  ;;       (set-marker parent-child next)))))
  )

(defun denote-tree--determine-node-bounds (node alist)
  "Return bounds of current NODE with ALIST as a cons.

If :next node doesn't exist, the situation is trivial.  If it is further
along the buffer than NODE, then just jump to it and return EoL of
previous line.  If NODE and :next node point to the same location and
:next node precedes the NODE-POS, then we can have arbitrary
\"deepness\", iterate until you find parent node which next node is
grater than node to be redrawn.  If you ran out of nodes to check, you
are at the top and the last node is your target.  If nothing matches,
signal an error."
  (list
   (save-excursion
     (goto-char (denote-tree--nested-value alist node :pos))
     (line-beginning-position))
   (let ((now-pos (denote-tree--nested-value alist node :pos))
         (next-pos (denote-tree--nested-value alist node :next :pos))
         (prev-pos (denote-tree--nested-value alist node :prev :pos)))
     (cond
      ((not (denote-tree--nested-value alist node :parent :pos))
       (1- (point-max)))
      ((< now-pos next-pos)
       (save-excursion
         (goto-char next-pos)
         (forward-line -1)
         (line-end-position)))
      ((>= now-pos next-pos)
       (save-excursion
         (goto-char (denote-tree--nested-value alist node :parent :pos))
         (let (next)
           (while (and (setq next (denote-tree--nested-value
                                   alist
                                   (get-text-property
                                    (point) 'denote-tree--identifier)
                                   :next :pos))
                       (> now-pos next))
             (goto-char (denote-tree--nested-value alist
                                        (get-text-property
                                         (point) 'denote-tree--identifier)
                                        :parent :pos)))
           (if (> now-pos (or next 1))
               (1- (point-max))
             (goto-char (denote-tree--nested-value
                         alist
                         (get-text-property
                          (point) 'denote-tree--identifier)
                         :next :pos))
             (forward-line -1)
             (line-end-position)))))
      (t (error "Denote tree buffer %s is malformed" (buffer-name)))))))

(defmacro denote-tree--build-marker-alist (pos)
  "Return alist of KEY MARKER NEXT-PROP at POS.

The alist is made out of identifier of a marker, the marker itself and
the opposite identifier.  It's used when referencing the node under the
marker in order to set it's opposite to the current node."
  ;; `(list
  ;;   (list 'denote-tree--prev
  ;;         (copy-marker
  ;;          (get-text-property ,pos 'denote-tree--prev))
  ;;         'denote-tree--next)
  ;;   (list 'denote-tree--next
  ;;         (copy-marker
  ;;          (get-text-property ,pos 'denote-tree--next))
  ;;         'denote-tree--prev)
  ;;   (list 'denote-tree--parent
  ;;         (copy-marker
  ;;          (get-text-property ,pos 'denote-tree--parent))
  ;;         'denote-tree--child))
  )


;;;; Helpers for Links and Buffers

(defun denote-tree--collect-links (buffer)
  "Collect all denote style identifiers in BUFFER.
Return as a list sans BUFFER's own identifier."
  (let ((buffer-id
         (or (denote-retrieve-filename-identifier buffer)
             (denote-tree--collect-keywords-as-string buffer '(identifier))))
        found-ids)
    (with-current-buffer buffer
      (goto-char (point-min))
      (while (search-forward-regexp denote-id-regexp nil t)
        (push (intern (concat (match-string-no-properties 1)
                              (match-string-no-properties 2)))
              found-ids))
      (delete (intern buffer-id) (nreverse found-ids)))))

(defun denote-tree--build-extended-filetype (gen-from add-this)
  "Add keys and values from ADD-THIS to GEN-FROM alist."
  (let ((ext-filetype (copy-tree gen-from)))
    (dolist (type ext-filetype)
      (mapc
       (lambda (key)
         (unless (plist-member (cdr type) (car key))
           (setf (cdr type)
                 (plist-put
                  (cdr type) (car key) (plist-get (cdr key) (car type))))))
       add-this))
    ext-filetype))

(defun denote-tree--collect-keywords (buffer keywords)
  "Return denote propertized KEYWORDS from BUFFER."
  (when-let* ((filetype (denote-tree--find-filetype buffer))
              (regexps (denote-tree--get-regexps (cdr filetype))))
    (with-current-buffer buffer
      (mapcar (lambda (el)
                (denote-tree--collect-keywords-helper el regexps filetype))
              keywords))))

(defun denote-tree--collect-keywords-helper (el regexps filetype)
  "Turn EL into cons according to REGEXPS and FILETYPE."
  (goto-char (point-min))
  (cond
   ;; if it's a string, just push it
   ((stringp el)
    (cons 'str el))
   ;; if it's in regexps, covert to str and push
   ((re-search-forward (plist-get
                        (cdr filetype)
                        (seq-find
                         (lambda (reg)
                           (denote-tree--extract-and-compare-symbols reg el))
                         regexps))
                       nil t)
    (cons el
          (funcall denote-tree-node-colorize-function
                   (denote-trim-whitespace
                    (buffer-substring-no-properties (point) (line-end-position)))
                   el)))
   ;; symbol with no associated str
   ((symbolp el)
    (list el))))

(defun denote-tree--get-regexps (plist)
  "Return list of all symbols ending in -regexp in PLIST."
  (let (lst)
    (dolist (el plist lst)
      (and (symbolp el)
           (string-suffix-p
            "-regexp" (symbol-name el))
           (stringp (plist-get plist el))
           (push el lst)))))

(defun denote-tree--extract-and-compare-symbols
    (symbol element &optional extractor-regexp)
  "Apply EXTRACTOR-REGEXP to SYMBOL and compare with ELEMENT.

EXTRACTOR-REGEXP should capture one group, which will be transformed
into shortened form.  If EXTRACTOR-REGEXP is nil, then the default value
mangles the SYMBOL like so,

:key-value-regexp      -> key
:foo-bar-regexp        -> foo
:identifier-val-regexp -> identifier"
  (setq extractor-regexp (or extractor-regexp ":\\(.+?\\)-\\(?:.*?\\)regexp"))
  (and (eq (intern
            (replace-regexp-in-string
             extractor-regexp "\\1" (symbol-name symbol)))
           element)
       symbol))

(defun denote-tree--collect-keywords-as-string (buffer keywords)
  "Return KEYWORDS as a joint string from BUFFER."
  (string-join (seq-filter
                #'identity
                (mapcar #'cdr (denote-tree--collect-keywords buffer keywords)))
               " "))

(defun denote-tree--find-filetype (buffer)
  "Guess the filetype in BUFFER and return it as a symbol.

`denote-tree--find-filetype' works refering only to a buffer by finding
any regex from `denote-tree--extended-filetype' that matches in the
front matter.  This can be potentially expensive (worst case scenario is
not finding a match), but guaranteed to work as long the user set the
front-matter."
  (with-current-buffer buffer
    (goto-char (point-min))
    (let ((filetype
           (seq-find
            (lambda (type)
              (let ((types-plist type))
                (seq-find
                 (lambda (el)
                   (save-excursion
                     (re-search-forward (plist-get (cdr types-plist) el)
                                        nil t)))
                 (denote-tree--get-regexps (cdr types-plist)))))
            denote-tree--extended-filetype)))
      (unless filetype
        (warn "%s not a denote-style buffer" buffer))
      filetype)))

(defun denote-tree--open-link-maybe (element)
  "Return ELEMENT buffer, create if necessary.
Add ELEMENT to `denote-tree--visited-buffers' to delete it after
`denote-tree' initialization."
  (unless (get-buffer element)
    (if-let* ((file-path (denote-get-path-by-id element)))
        (progn
          (with-current-buffer (get-buffer-create element)
            (insert-file-contents file-path))
          (unless (member element denote-tree--visited-buffers)
            (push element denote-tree--visited-buffers)))
      (warn "%s was not found" element)
      (setq element "nil")))
  (intern element))


;;;; Helper functions

(defun denote-tree--clean-up ()
  "Clean up buffers created during the tree walk."
  (dolist (el denote-tree--visited-buffers)
    ;; silence all kill-buffer errors
    (condition-case nil
        (kill-buffer el)
      (error nil))))

(defun denote-tree--default-props (str type)
  "Default function returning STR of TYPE with properties.
One props returned has to be denote-tree--type."
  (propertize str 'denote-tree--type type))

(defun denote-tree--get-node-pos (&optional object limit)
  "Get node position in line."
  (next-single-property-change
   (line-beginning-position) 'button-data object limit))

(defun denote-tree--get-prop (prop &optional at-pos)
  "Get PROP at current line or starting from AT-POS.
Return nil if prop not found.

This function will move the point, if AT-POS is a position."
  (and at-pos (goto-char at-pos))
  (condition-case nil
      (get-text-property
       (next-single-property-change
        (line-beginning-position) prop nil (line-end-position))
       prop)
    (error nil)))

(provide 'denote-tree)
;;; denote-tree.el ends here
