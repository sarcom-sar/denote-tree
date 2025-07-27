;;; denote-tree.el --- Visualize your notes as a tree -*- lexical-binding: t -*-

;; Copyright 2024, Sararin
;; Created: 2024-09-15 Sun
;; Version: 0.9.0
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


;;;; Faces and Custom

(defgroup denote-tree-faces ()
  "Faces for `denote-tree'."
  :group 'faces)

(defface denote-tree-node '((t :inherit link))
  "Default face used for nodes.")

(defface denote-tree-circular-node '((t :inherit link-visited))
  "Default face used for circular nodes.")

(defgroup denote-tree ()
  "Visualise your notes as a tree."
  :group 'convenience)

(defcustom denote-tree-buffer-prefix "denote-tree"
  "Prefix of the buffer `denote-tree' will be built in.

Every `denote-tree' buffer has a unique name made from this prefix and
root node of it's tree."
  :type 'string)

(defcustom denote-tree-node-colorize-function #'denote-tree--default-props
  "Add properties to information from the node according to type.

Function accepts two arguments STR and TYPE.  Choosen string from
front-matter is propertized according to type from
`denote-tree-node-description'."
  :type 'function)

(defcustom denote-tree-max-traversal-depth t
  "Maximum traversal depth of `denote-tree'.
If t traverse all the way, if num, traverse num nodes deep."
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
    (keymap-set map "w" #'denote-tree-copy-node-id)
    map)
  "Keymap for `denote-tree-mode'.")

(define-derived-mode denote-tree-mode special-mode "denote-tree"
  "Major mode for output from \\[denote-tree].
Move point to one of the nodes in the buffer, then press a button to
open it.
\\{denote-tree-mode-map\}"
  :interactive nil)

;;;###autoload
(defun denote-tree (buffer)
  "Draw hierarchy between denote files as a tree.

The function allows the user to choose between all file-visiting
buffers.  If called non-interactively, then the BUFFER has to be a valid
denote-style identifier."
  (interactive
   (list
    (buffer-file-name
     (get-buffer
      (read-buffer "Draw buffer: "
                   (if (eq (selected-window) (next-window))
		                   (window-buffer (next-window))
		                 (other-buffer (current-buffer)))
		               t
                   (lambda (x) (buffer-file-name (cdr x))))))))
  (let* ((id (denote-retrieve-filename-identifier-with-error buffer))
         (buffer-name (concat "*" denote-tree-buffer-prefix " " id "*")))
    (unwind-protect
        (progn
          (setq denote-tree--extended-filetype
                (denote-tree--build-extended-filetype
                 denote-file-types denote-tree-extend-filetype-with))
          (let ((inhibit-read-only t))
            (with-current-buffer (get-buffer-create buffer-name)
              (erase-buffer)
              (denote-tree-mode)
              (denote-tree--draw-tree id)
              (setq denote-tree--buffer-name buffer-name)))
          (pop-to-buffer buffer-name)
          (goto-char (1+ (length denote-tree-lower-knee))))
      (denote-tree--clean-up))))

(defun denote-tree-enter-node (&optional button)
  "Enter node at point in other window.
BUTTON is pased as node's ID."
  (interactive)
  (if button
      (find-file-other-window
       ;; no need to file-check, since if it's drawn
       ;; then it's good to show
       (denote-get-path-by-id button))
    (message "No valid node under the point")))

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
      (setq arg (- arg))))
    (if (< arg 0)
        (denote-tree-parent-node (- arg))
      (dotimes (_ arg next-point)
        (let ((node-id (get-text-property (point) :denote-tree--identifier)))
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
ommited, nil or zero, move once."
  (interactive "p")
  (or arg (setq arg 1))
  (if (< arg 0)
      (denote-tree-child-node (- arg))
    (let ((alist denote-tree--tree-alist)
          next-point canon-point)
      (dotimes (_ arg next-point)
        (let ((node-id (get-text-property (point) :denote-tree--identifier)))
          (setq next-point (denote-tree--nested-value
                            alist node-id :parent :pos))
          (setq canon-point
                (denote-tree--nested-value
                 alist
                 (get-text-property next-point :denote-tree--identifier)
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
      (let ((node-id (get-text-property (point) :denote-tree--identifier)))
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

What is editable is dependent on `denote-prompts'."
  (interactive)
  (let* ((identifier (denote-tree--get-prop 'button-data))
         (buffer (denote-tree--edit-node (denote-get-path-by-id identifier))))
    (save-excursion
      (goto-char (point-min))
      ;; edit all occurences of that buffer
      (while (text-property-search-forward
              'button-data identifier t)
        (setq denote-tree--tree-alist
              (denote-tree--redraw-node
               buffer denote-tree--tree-alist))))))

(defun denote-tree-copy-node-id ()
  "Save canonical denote id to the kill ring."
  (interactive)
  (let ((id (denote-tree--get-prop 'button-data)))
    (message "%s" id)
    (kill-new id)))


;;;; Initialisation

(defun denote-tree--build-extended-filetype (gen-from add-this)
  "Add keys and values from ADD-THIS to GEN-FROM alist.

This is purely backwards-compatible scheme to allow
`denote-tree-extend-filetype-with' to extend `denote-file-types' in case user's
denote version is too old."
  (let ((ext-filetype (copy-tree gen-from)))
    (dolist (type ext-filetype)
      (let ((ret '()))
        (dolist (key add-this)
          (push (plist-get (cdr key) (car type)) ret)
          (push (car key) ret))
        (setf (cdr type) (seq-union (cdr type) ret))))
    ext-filetype))


;;;; Interactive function helpers

(defun denote-tree--edit-node (buffer)
  "Call `denote-rename-file' interactively to edit BUFFER.

Return current buffer object."
  (let ((denote-save-buffers t))
    (with-current-buffer (find-file-noselect buffer)
      (call-interactively #'denote-rename-file)
      (current-buffer))))

(defun denote-tree--redraw-node (buffer alist)
  "Redraw node based on BUFFER's, return updated ALIST.

Include only elements from `denote-tree-node-description'.  Preserve
properties."
  (let ((inhibit-read-only t)
        (node (get-text-property (point) :denote-tree--identifier))
        (new-alist alist)
        (new-descp (denote-tree--collect-keywords-as-string
                    buffer denote-tree-node-description))
        (pos))
    (setf (alist-get node new-alist)
          (plist-put (alist-get node new-alist) :descp new-descp))
    (delete-region (line-beginning-position) (line-end-position))
    (setq pos
          (denote-tree--draw-node
           node
           (alist-get node new-alist)
           (denote-tree--nested-value
            alist node :parent :next-indent)))
    (setf (alist-get node new-alist)
          (plist-put (alist-get node new-alist) :pos pos))
    (delete-char 1)
    new-alist))


;;;; Tree traversal

(defun denote-tree--traverse-structure (alist &rest args)
  "Iterate on ALIST via ARGS.  Return a new alist.

This function abstracts the traversal of structure defied in ALIST, both the
`:call-fn' and `:other-fn' should return a list of elements ELEMENT, NEW-ALIST,
INFO and STACK which specify the the next step of iteration.

The full detail of all attributes:

 `:call-fn' - user supplied functions with 4 arguments which are in
              charge of maintaining the stack;
`:other-fn' - user supplied function with 3 arguments which is ran if function
              defined by `:call-fn' failed.  If this argument is empty, default
              to `(lambda (x y z) (list (cadr z) x y (cdr z)))';
 `:element' - current element under traversal;
    `:info' - stores the information specific to the current `:call-fn' or
              `:other-fn' implementation that can not be generalised;
   `:stack' - maintains the elements to traverse further."
  (let ((element (plist-get args :element))
        (info (plist-get args :info))
        (stack (plist-get args :stack))
        (call-fn (plist-get args :call-fn))
        (other-fn (or (plist-get args :other-fn)
                      (lambda (x y z) (list (cadr z) x y (cdr z))))))
    (when (car alist)
      (let ((progress (make-progress-reporter "Building denote-tree buffer..."))
            (new-alist (copy-sequence alist)))
        (while element
          (seq-setq (element new-alist info stack)
                    (or (funcall call-fn element new-alist info stack)
                        (funcall other-fn new-alist info stack)))
          (progress-reporter-update progress))
        (progress-reporter-done progress)
        new-alist))))

(defun denote-tree--draw-tree (buffer)
  "Draw and propertize a tree in current buffer starting with BUFFER."
  (setq denote-tree--tree-alist
        (denote-tree--fix-children-in-alist
         (denote-tree--walk-links
          buffer
          :lastp t
          :depth denote-tree-max-traversal-depth)))
  (when denote-tree--tree-alist
    (denote-tree--draw-node-list denote-tree--tree-alist (intern buffer))))


;;;;; Draw a node

(defun denote-tree--draw-node-list (alist initial-node)
  "Draw every node in ALIST starting from INITIAL-NODE."
  (denote-tree--traverse-structure
   alist
   :element initial-node
   :info (alist-get initial-node alist)
   :stack (list initial-node)
   :call-fn #'denote-tree--draw-node-list-helper
   :other-fn (lambda (alist _ stack)
               (list (cadr stack)
                     alist
                     (alist-get (cadr stack) alist)
                     (cdr stack)))))

(defun denote-tree--draw-node-list-helper (node alist node-plist stack)
  "Set the current NODE in NODE-PLIST and advance the STACK.

This function calls `denote-tree--draw-node' to do an actual drawing.
Besides delegating the drawing part it also advances the stack
and sets up everything for next iteration."
  (let ((point (denote-tree--draw-node
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

(defun denote-tree--draw-node (node plist next-indent)
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
     :denote-tree--identifier node)
    (denote-tree--set-button point
                  (symbol-name (plist-get plist :true-name)))
    (insert "\n")
    point))

(defun denote-tree--calculate-indent (indent lastp)
  "Concat INDENT and either denote-tree-space or denote-tree-pipe."
  (concat indent (if lastp denote-tree-space denote-tree-pipe)))

(defun denote-tree--set-button (position buffer)
  "Add button to visit BUFFER at POSITION."
  (make-text-button position (+ position (length denote-tree-node))
                    'action #'denote-tree-enter-node
                    'button-data buffer))


;;;;; Build alist

(defun denote-tree--walk-links (buffer &rest args)
  "Walk along the links from BUFFER with ARGS.

This function returns a new alist that maps relations between nodes in
form specified by `denote-tree--node-plist'.

The following attributes are recognized:

      `:indent' - initial indentation for the first node;
       `:lastp' - is this node the last node?
       `:depth' - maximum depth the traversal is going to have, if t, then it's
                  unlimited;
      `:parent' - parent of a current node, the root's parent is nil;
        `:next' - next sibling of the parent node, if there is none, then it's
                  the same node;
        `:prev' - prev sibling of the parent node, if there is none, then it's
                  the same node;
`:suppl-alist:' - if this is not the first traversal, then this is the
                  current alist, which will be used to build a nowe one."
  (let* ((node (intern buffer))
         (indent (plist-get args :indent))
         (lastp (plist-get args :lastp))
         (depth (plist-get args :depth))
         (parent (plist-get args :parent))
         (next (or (plist-get args :next)
                   node))
         (prev (or (plist-get args :prev)
                   node))
         (suppl-alist (plist-get args :suppl-alist)))
    (denote-tree--traverse-structure
     (append
      (list
       (denote-tree--node-plist
        node
        :true-name node
        :next next
        :prev prev
        :parent parent
        :indent indent
        :lastp lastp
        :depth depth))
      suppl-alist)
     :element node
     :stack (list node)
     :call-fn #'denote-tree--grow-alist-and-stack
     :other-fn (lambda (alist _ stack)
                 (list (cadr stack)
                       alist
                       nil
                       (cdr stack))))))

(defun denote-tree--grow-alist-and-stack (node alist info stack)
  "Add NODE to ALIST, fetch more nodes for STACK."
  (when-let* (((eq node (denote-tree--nested-value alist node :true-name)))
              (depth (denote-tree--nested-value alist node :depth)))
    (let* ((indent (denote-tree--nested-value alist node :next-indent))
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
           (children
            (seq-filter (lambda (x)
                          (denote-tree--open-link-maybe (symbol-name (cdr x))))
                        uniq-links-in-node))
           (children-list (mapcar #'car children))
           (last-children-node (car (last children-list)))
           (new-stack (append children-list (cdr stack)))
           (new-alist
            (append
             (mapcar (lambda (x)
                       (denote-tree--node-plist
                        (car x)
                        :true-name (cdr x)
                        :next (denote-tree--next-sibling (car x) children-list)
                        :prev (denote-tree--next-sibling (car x) (reverse children-list))
                        :parent node
                        :indent indent
                        :lastp (eq (car x) last-children-node)
                        :depth new-depth))
                     children)
             alist)))
      (setf (alist-get node new-alist)
            (plist-put (alist-get node new-alist) :children children-list))
      (list (car new-stack) new-alist info new-stack))))

(defun denote-tree--node-plist (x &rest args)
  "Build full plist for node X.

The following attributes are recognised:

`:true-name' - denote id of the note;
     `:next' - next sibling;
     `:prev' - previous sibling;
   `:parent' - parent node;
   `:indent' - next indent;
    `:lastp' - is the node last node;
    `:depth' - depth of the current node."
  (let* ((node x)
         (true-node (plist-get args :true-name))
         (indent (denote-tree--calculate-indent
                  (or (plist-get args :indent) "")
                  (plist-get args :lastp))))
    (denote-tree--open-link-maybe (symbol-name true-node))
    (list
     node
     :next-indent indent
     :true-name true-node
     :next (plist-get args :next)
     :prev (plist-get args :prev)
     :descp (denote-tree--collect-keywords-as-string
             (symbol-name true-node) denote-tree-node-description)
     :children nil
     :parent (plist-get args :parent)
     :last (plist-get args :lastp)
     :depth (plist-get args :depth))))

(defun denote-tree--next-sibling (x siblings)
  "Return the :next SIBLING of X."
  (when-let* (((seq-contains-p siblings x))
              (next (copy-tree siblings)))
    (setcdr (last next) next)
    (cadr (memq x next))))

(defun denote-tree--fix-children-in-alist (alist)
  "Copy :children of true node to the same prop of duplicate node in ALIST."
  (let (new-alist)
    ;; the order matters, make it not so
    (dolist (x alist (nreverse new-alist))
      (if-let* ((true-name (plist-get (cdr x) :true-name))
                ((not (eq (car x) true-name)))
                (children (denote-tree--nested-value alist true-name :children)))
          (push (append (list (car x)) (plist-put (cdr x) :children children))
                new-alist)
        (push x new-alist)))))

(defun denote-tree--unique-nodes (node existsp)
  "Return a pair new id of NODE and NODE symbol itself.

If EXISTSP, return an unique identifier."
  (cons (if existsp (gensym node) node)
        node))


;;;;;; Individual node interaction

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

(defun denote-tree--find-filetype (buffer)
  "Guess the filetype in BUFFER and return it as a symbol.

`denote-tree--find-filetype' works refering only to a buffer by finding
any regex from `denote-tree--extended-filetype' that matches in the
front matter.  This can be potentially expensive (worst case scenario is
not finding a match), but guaranteed to work as long the user set the
front-matter."
  (with-current-buffer buffer
    (goto-char (point-min))
    (let ((filetype))
      (setq filetype
	          (catch 'file-type
	            (dolist (type-plist denote-tree--extended-filetype)
		            (dolist (el (denote-tree--get-regexps (cdr type-plist)))
		              (let ((symbol-in-buff
			                   (save-excursion
			                     (re-search-forward (cadr el) nil t))))
		                (when symbol-in-buff
		                  (throw 'file-type type-plist)))))))
      (unless filetype
	      (warn "%s not a denote-style buffer" buffer))
      filetype)))

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
      (seq-uniq
       (delete (intern buffer-id) (nreverse found-ids))
       #'eq))))

(defun denote-tree--collect-keywords-as-string (buffer keywords)
  "Return KEYWORDS as a joint string from BUFFER."
  (let ((result '()))
    (dolist (el (denote-tree--collect-keywords buffer keywords))
      (when (cdr el)
        (push (cdr el) result)))
    (string-join (nreverse result) " ")))

(defun denote-tree--collect-keywords (buffer keywords)
  "Return denote propertized KEYWORDS from BUFFER."
  (when-let* ((regexps
               (thread-first
                 (denote-tree--find-filetype buffer)
                 (cdr)
                 (denote-tree--get-regexps))))
    (with-current-buffer buffer
      (let ((result '()))
        (dolist (el keywords result)
          (push (denote-tree--collect-keywords-helper el regexps)
                result))))))

(defun denote-tree--collect-keywords-helper (el regexps)
  "Turn EL into cons according to REGEXPS."
  (goto-char (point-min))
  (or (and (stringp el)
           (cons 'str el))
      (and (re-search-forward
            (denote-tree--extract-and-compare-symbols el regexps) nil t)
           (cons el (funcall denote-tree-node-colorize-function
                             (denote-trim-whitespace
                              (buffer-substring-no-properties
                               (point) (line-end-position)))
                             el)))
      (and (symbolp el)
           (cons el nil))))

(defun denote-tree--get-regexps (plist)
  "Return alist of all keys ending in -regexp with values in PLIST."
  (let (lst el)
    (while plist
      (setq el (car plist))
      (and (symbolp el)
	         (string-suffix-p
	          "-regexp" (symbol-name el))
	         (stringp (cadr plist))
	         (push (cadr plist) lst)
	         (push el lst))
      (setq plist (cddr plist)))
    (seq-partition lst 2)))

(defun denote-tree--extract-and-compare-symbols
    (el regexps &optional extractor-regexp)
  "Return symbol in REGEXPS, that matches EL.

Optional argument EXTRACTOR-REGEXP is passed along to
`denote-tree--extract-and-compare-symbol' and returns the matching
symbol."
  (catch 'break
    (let ((regexps (if (listp regexps) regexps (list regexps))))
      (dolist (reg regexps)
        (let ((symbol (denote-tree--extract-and-compare-symbol
                       (car reg) el extractor-regexp)))
          (when symbol
            (throw 'break (car (alist-get symbol regexps)))))))))

(defun denote-tree--extract-and-compare-symbol
    (symbol element &optional extractor-regexp)
  "Apply EXTRACTOR-REGEXP to SYMBOL and compare with ELEMENT.

EXTRACTOR-REGEXP should capture one group, which will be transformed
into shortened form.  If EXTRACTOR-REGEXP is nil, then the default value
mangles the SYMBOL like so,

:key-value-regexp      -> key
:foo-bar-regexp        -> foo
:identifier-val-regexp -> identifier

If the mangled form is `eq' to SYMBOL, return the SYMBOL."
  (or extractor-regexp (setq extractor-regexp ":\\(.+?\\)-\\(?:.*?\\)regexp"))
  (and (eq (intern
            (replace-regexp-in-string
             extractor-regexp "\\1" (symbol-name symbol)))
           element)
       symbol))


;;;; Redraw

(defun denote-tree--deepen-traversal (alist)
  "Retraverse current node under point with ALIST.

Especially useful, if `denote-tree-max-traversal-depth' is set to very
low value."
  (let* ((inhibit-read-only t)
         (curr-pos (point))
         (curr-node (get-text-property (point) :denote-tree--identifier))
         (new-alist alist)
         (orphans '())
         (move-orphans-to '()))
    ;; trying to redraw from cyclical node, wth?
    (when (eq curr-node (denote-tree--nested-value alist curr-node :true-name))
      (unwind-protect
          (progn
            (save-restriction
              (apply #'narrow-to-region
                     (denote-tree--determine-node-bounds
                      curr-node alist))
              (let* ((alist-in-region
                      (save-excursion
                        (denote-tree--alist-in-region alist)))
                     (alist-sans-region
                      (seq-difference alist alist-in-region)))
                (setq new-alist (denote-tree--fix-children-in-alist
                                 (denote-tree--walk-links
                                  (symbol-name curr-node)
                                  :indent (buffer-substring-no-properties
                                           (line-beginning-position)
                                           (- (denote-tree--get-node-pos)
                                              (length denote-tree-node)))
                                  :lastp (denote-tree--nested-value
                                          alist curr-node :last)
                                  :depth denote-tree-max-traversal-depth
                                  :parent (denote-tree--nested-value
                                           alist curr-node :parent)
                                  :next (denote-tree--nested-value
                                         alist curr-node :next)
                                  :prev (denote-tree--nested-value
                                         alist curr-node :prev)
                                  :suppl-alist alist-sans-region)))
                (setq orphans
                      (seq-difference (mapcar #'car alist-in-region)
                                      (mapcar #'car new-alist)))
                (when orphans
                  (setq move-orphans-to
                        (seq-uniq
                         (seq-remove
                          #'null (reverse (denote-tree--find-orphans orphans alist)))))
                  (let ((mapcared-move-orphans (mapcar #'car move-orphans-to)))
                    (setq new-alist
                          (seq-remove
                           (lambda (x)
                             (memq (car x) mapcared-move-orphans))
                           new-alist)))))
              (delete-region (point-min) (point-max))
              (goto-char (point-min))
              (denote-tree--draw-node-list new-alist curr-node)
              (delete-region (1- (point-max)) (point-max)))
            (when move-orphans-to
              (dolist (el move-orphans-to)
                (when-let* ((goto
                             (denote-tree--nested-value
                              new-alist (plist-get (cdr el) :parent) :pos)))
                  (goto-char goto)
                  (seq-let (_ alist) (denote-tree--deepen-traversal new-alist)
                    (setq new-alist alist))))))
        (denote-tree--clean-up)))
    (list curr-pos new-alist)))

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
         (next-pos (denote-tree--nested-value alist node :next :pos)))
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
         (let (prev-next next id)
           (setq id (get-text-property (point) :denote-tree--identifier))
           (while (and (setq next (denote-tree--nested-value
                                   alist id :next :pos))
                       (not (equal prev-next next))
                       (> now-pos next))
             (setq prev-next next)
             (goto-char (or (denote-tree--nested-value alist id :parent :pos)
                            1))
             (setq id (get-text-property (point) :denote-tree--identifier)))
           (if (> now-pos (or next 1))
               (1- (point-max))
             (goto-char (denote-tree--nested-value alist id :next :pos))
             (forward-line -1)
             (line-end-position)))))
      (t (error "Denote tree buffer %s is malformed" (buffer-name)))))))

(defun denote-tree--alist-in-region (alist)
  "Return ALIST of nodes from the current region."
  (let* ((nodes-in-region
          (denote-tree--walk-region
           (lambda ()
             (get-text-property
              (point) :denote-tree--identifier))))
         (result '()))
    (dolist (el nodes-in-region result)
      (push (assq el alist) result))))

(defun denote-tree--walk-region (func)
  "Step through every line of region and apply FUNC to it.

Return a payload."
  (let ((payload '()))
    (while (< (point) (point-max))
      (setq payload (append (list (funcall func)) payload))
      (forward-line))
    payload))

(defun denote-tree--find-orphans (orphaned alist)
  "Find ORPHANED nodes in an ALIST.

If a node is deleted during rescan of a tree, then there is
a possibility, that that node had cyclical buffers associated
with it.  Children of that node become effectively lost."
  (let ((result '()))
    (dolist (orp-el orphaned result)
      (let ((orphan (denote-tree--first-orphan (symbol-name orp-el) alist)))
        (push orphan result)))))

(defun denote-tree--first-orphan (el-name alist)
  "Return first element of ALIST equal to EL-NAME or nil."
  (catch 'break
    (dolist (alist-el alist)
      (let ((alist-key (symbol-name (car alist-el))))
        (when (and (not (string= el-name alist-key))
                   (string-prefix-p el-name alist-key))
          (throw 'break alist-el))))))


;;;; General purpose helpers

(defun denote-tree--nested-value (alist initial-key &rest nested-value)
  "Iteratively return NESTED-VALUE of INITIAL-KEY in ALIST."
  (let* ((prop (car nested-value))
         (value (plist-get (alist-get initial-key alist) prop)))
    (dolist (trio (cdr nested-value) value)
      (seq-find
       (lambda (x) (setq value (plist-get (alist-get x alist) trio)))
       (if (listp value) value (list value))))))

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


;;;; Finalization

(defun denote-tree--clean-up ()
  "Clean up buffers created during the tree walk."
  (dolist (el denote-tree--visited-buffers)
    ;; silence all kill-buffer errors
    (condition-case nil
        (kill-buffer el)
      (error nil))))

(provide 'denote-tree)
;;; denote-tree.el ends here
