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

(defvar-local denote-tree--cyclic-buffers '()
  "List of buffers that are cyclic nodes.

Every entry of `denote-tree--cyclic-buffers' is denote ID that appears
cyclically over the buffer.  `cdr' of that variable is set to the list
of positions at which that denote ID is present.")

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
    (denote-tree--deepen-traversal))))

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
        next-point curr-point)
    (cond
     ((listp arg)
      (setq preserve-teleport-p (not denote-tree-preserve-teleports-p))
      (setq arg 1))
     ((eq arg '-)
      (setq arg -1)))
    (if (< arg 0)
        (denote-tree-parent-node (- arg))
      (dotimes (_ arg next-point)
        (and (setq next-point (get-text-property (point) 'denote-tree--child))
             (setq curr-point
                   (denote-tree--get-node-pos curr-point))
             (goto-char next-point)
             preserve-teleport-p
             (> curr-point next-point)
             (push (list (set-marker (make-marker) curr-point) next-point)
                   denote-tree--teleport-stack))))))

(defun denote-tree-parent-node (&optional arg)
  "Move the point to the parent of a node ARG times.

If ARG is negative move to the child of a node ARG times.  If ARG is
ommited, nil or zero, move once.

If `denote-tree-preserve-teleports-p' is set to t, teleport to the
parent the point came from."
  (interactive "p")
  (or arg (setq arg 1))
  (let (next-point canon-point current-teleport)
    (if (< arg 0)
        (denote-tree-child-node (- arg))
      (dotimes (_ arg next-point)
        (and (setq next-point (get-text-property (point) 'denote-tree--parent))
             (setq canon-point
                   (get-text-property next-point 'denote-tree--child))
             (goto-char next-point)
             (setq current-teleport (car denote-tree--teleport-stack))
             (equal canon-point (cadr current-teleport))
             (setq next-point (goto-char (car current-teleport)))
             (pop denote-tree--teleport-stack))))))

(defun denote-tree-next-node (&optional arg)
  "Move the point to the next sibling node ARG times.

If ARG is negative move to the prev sibling node ARG times.  If ARG is
omitted, nil or zero, move once."
  (interactive "p")
  (or arg (setq arg 1))
  (let ((direction (if (<= arg 0) 'denote-tree--prev 'denote-tree--next))
        (arg (abs arg))
        next-point)
    (dotimes (_ arg next-point)
      (and (setq next-point (get-text-property (point) direction))
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
          (denote-tree--redraw-node buffer (point)))))))


;;;; Utilities for node editing

(defun denote-tree--edit-node (buffer)
  "Call `denote-rename-file' interactively to edit BUFFER.

Return current buffer object."
  (let ((denote-save-buffers t))
    (with-current-buffer (find-file-noselect buffer)
      (call-interactively #'denote-rename-file)
      (current-buffer))))

(defun denote-tree--redraw-node (buffer pos)
  "Redraw node based on BUFFER's front matter at POS.

Include only elements from `denote-tree-node-description'.  Preserve
properties."
  (let ((inhibit-read-only t)
        (props (text-properties-at (line-beginning-position))))
    (save-excursion
      (goto-char pos)
      (delete-region pos (line-end-position))
      (insert (denote-tree--collect-keywords-as-string
               buffer denote-tree-node-description))
      (add-text-properties pos (line-end-position) props))))


;;;; Tree traversal

(defun denote-tree--draw-tree (buffer)
  "Draw and propertize a tree in current buffer starting with BUFFER."
  (let ((progress (make-progress-reporter "Building denote-tree buffer...")))
    (denote-tree--walk-links
     buffer "" t denote-tree-max-traversal-depth progress)
    (delete-region (1- (point-max)) (point-max))
    (denote-tree--add-props-to-cycles)
    (progress-reporter-done progress)))

(defun denote-tree--walk-links (buffer indent lastp depth &optional progress)
  "Walk along the links starting from BUFFER.

Draw the current buffer as a node in `denote-tree--buffer-name'.  Set
it's properties.  Collect all the links and call
`denote-tree--walk-links' on them recursively.  If BUFFER was already
visited do not iterate over it.  If BUFFER doesn't have a file, skip
over and return a symbol \\='notvalid.

Argument INDENT   - state of INDENT between traversals.
Argument LASTP    - is the node the last child of parent node?
Argument DEPTH    - maximum depth of the traversal.
Argument PROGRESS - a progress reporter."
  ;; draw node in buffer,
  ;; extract position of point at node
  ;; carry over the indent
  (if-let* ((buffer (denote-tree--open-link-maybe buffer)))
      (let ((links-in-buffer (denote-tree--collect-links buffer))
            (depth (cond
                    ((symbolp depth) depth)
                    ((and (numberp depth) (< 0 (1- depth))) (1- depth))
                    ((and (numberp depth) (= 0 (1- depth))) nil)
                    (t t)))
            node-children pos)
        (seq-setq (pos indent) (denote-tree--draw-node buffer indent lastp))
        (when progress
          (progress-reporter-update progress))
        ;; traverse the buffer structure
        ;; if current buffer is in denote-tree--cyclic-buffers
        ;; do not go deeper, because you enter a cycle
        (unless (member buffer denote-tree--cyclic-buffers)
          (dolist (el links-in-buffer)
            (when (and (get-buffer el)
                       (not (member
                             el denote-tree--cyclic-buffers)))
              (push el denote-tree--cyclic-buffers))
            (when depth
              (setq lastp (string= el (car (last links-in-buffer))))
              (push (denote-tree--walk-links
                     el indent lastp depth progress)
                    node-children))))
        ;; add props to current node and it's children
        (denote-tree--set-button pos buffer)
        (denote-tree--add-props-to-children
         (nreverse (seq-filter #'markerp node-children)) pos)
        pos)
    'notvalid))

(defun denote-tree--walk-links-iteratively (buffer indent &optional lastp)
  "Walk links from BUFFER with starting INDENT."
  (let* ((node (intern buffer))
         (children (list (intern buffer)))
         (node-alist (list (list node
                                 :next-indent (denote-tree--calculate-indent
                                               indent lastp)
                                 :next node
                                 :prev node
                                 :parent nil
                                 :name buffer
                                 :descp (denote-tree--collect-keywords-as-string
                                         buffer denote-tree-node-description)
                                 :last lastp))))
    (while node
      (let* ((current-node
              (alist-get node node-alist))
             (parent-node
              (alist-get (plist-get current-node :parent) node-alist)))
        (insert
         (if parent-node
             (plist-get parent-node :next-indent)
           "")
         (if (plist-get current-node :last) denote-tree-lower-knee denote-tree-tee)
         (if (eq node (intern (plist-get current-node :name)))
             (propertize denote-tree-node 'face 'denote-tree-node)
           (propertize denote-tree-node 'face 'denote-tree-circular-node))
         (plist-get current-node :descp)))
      (setf (alist-get node node-alist)
            (append (alist-get node node-alist)
                    (list :pos (point-marker))))
      (insert "\n")
      (seq-setq (node node-alist children)
                (if (eq node (intern (plist-get (alist-get node node-alist) :name)))
                    (denote-tree--grow-alist-and-children
                     node node-alist children)
                  (list (cadr children) node-alist (cdr children)))))
    node-alist))

(defun denote-tree--grow-alist-and-children (node alist children)
  "Add NODE to ALIST, fetch more nodes for CHILDREN."
  (let* ((current-plist (alist-get node alist))
         (node (denote-tree--open-link-maybe (symbol-name node)))
         (indent (plist-get (alist-get node alist) :next-indent))
         (children-nodes (save-excursion (denote-tree--collect-links (symbol-name node))))
         (last-children-node (car (last children-nodes)))
         (uniq-links-in-node
          (mapcar (lambda (x)
                    (denote-tree--unique-nodes
                     x alist node indent (eq x last-children-node)))
                  children-nodes))
         (keys (mapcar #'car uniq-links-in-node)))
    (mapc (lambda (x) (push x alist)) uniq-links-in-node)
    (mapc (lambda (x)
            (denote-tree--next-sibling x alist keys))
          keys)
    (setf (alist-get node alist)
          (append (list :children keys) current-plist))
    (setq children (append keys (cdr children)))
    (list (car children) alist children)))

(defun denote-tree--unique-nodes (x alist &optional parent indent lastp)
  "Construct skeletal plist of X.

If X already exists in ALIST, create new copy."
  (let* ((indent (denote-tree--calculate-indent indent lastp)))
    (list
     (if (alist-get x alist) (gensym x) x)
     :next-indent indent
     :name (symbol-name x)
     :descp (denote-tree--collect-keywords-as-string
             (symbol-name x) denote-tree-node-description)
     :parent parent
     :last lastp)))

(defun denote-tree--next-sibling (x alist siblings)
  "Set :next/:prev property of X in ALIST."
  (let ((next (copy-tree siblings))
        (prev (copy-tree (reverse siblings))))
    (setcdr (last next) next)
    (setcdr (last prev) prev)
    (setf (alist-get x alist)
          (append (list :next (cadr (memq x next))
                        :prev (cadr (memq x prev)))
                  (alist-get x alist)))))

(defun denote-tree--add-props-to-cycles ()
  "Add \\='denote-tree--child prop to elements of `denote-tree--cyclic-buffers'.

Iterate over `denote-tree--cyclic-buffers' finding the original and then
setting \\='denote-tree--child prop of other cyclic buffers to the value
of the original."
  (dolist (node-id denote-tree--cyclic-buffers)
    (goto-char (point-min))
    (while (and (> (point-max) (point))
                (not (eq (get-text-property (point) 'face) 'denote-tree-node)))
      (text-property-search-forward 'button-data node-id))
    (let ((marker (get-text-property (point) 'denote-tree--child)))
      (goto-char (point-min))
      (while (text-property-search-forward 'button-data node-id)
        (when (eq (get-text-property (point) 'face) 'denote-tree-circular-node)
          (add-text-properties
           (line-beginning-position) (line-end-position)
           (list 'denote-tree--child marker)))))))

(defun denote-tree--calculate-indent (indent lastp)
  (concat indent (if lastp denote-tree-space denote-tree-pipe)))

(defun denote-tree--draw-node (node-name indent lastp)
  "Draw NODE-NAME according to INDENT in current buffer.

Insert the current line as follows INDENT `denote-tree-node' title of
the current denote note.  Face of `denote-tree-node' is either
`denote-tree-circular-node' if current NODE-NAME is a member of
`denote-tree--cyclic-buffers' or `denote-tree-node' if it's not.  Call
`denote-tree-node-colorize-function' on title.

Return location of a point where the node starts and the current indent.
Argument LASTP is the current node last child of parent."
  (let ((circularp (member node-name denote-tree--cyclic-buffers))
        (keywords denote-tree-node-description)
        point-star-loc)
    (insert indent)
    (cond
     (lastp
      (setq indent (concat indent denote-tree-space))
      (insert denote-tree-lower-knee))
     (t
      (setq indent (concat indent denote-tree-pipe))
      (insert denote-tree-tee)))
    (setq point-star-loc (point-marker))
    (insert
     (propertize denote-tree-node
                 'face
                 (if circularp
                     'denote-tree-circular-node
                   'denote-tree-node))
     (denote-tree--collect-keywords-as-string node-name keywords) "\n")
    (list point-star-loc indent)))

(defun denote-tree--set-button (position buffer)
  "Add button to visit BUFFER at POSITION."
  (make-text-button position (+ position (length denote-tree-node))
                    'action #'denote-tree-enter-node
                    'button-data buffer))

(defun denote-tree--add-props-to-children (node-children parent)
  "Iterate over NODE-CHILDREN to set node's props.  Keep node's PARENT.

Every node contains props \\='denote-tree--next, \\='denote-tree--prev
and \\='denote-tree--parent which contain point's position to go to get
to previous/next sibling node or a parent."
  (when (and parent node-children)
    (save-excursion
      (goto-char parent)
      (add-text-properties
       (line-beginning-position) (line-end-position)
       (list
        'denote-tree--child (car node-children)))))
  (let ((prev (car (last node-children)))
        (next (copy-sequence node-children)))
    (when next
      (setcdr (last next) next))
    (dolist (current node-children)
      (setq next (cdr next))
      ;; if tail is null, then we are at last element,
      ;; fetch start of child nodes
      (save-excursion
        (goto-char current)
        (add-text-properties
         (line-beginning-position) (line-end-position)
         (list
          'denote-tree--next (set-marker (make-marker) (car next))
          'denote-tree--prev (set-marker (make-marker) prev)
          'denote-tree--parent (set-marker (make-marker) parent))))
      (setq prev current))))

(defun denote-tree--deepen-traversal ()
  "Retraverse current node under point.

Especially useful, if `denote-tree-max-traversal-depth' is set to very
low value."
  (let* ((inhibit-read-only t)
         ;; pos of current node
         (old-buffer (buffer-name))
         (node-pos (denote-tree--get-node-pos))
         (marker-alist (denote-tree--build-marker-alist node-pos))
         (id (denote-tree--get-prop 'button-data))
         (indent (buffer-substring-no-properties
                  (line-beginning-position)
                  (- node-pos (length denote-tree-node))))
         (lastp (save-excursion
                  (goto-char (line-beginning-position))
                  (search-forward
                   denote-tree-lower-knee (line-end-position) t)))
         (reg-beg) (reg-end)
         (_ (seq-setq (reg-beg reg-end)
                      (denote-tree--determine-node-bounds
                       node-pos marker-alist))))
    (seq-let (visited-buffers cyclic-buffers)
        (denote-tree--nuke-props-in-region reg-beg reg-end)
      (with-temp-buffer ;; new-buffer
        (let ((new-buffer (buffer-name)))
          (setq denote-tree--visited-buffers visited-buffers
                denote-tree--cyclic-buffers cyclic-buffers)
          (unwind-protect
              (let ((progress (make-progress-reporter
                               "Rebuilding denote-tree buffer...")))
                (denote-tree--walk-links
                 id indent lastp denote-tree-max-traversal-depth progress)
                (progress-reporter-done progress))
            (denote-tree--clean-up))
          (setq visited-buffers denote-tree--visited-buffers
                cyclic-buffers denote-tree--cyclic-buffers)
          (with-current-buffer old-buffer
            (save-restriction
              (narrow-to-region reg-beg (1+ reg-end))
              (with-current-buffer new-buffer
                (denote-tree--compare-and-insert-new-to old-buffer (point-min) 1))
              (goto-char (point-min))
              (denote-tree--set-positions-to-markers)
              (goto-char (point-min))))))
      (setq denote-tree--visited-buffers
            (seq-union denote-tree--visited-buffers
                       visited-buffers))
      (setq denote-tree--cyclic-buffers
            (seq-union denote-tree--cyclic-buffers
                       cyclic-buffers)))
    (denote-tree--add-props-to-cycles)
    (goto-char node-pos)))

(defun denote-tree--sanitize-deleted-entries (buffer)
  "Remove all nodes in BUFFER not present during redrawing.

Iterate over all the lines in BUFFER and if they are not present in
redrawn buffer, then remove them (and their children) from BUFFER."
  (let ((new-buf (current-buffer)))
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))
        (goto-char (line-end-position))
        (while (> (point-max) (point))
          (let* ((old-line
                  (buffer-substring-no-properties
                   (or (denote-tree--get-node-pos)
                       (point))
                   (line-end-position)))
                 (foundp (with-current-buffer new-buf
                           (goto-char 1)
                           (search-forward old-line nil t))))
            (if foundp
                (forward-line)
              ;; this is not the end, what about children?
              (denote-tree--link-next-and-prev-node (point))
              (save-restriction
                (apply #'narrow-to-region
                       (denote-tree--determine-node-bounds
                        (point) (denote-tree--build-marker-alist (point))))
                ;;unimplemented
                ;; (find-cyclical-buffers)
                ;; determine if they exist at all and every reference
                ;; to them should be deleted
                ;; or
                ;; reference are "killed" locally, then move the
                ;; resulting tree to earliest suitable location
                ;;unimplemented
                ;; (strip-all-markers)
                ;; iteratively remove /all/ markers in props
                (delete-region (point-min) (point-max)))
              (delete-region (point) (1+ (point))))))))))

(defun denote-tree--link-next-and-prev-node (pos)
  "Nodes in vicinity of node at POS point at nearest neighbor.

If node points at node at POS with \\='denote-tree--child prop set
marker to nil."
  (when-let* ((next (get-text-property pos 'denote-tree--next))
              (next-prev (get-text-property next 'denote-tree--prev))
              (prev (get-text-property pos 'denote-tree--prev))
              (prev-next (get-text-property prev 'denote-tree--next))
              (parent (get-text-property pos 'denote-tree--parent))
              (parent-child (get-text-property parent 'denote-tree--child)))
    ;; handle neighbors
    (cond
     ;; there are /at max/ two nodes
     ((equal next prev)
      (set-marker next-prev next)
      (set-marker prev-next prev))
     ;; there is more than two nodes
     (t
      (set-marker next-prev prev)
      (set-marker prev-next next)))
    ;; handle parent
    (let ((next-next (get-text-property next 'denote-tree--next)))
      (cond
       ((and (= next-next pos)
             (= parent-child pos))
        (set-marker parent-child nil))
       ((= parent-child pos)
        (set-marker parent-child next))))))

(defun denote-tree--compare-and-insert-new-to
    (buffer old-pos new-pos)
  "Insert current buffer nodes into BUFFER starting from OLD-POS.

Argument NEW-POS - a corresponding position in a temporary buffer where
                   redrawing with `denote-tree--deepen-traversal' takes
                   place."
  (with-current-buffer buffer
      (goto-char old-pos))
  (goto-char new-pos)
  (while (> (point-max) (point))
    (let ((old-line (with-current-buffer buffer
                      (buffer-substring (line-beginning-position) (line-end-position))))
          (new-line (buffer-substring (line-beginning-position) (line-end-position))))
      (cond
       ((eq t (compare-strings old-line nil nil new-line nil nil t))
        (with-current-buffer buffer
          (denote-tree--copy-new-markers-to-old-node new-line)
          (forward-line))
        (forward-line))
       (t
        (with-current-buffer buffer
          (save-excursion
            (denote-tree--insert-new-node-and-markers new-line))
          (goto-char (line-end-position)))
        (forward-line))))))

(defun denote-tree--copy-new-markers-to-old-node (payload)
  "Copy props from PAYLOAD and insert them to old node.

PAYLOAD comes from smaller buffer.  It's properties need to be
realigned, so marker positions match those of bigger buffer."
  (let ((text-props (text-properties-at (line-beginning-position))))
    (dolist (el '(denote-tree--child
                  denote-tree--next
                  denote-tree--prev
                  denote-tree--parent))
      (when-let* (((null (plist-member text-props el)))
                  (new-marker (get-text-property 0 el payload))
                  (new-position (1- (+ (point-min) new-marker))))
        (setq text-props
              (plist-put text-props el (cons new-marker new-position)))))
    (add-text-properties (line-beginning-position) (line-end-position)
                         text-props)))

(defun denote-tree--insert-new-node-and-markers (payload)
  "Insert PAYLOAD and correct it's properties.

PAYLOAD comes from smaller buffer.  It's properties need to be
realigned, so marker positions match those of bigger buffer."
  (forward-line -1)
  (goto-char (line-end-position))
  (insert "\n" payload)
  (let ((text-props (text-properties-at (line-beginning-position))))
    (dolist (el '(denote-tree--child
                  denote-tree--next
                  denote-tree--prev
                  denote-tree--parent))
      (when-let* ((marker (plist-get text-props el))
                  ((marker-position marker))
                  ;; node position in new buffer is offset from
                  ;; the marker of old buffer by (point-min)
                  (new-position (1- (+ (point-min) marker)))
                  (new-buffer (current-buffer)))
        (setf (plist-get text-props el)
              (cons marker new-position))))
    (add-text-properties (line-beginning-position) (line-end-position)
                         text-props)))

(defun denote-tree--set-positions-to-markers ()
  "Reset positions of prop markers from consp to singular value.

In previous step of redrawing, the newly inserted nodes had their new
position saved as `cdr' of a cons cell.  In restore those positions in
current buffers as actual positions."
  (while (> (point-max) (point))
    (let ((text-props (text-properties-at (line-beginning-position))))
      (dolist (el '(denote-tree--child
                    denote-tree--next
                    denote-tree--prev
                    denote-tree--parent))
        (when-let* ((pos (plist-get text-props el))
                    ((consp pos)))
          (setf (plist-get text-props el)
                (set-marker (car pos) (cdr pos) (current-buffer)))))
      (add-text-properties (line-beginning-position) (line-end-position)
                           text-props))
    (forward-line)
    (goto-char (line-end-position))))

(defun denote-tree--nuke-props-in-region (beg end)
  "For region BEG END remove all props and it's record.

Non cyclical nodes are removed from `denote-tree--visited-buffers' and
`denote-tree--cyclic-buffers'."
  (save-restriction
    (widen)
    (narrow-to-region beg end)
    (goto-char (point-min))
    (let ((non-cyclical '())
          (visited-buffers denote-tree--visited-buffers)
          (cyclic-buffers denote-tree--cyclic-buffers))
      (while (> (point-max) (point))
        (let* ((pos (denote-tree--get-node-pos))
               (data-prop (and pos (get-text-property pos 'button-data)))
               (face-prop (and pos (get-text-property pos 'face))))
          (when (eq face-prop 'denote-tree-node)
            (push data-prop non-cyclical))
          (goto-char (line-end-position))
          (forward-line)))
      (setq visited-buffers
            (seq-difference visited-buffers
                            non-cyclical))
      (setq cyclic-buffers
            (seq-difference cyclic-buffers
                            non-cyclical))
      (list visited-buffers cyclic-buffers))))

(defun denote-tree--determine-node-bounds (node-pos marker-alist)
  "Determine bounds of current node at NODE-POS with MARKER-ALIST.

MARKER-ALIST contains information about neighbors of the node.  Return
cons of node's start and node's end.

If \\='denote-tree--next doesn't exist, the situation is trivial.  If it
is further along the buffer than NODE-POS, then just jump to it and
return EoL of previous line.  If NODE-POS and \\='denote-tree--next
point to the same location \\='denote-tree--next precedes the NODE-POS,
then we can have arbitrary \"deepness\", iterate until you find parent
node which next node is grater than node to be redrawn.  If you ran out
of nodes to check, you are at the top and the last node is your target.
If nothing matches, signal an error."
  (list
   (line-beginning-position)
   (let-alist marker-alist
     (cond
      ((not (marker-position (car .denote-tree--next)))
       ;; do not kill the last newline
       (1- (point-max)))
      ((< node-pos (car .denote-tree--next))
       (save-excursion
         (goto-char (car .denote-tree--next))
         (forward-line -1)
         (line-end-position)))
      ((>= node-pos (car .denote-tree--next))
       (save-excursion
         (goto-char (car .denote-tree--parent))
         (let (next)
           (while (and (setq next (get-text-property (point) 'denote-tree--next))
                       (> node-pos next))
             (goto-char (get-text-property (point) 'denote-tree--parent))))
         (if (> node-pos (or (get-text-property (point) 'denote-tree--next) 1))
             ;; ditto
             (1- (point-max))
           (goto-char (get-text-property (point) 'denote-tree--next))
           (forward-line -1)
           (line-end-position))))
      (t (error "Denote tree buffer is malformed"))))))

(defmacro denote-tree--build-marker-alist (pos)
  "Return alist of KEY MARKER NEXT-PROP at POS.

The alist is made out of identifier of a marker, the marker itself and
the opposite identifier.  It's used when referencing the node under the
marker in order to set it's opposite to the current node."
  `(list
    (list 'denote-tree--prev
          (copy-marker
           (get-text-property ,pos 'denote-tree--prev))
          'denote-tree--next)
    (list 'denote-tree--next
          (copy-marker
           (get-text-property ,pos 'denote-tree--next))
          'denote-tree--prev)
    (list 'denote-tree--parent
          (copy-marker
           (get-text-property ,pos 'denote-tree--parent))
          'denote-tree--child)))


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
      (setq element nil)))
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
