;;; denote-tree.el --- Visualize your notes as a tree -*- lexical-binding: t -*-

;; Copyright 2024, Sararin
;; Created: 2024-09-15 Sun
;; Version: 0.7.5
;; Keywords: convenience
;; URL: http://github.com/sarcom-sar/denote-tree.el
;; Package-Requires: ((emacs "27.1") (denote "3.0.1"))

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
;; `denote-tree-preserve-teleports-p').  As a drawback, it is pretty stupid
;; and has to redraw entire thing from scratch, if anything changes.
;;
;; User can customize `denote-tree-node-face' and
;; `denote-tree-circular-node-face' to make them more visible.  With a bit of
;; hacking it is also feasible to implement colored node titles via
;; `denote-tree-node-colorize-function'.
;;
;; For performance reasons `denote-tree-max-traversal-depth' can be reduced.
;;

;;; Code:

(require 'denote)
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

Every `denote-tree' buffer has a unique name made from this prefix
and root node of it's tree."
  :group 'denote-tree
  :type 'string)

(defcustom denote-tree-node-colorize-function #'denote-tree--default-props
  "Add properties to information from the node according to type.

Function accepts two arguments STR and TYPE.  Choosen string from front-matter
is propertized according to type from denote-tree-include-from-front-matter."
  :group 'denote-tree
  :type 'function)

(defcustom denote-tree-max-traversal-depth t
  "Maximum traversal depth of `denote-tree'.
If t traverse all the way, if num, traverse n nodes deep."
  :group 'denote-tree
  :type '(choice symbol natnum))

(defcustom denote-tree-node-description '(title)
  "Elements of front matter to include in node's description.

User can also extend denote's front matter by any arbitrary element, but they
have to add corresponding regex to `denote-file-types' for `denote-tree' to
recognize it.  That user variable also supports arbitrary strings.

Denote's default front matter elements:
- title
- identifier
- keywords
- signature
- date string"
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

(defcustom denote-tree-fancy-edit t
  "If t, use fancy editing with widgets.
If nil fall back thin `denote-rename-file' wrapper."
  :group 'denote-tree
  :type 'boolean)


;;;; Vars and consts

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

(defvar-local denote-tree--teleport-stack '()
  "Stack of point positions denoting WHERE-TO jump FROM-WHERE.
FROM-WHERE is a positions of first child node.  WHERE-TO
is a point position of cyclical parent node.")

(defvar-local denote-tree--buffer-name ""
  "Actual name of the buffer with denote-tree's tree.")


;;;; Mode and interactive functions

(defvar denote-tree-mode-map
  (let ((map (make-sparse-keymap)))
    (bind-key "n" #'denote-tree-next-node map)
    (bind-key "p" #'denote-tree-prev-node map)
    (bind-key "f" #'denote-tree-child-node map)
    (bind-key "b" #'denote-tree-parent-node map)
    (bind-key "g" #'denote-tree-redraw map)
    (bind-key "e" #'denote-tree-edit-node map)
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
        (or buffer (setq buffer
                         (denote-tree--collect-keywords-as-string
                          (current-buffer) '(identifier))))
        (denote-tree--open-link-maybe buffer)
        (setq denote-tree--buffer-name
              (concat
               "*"
               denote-tree-buffer-prefix
               " "
               (if (bufferp buffer)
                   (buffer-name buffer)
                 buffer)
               "*"))
        (let ((inhibit-read-only t))
          (with-current-buffer (get-buffer-create denote-tree--buffer-name)
            (erase-buffer)
            (denote-tree-mode)
            (denote-tree--draw-tree buffer)
            (setq denote-tree--buffer-name (buffer-name))))
        (pop-to-buffer denote-tree--buffer-name)
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
With ARG set to \\[universal-argument], redraw from node at point."
  (interactive "P")
  (unless (equal arg '(4))
    (goto-char (point-min)))
  (when-let ((current-node
              (get-text-property
               (next-single-property-change
                (line-beginning-position) 'button-data)
               'button-data)))
    (denote-tree current-node)))

(defun denote-tree-child-node (&optional arg)
  "Move the point to the child of a node ARG times.
If ARG is negative move to the parent of a node ARG times.
If ARG is ommited, nil or zero, move once.  With \\[universal-argument]
reverse `denote-tree-preserve-teleports-p' one time.

If `denote-tree-preserve-teleports-p' is set to t, preserve
the parent node position for future backtracking."
  (interactive "P")
  (or arg (setq arg 1))
  (let ((preserve-teleport-p denote-tree-preserve-teleports-p))
    (when (equal arg '(4))
      (setq preserve-teleport-p (not denote-tree-preserve-teleports-p))
      (setq arg 1))
    (if (< arg 0)
        (denote-tree-parent-node (- arg))
      (dotimes (_ arg)
        (when-let ((next-point
                    (get-text-property (point) 'denote-tree--child))
                   (curr-point
                    (next-single-property-change
                     (line-beginning-position) 'button-data)))
          (when (and preserve-teleport-p
                     (> (point) next-point))
            (push (list curr-point next-point)
                  denote-tree--teleport-stack))
          (goto-char next-point))))))

(defun denote-tree-parent-node (&optional arg)
  "Move the point to the parent of a node ARG times.
If ARG is negative move to the child of a node ARG times.
If ARG is ommited, nil or zero, move once.

If `denote-tree-preserve-teleports-p' is set to t, teleport to
the parent the point came from."
  (interactive "p")
  (or arg (setq arg 1))
  (if (< arg 0)
      (denote-tree-child-node (- arg))
    (dotimes (_ arg)
      (when-let ((next-point (get-text-property
                              (point) 'denote-tree--parent))
                 (canon-point (get-text-property
                               next-point 'denote-tree--child)))
        (let ((current-teleport (car denote-tree--teleport-stack)))
          (if (equal canon-point (cadr current-teleport))
              (progn
                (goto-char (car current-teleport))
                (pop denote-tree--teleport-stack))
            (goto-char next-point)))))))

(defun denote-tree-next-node (&optional arg)
  "Move the point to the next sibling node ARG times.
If ARG is negative move to the prev sibling node ARG times.
If ARG is omitted, nil or zero, move once."
  (interactive "p")
  (or arg (setq arg 1))
  (let ((direction (if (<= arg 0) 'denote-tree--prev 'denote-tree--next))
        (arg (abs arg)))
    (dotimes (_ arg)
      (when-let ((next-point
                  (get-text-property (point) direction)))
        (goto-char next-point)))))

(defun denote-tree-prev-node (&optional arg)
  "Move the point to the prev sibling node ARG times.
If ARG is negative move to the nextv sibling node ARG times.
If ARG is omitted, nil or zero, move once."
  (interactive "p")
  (or arg (setq arg 1))
  (denote-tree-next-node (- arg)))

(defun denote-tree-edit-node ()
  "Edit node's front matter.  What is editable is dependent on `denote-prompts'.
If `denote-tree-edit-mode' is loaded, use it's UI."
  (interactive)
  (if denote-tree-fancy-edit
      (progn
        (require 'denote-tree-edit)
        (denote-tree-edit-mode))
    (let* ((identifier (get-text-property
                        (next-single-property-change
                         (line-beginning-position) 'button-data)
                        'button-data))
           (buffer (denote-tree--edit-node (denote-get-path-by-id identifier))))
      (save-excursion
        (goto-char (point-min))
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
Include only elements from `denote-tree-node-description'.

Preserve properties."
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
  (denote-tree--walk-links buffer "" t denote-tree-max-traversal-depth)
  (denote-tree--add-props-to-cycles))

(defun denote-tree--walk-links (buffer indent lastp depth)
  "Walk along the links starting from BUFFER.

Draw the current buffer as a node in `denote-tree--buffer-name'.  Set it's
properties.  Collect all the links and call `denote-tree--walk-links' on
them recursively.  If one of the buffers was already visited do not iterate
over it.

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
          (push (denote-tree--walk-links el indent lastp depth)
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
  (dolist (node-id-and-pos denote-tree--cyclic-buffers)
    (goto-char (point-min))
    (text-property-search-forward 'button-data (car node-id-and-pos))
    (let ((child-prop (get-text-property (point) 'denote-tree--child)))
      (dolist (node-pos (cdr node-id-and-pos))
        (goto-char node-pos)
        (add-text-properties (line-beginning-position)
                             (line-end-position)
                             (list 'denote-tree--child child-prop))))))

(defun denote-tree--draw-node (node-name indent lastp)
  "Draw NODE-NAME according to INDENT in current buffer.

Insert the current line as follows INDENT `denote-tree-node' title of
the current denote note.  Face of `denote-tree-node' is either
`denote-tree-circular-node-face' if current NODE-NAME is a member of
`denote-tree--cyclic-buffers' or `denote-tree-node-face' if it's not.
Call `denote-tree-node-colorize-function' on title.

Return location of a point where the node starts and the current indent.
Argument LASTP is the current node last child of parent."
  (let ((circularp (assoc node-name denote-tree--cyclic-buffers))
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
    (insert (propertize denote-tree-node
                        'face (if circularp
                                  'denote-tree-circular-node
                                'denote-tree-node))
            (denote-tree--collect-keywords-as-string node-name keywords)
            "\n")
    (cons point-star-loc indent)))

(defun denote-tree--set-button (position buffer)
  "Add button to visit BUFFER at POSITION."
  (make-text-button position
                    (+ position (length denote-tree-node))
                    'action #'denote-tree-enter-node
                    'button-data buffer))

(defun denote-tree--add-props-to-children (node-children parent)
  "Iterate over NODE-CHILDREN to set node's props.  Keep node's PARENT.

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


;;;; Helpers for Links and Buffers

(defun denote-tree--collect-links (buffer)
  "Collect all denote style identifiers in BUFFER.
Return as a list sans BUFFER own identifiers."
  (setq buffer (denote-tree--open-link-maybe buffer))
  (let ((buffer-id (cdar (denote-tree--collect-keywords buffer '(identifier))))
        found-ids)
    (with-current-buffer buffer
      (goto-char (point-min))
      (while (search-forward-regexp denote-id-regexp nil t)
        (push (concat (match-string-no-properties 1)
                      (match-string-no-properties 2))
              found-ids))
      ;; first element is /always/ the buffer's id
      (delete buffer-id (nreverse found-ids)))))

(defun denote-tree--build-full-filetype (filetype)
  "Build extended FILETYPE with additional regexps."
  (let ((f-type (copy-tree filetype)))
    (when-let ((type (car filetype))
               (oldp (not (plist-get (cdr filetype) :date-key-regexp))))
      (setf (cdr f-type)
            (append (cdr f-type)
                    (cond
                     ((eq type 'org)
                      (list :date-key-regexp "^#\\+date\\s-*:"
                            :signature-key-regexp "^#\\+signature\\s-*:"
                            :identifier-key-regexp "^#\\+identifier\\s-*:"))
                     ((eq type 'markdown-yaml)
                      (list :date-key-regexp "^date\\s-*:"
                            :signature-key-regexp "^signature\\s-*:"
                            :identifier-key-regexp "^identifier\\s-*:"))
                     ((eq type 'markdown-toml)
                      (list :date-key-regexp "^date\\s-*="
                            :signature-key-regexp "^signature\\s-*="
                            :identifier-key-regexp "^identifier\\s-*="))
                     ((eq type 'text)
                      (list :date-key-regexp "^date\\s-*:"
                            :signature-key-regexp "^signature\\s-*:"
                            :identifier-key-regexp "^identifier\\s-*:"))))))
    f-type))

(defun denote-tree--collect-keywords (buffer keywords)
  "Return denote propertized KEYWORDS from BUFFER.
Return \"\" if none are found."
  (when-let* ((filetype (denote-tree--build-full-filetype
                         (denote-tree--find-filetype buffer)))
              (regexps (denote-tree--get-regexps (cdr filetype))))
    (let (lst type)
      (with-current-buffer buffer
        (dolist (el keywords)
          (when-let* ((matching-regexp
                       (plist-get
                        (cdr filetype) (seq-find
                                        (lambda (reg)
                                          (denote-tree--compare reg el))
                                        regexps)))
                      ((goto-char (point-min)))
                      ((re-search-forward matching-regexp nil t)))
            (setq type el)
            (setq el (denote-trim-whitespace
                      (buffer-substring-no-properties
                       (point) (line-end-position)))))
          (cond
           ((stringp el)
            (push (cons type
                        (funcall
                         denote-tree-node-colorize-function el type))
                  lst))
           ((symbolp el)
            (push (list el) lst)))))
      lst)))

(defun denote-tree--get-regexps (plist)
  "Return list of all symbols ending in -regexp in PLIST."
  (let ((lst))
    (dolist (el plist lst)
      (when-let* (((symbolp el))
                  ((string-suffix-p
                    "-regexp" (symbol-name el)))
                  ((stringp (plist-get plist el))))
        (push el lst)))))

(defun denote-tree--compare (pot-regexp element)
  "Compare POT-REGEXP to ELEMENT.

Format of POT-REGEXP has to be :FOO-BAR-regexp, for example:

:key-value-regexp       -> key
:foo-bar-regexp        -> foo
:identifier-val-regexp -> identifier"
  (and (eq (intern
            (replace-regexp-in-string
             ":\\(.+?\\)-\\(?:.*?\\)regexp" "\\1" (symbol-name
                                                   pot-regexp)))
           element)
       pot-regexp))

(defun denote-tree--collect-keywords-as-string (buffer keywords)
  "Return KEYWORDS as a joint string from BUFFER."
  (let ((lst (seq-filter #'identity (mapcar #'cdr (denote-tree--collect-keywords buffer keywords)))))
    (string-join (nreverse lst) " ")))

(defun denote-tree--find-filetype (buffer)
  "Guess the filetype in BUFFER and return it as a symbol.

`denote-tree--find-filetype' works refering only to a buffer by finding any
regex from `denote-file-types' that matches in the front matter.
This can be potentially expensive (worst case scenario is not finding
a match), but guaranteed to work as long the user set the front-matter."
  (with-current-buffer buffer
    (goto-char (point-min))
    (let ((filetype (seq-find
                     (lambda (type)
                       (let ((types-plist
                              (denote-tree--build-full-filetype type)))
                         (seq-find
                          (lambda (el)
                            (save-excursion
                              (re-search-forward
                               (plist-get (cdr types-plist) el) nil t)))
                          (nreverse (denote-tree--get-regexps (cdr types-plist))))))
                     denote-file-types)))
      (unless filetype
        (message "%s not a denote-style buffer" buffer))
      filetype)))

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


;;;; Helper functions

(defun denote-tree--clean-up ()
  "Clean up buffers created during the tree walk."
  (dolist (el denote-tree--visited-buffers)
    (when (get-buffer el)
      (kill-buffer el)))
  (setq denote-tree--visited-buffers nil)
  (setq denote-tree--cyclic-buffers nil))

(defun denote-tree--default-props (str type)
  "Default function returning STR of TYPE with properties."
  (propertize str 'denote-tree--type type))

(provide 'denote-tree)
;;; denote-tree.el ends here
