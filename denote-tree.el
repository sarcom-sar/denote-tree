;;; denote-tree.el --- Visualize your notes as a tree -*- lexical-binding: t -*-

;; Copyright 2024, Sararin
;; Created: 2024-09-15 Sun
;; Version: 0.1.0
;; Keywords: convenience
;; URL: http://127.0.0.1/
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

;; denote-tree visualizes your notes as a tree.
;;
;; A       A1
;; +-B     B1
;; | '-C   C1
;; |   '-D D1
;; +-B     B2
;; | '-C   C2
;; '-B     B3
;; | +-C   C3
;; | '-C   C4
;; |   '-D D2
;; +-B     B4
;; +-B     B5

;;; Code:

(require 'denote)
(require 'org)
(require 'compat)

(defface denote-tree-node-face '((t :inherit link))
  "Default face used for nodes.")

(defcustom denote-tree-buffer-name "*denote-tree*"
  "Name of the buffer denote-tree will be built in.")

(defconst denote-tree-lower-knee "'-")
(defconst denote-tree-tee "+-")
(defconst denote-tree-space "  ")
(defconst denote-tree-pipe "| ")
(defconst denote-tree-node "*")

(defvar denote-tree--cyclic-trees '()
  "List of all partial trees that contain cycles.")

(defvar denote-tree--mark-tree '()
  "Tree of positions used by denote-tree buffer.
Used directly to traverse the tree structure.")

(defvar denote-tree--visited-buffers '()
  "List of already created buffers.")

(defvar denote-tree--cyclic-buffers '()
  "List of buffers that are cyclic nodes.")

(defvar-local denote-tree--pointer '()
  "Node the point is at.")

(defvar-local denote-tree--node-stack '()
  "Stack of parent nodes.")

(defvar-local denote-tree--pos-stack '()
  "Stack of point position in parent nodes.")

(defvar-local denote-tree--closure nil
  "Closure of current instance of `denote-tree--sideways-maker'.")

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
  :interactive nil
  (setq denote-tree--closure
        (denote-tree--movement-maker 1 0)) ; root never has siblings
  (setq denote-tree--pointer denote-tree--mark-tree))

(defun denote-tree--movement-maker (len-list init-val)
  "Return values from 0 to LEN-LIST."
  (let ((pos init-val)
        (len len-list)
        (val))
    (lambda (direction)
      (setq pos (+ pos direction))
      (setq val (mod pos len))
      val)))

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
  (let ((total))
    (dotimes (total arg)
      (when (cadr denote-tree--pointer)
        (push denote-tree--pointer denote-tree--node-stack)
        (push (funcall denote-tree--closure 0) denote-tree--pos-stack)
        (setq denote-tree--pointer (cadr denote-tree--pointer))
        (setq denote-tree--closure (denote-tree--movement-maker
                                    (length (cdar denote-tree--node-stack))
                                    0))
        (goto-char (car denote-tree--pointer))))))

(defun denote-tree-parent-node (&optional arg)
  "Move the point to the parent node of a current node ARG times.
If ARG is omitted or nil, move to the parent of a current node."
  (interactive "p")
  (or arg (setq arg 1))
  (let ((total 0))
    (dotimes (total arg)
      (when denote-tree--node-stack
        (setq denote-tree--pointer (pop denote-tree--node-stack))
        (setq denote-tree--closure (denote-tree--movement-maker
                                    (length (cdar denote-tree--node-stack))
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

(defun denote-tree--collect-links (buffer)
  "Collect all links of type denote in BUFFER."
  (setq buffer (denote-tree--open-link-maybe buffer))
  (with-current-buffer buffer
    (org-element-map (org-element-parse-buffer) 'link
      (lambda (link)
        (when (string= (org-element-property :type link) "denote")
          (org-element-property :path link))))))

(defun denote-tree--walk-links (buffer)
  "Return a tree of denote links starting with current BUFFER."
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
              (setq lst (append lst (list (denote-tree--walk-links el))))))
          lst)))))

(defun denote-tree--collect-keyword (buffer keyword)
  "Return org KEYWORD from BUFFER.
Return nil if none is found."
  (let ((collected-keyword))
    (with-current-buffer buffer
      (setq collected-keyword (org-collect-keywords (list keyword))))
    (car (cdar collected-keyword))))

(defun denote-tree--open-link-maybe (element)
  "Return ELEMENT buffer, create if necessary."
  (unless (member element denote-tree--visited-buffers)
    (add-to-list 'denote-tree--visited-buffers element)
    (get-buffer-create element)
    (with-current-buffer element
      (org-mode)
      (erase-buffer)
      (insert-file-contents (denote-get-path-by-id element))))
  element)

(defun denote-tree--clean-up ()
  "Clean up buffers created during the tree walk."
  (dolist (el denote-tree--visited-buffers)
    (kill-buffer el))
  (setq denote-tree--visited-buffers nil)
  (setq denote-tree--cyclic-buffers nil))

(defun denote-tree (&optional buffer)
  "Draw hierarchy between denote files as a tree.
The function uses either the current buffer, if called from a function
a BUFFER provided by the user."
  (interactive)
  (denote-tree--clean-up)
  (or buffer (setq buffer (denote-tree--collect-keyword (current-buffer)
                                                        "identifier")))
  (denote-tree--open-link-maybe buffer)
  (with-current-buffer-window denote-tree-buffer-name nil nil
    (let ((inhibit-read-only t))
      (erase-buffer)
      (setq denote-tree--mark-tree
            (denote-tree--draw-tree (denote-tree--walk-links buffer)))
      (denote-tree-mode)))
  (set-window-point (get-buffer-window denote-tree-buffer-name)
                    (goto-char (car denote-tree--mark-tree))))

(defun denote-tree--check (el lst)
  "Return the position of EL in LST if it exists.
Return nil otherwise."
  (let ((iter lst)
        (num 0))
    (while (not (or (equal el (car iter))
                    (equal iter nil)))
      (setq num (1+ num))
      (setq iter (cdr iter)))
    (unless (equal iter nil)
      num)))

(defun denote-tree--re-circularize-tree (node)
  "Return the tree with cyclical structure of the original.

If a current node is in `denote-tree--cyclic-buffers', save the entire
thing to `denote-tree--cyclic-trees'.  If a current node matches the
`car' of a node, set it's `cdr' to the `cdr' of
`denote-tree--cyclic-trees'.  Return modified tree."
  (if (not (listp node))
      node
    (let ((lst (list (car node)))
          (skip-this-loop nil)
          (checked-element)
          (pos-to-id))
      (setq pos-to-id
            (get-text-property (car node) 'denote--id))
      (cond
       ((setq checked-element
              (denote-tree--check
               (get-text-property (car node) 'denote--id)
               (mapcar #'(lambda (el)
                           (get-text-property el 'denote--id))
                       (mapcar #'car denote-tree--cyclic-trees))))
        (put-text-property (car node)
                           (1+ (car node))
                           'face 'denote-tree-circular-node-face)
        (setcdr node (cdr (nth checked-element denote-tree--cyclic-trees)))
        (setq skip-this-loop t)
        (setq lst (append lst (cdr node))))
       ((denote-tree--check pos-to-id denote-tree--cyclic-buffers)
        (setq denote-tree--cyclic-trees
              (append denote-tree--cyclic-trees (list node)))))
      (dolist (el (cdr node))
        (unless skip-this-loop
          (setq lst (append lst (list (denote-tree--re-circularize-tree el))))))
      lst)))

(defun denote-tree--draw-tree (node)
  "Draw a tree in current buffer starting with NODE."
  (denote-tree--draw-tree-helper node "" t))

;; it is /imperative/ to merge this function and
;; denote-tree--walk-links, because they do a lot
;; of similar things
(defun denote-tree--draw-tree-helper (node indent last-child)
  "Insert INDENT and current NODE into the buffer.
If dealing with LAST-CHILD of NODE, alter pretty printing."
  (let ((point-loc))
    (insert indent)
    (cond
     (last-child
      (setq indent (concat indent denote-tree-space))
      (insert denote-tree-lower-knee))
     (t
      (setq indent (concat indent denote-tree-pipe))
      (insert denote-tree-tee)))
    (insert denote-tree-node)
    (setq point-loc (1- (point)))
    (add-text-properties point-loc (point) (list 'denote--id (car node)
                                                 'face 'denote-tree-node-face))
    (insert " " (denote-tree--collect-keyword (car node) "title") "\n")
    (let ((lst (list point-loc))
          (lastp last-child))
      (dolist (el (cdr node) lst)
        (setq lastp (equal el (car (last node))))
        (setq lst (append lst (list (denote-tree--draw-tree-helper el
                                                                   indent
                                                                   lastp)))))
      lst)))

(provide 'denote-tree)
;;; denote-tree.el ends here
