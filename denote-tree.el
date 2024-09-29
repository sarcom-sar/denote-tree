;;; denote-tree.el --- Visualize your notes as a tree -*- lexical-binding: t -*-

;; Copyright 2024, Sararin
;; Created: 2024-09-15 Sun
;; Version: 0.1.0
;; Keywords: convenience
;; URL: http://127.0.0.1/
;; Package-Requires: ((emacs "27.2"))

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

(defvar denote-tree--mark-tree '()
  "Tree of points in the `*denote-tree*' where nodes are.
Used directly to traverse the tree structure.")

(defvar denote-tree--visited-buffers '()
  "List of already created buffers.")

(defvar denote-tree--cyclic-buffers '()
  "List of buffers that are cyclic nodes.")

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
  (with-current-buffer-window "*denote-tree*" nil nil
      (erase-buffer)
      (denote-tree--draw-tree
       (denote-tree--walk-links buffer))))

(defun denote-tree--draw-tree (node)
  "Draw a tree in current buffer starting with NODE."
  (setq denote-tree--mark-tree
        (denote-tree--draw-tree-helper node "" t)))

(defun denote-tree--draw-tree-helper (node indent last-child)
  "Insert INDENT and current NODE into the buffer.
If dealing with LAST-CHILD of NODE, alter pretty printing."
  (insert indent)
  (cond
   (last-child
    (setq indent (concat indent "  "))
    (insert "'-"))
   (t
    (setq indent (concat indent "| "))
    (insert "+-")))
  (insert "* " (denote-tree--collect-keyword (car node) "title") "\n")
  (dolist (el (cdr node))
    (denote-tree--draw-tree-helper el
                                   indent
                                   (equal el
                                          (car (last node))))))

(provide 'denote-tree)
;;; denote-tree.el ends here
