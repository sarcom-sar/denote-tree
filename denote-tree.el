;;; denote-tree --- Visualize your notes as a tree -*- lexical-binding: t; -*-

;; Copyright 2024, Sararin
;; Created: 2024-09-15 Sun
;; Version: 0.1.0
;; Keywords: convenience
;; URL: http://127.0.0.1/
;; Package-Requires: ((emacs "29.1"))

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

;;; Code:

(defun denote-tree--walk (node)
  "Walks along the tree."
  (if (listp node)
      (let ((lst))
        (dolist (el node lst)
          (setq lst (append lst (denote-tree--walk el))))
        (list lst))
    (list node)))

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
    ; if no links return a buffer
    (if (null links-in-buffer)
        (list buffer)
      (let ((lst (cdar (org-collect-keywords '("identifier")))))
        ; if links go deeper
        (dolist (el links-in-buffer lst)
          (setq lst (append lst (list (denote-tree--walk-links el)))))
        lst))))

(defun denote-tree--open-link-maybe (element)
  "If ELEMENT is not a buffer, it's an id, open it."
  (if (bufferp element)
      element
    (find-file (denote-get-path-by-id element))))


(defun denote-tree--draw-tree (tree)
  "A mock as of right now."
  tree)

(provide 'denote-tree)
