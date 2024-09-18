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
;; FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; Loco. If not, see <https://www.gnu.org/licenses/>.

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

(provide 'denote-tree)
