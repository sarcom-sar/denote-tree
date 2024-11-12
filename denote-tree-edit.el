;;; denote-tree-edit.el --- Edit note with widgets -*- lexical-binding: t -*-

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

;; Small extension package which allows you to edit notes in-buffer.
;;

;;; Code:

(eval-when-compile
  (require 'denote)
  (require 'wid-edit))

(defvar-local denote-tree-edit--current-note '((file)
                                    (title . keep-current)
                                    (keywords . keep-current)
                                    (signature . keep-current)
                                    (date . keep-current))
  "Alist of a current note elements.")

(defvar-local denote-tree-edit--current-line nil
  "Beginning of currently edited line.")

(defvar denote-tree-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map widget-keymap)
    (define-key map (kbd "C-c C-c") #'denote-tree-edit-commit-changes)
    (define-key map (kbd "C-c C-k") #'denote-tree-edit-abort-changes)
    map)
  "Keymap for `denote-tree-edit-mode'.")

(define-derived-mode denote-tree-edit-mode text-mode "denote-tree-edit"
  "Edit the front matter of the note at point from within a tree.

Everything else is still read-only.  All newlines will be dropped.
\\{denote-tree-edit-mode-map}"
  (setq buffer-read-only nil)
  (setq denote-tree-edit--current-line (line-beginning-position))
  (setcdr (assq 'file denote-tree-edit--current-note)
          (denote-get-path-by-id
           (get-text-property
            (next-single-property-change (line-beginning-position)
                                         'button-data)
            'button-data)))
  (save-excursion
    (let ((inhibit-read-only t))
      ;; save to denote-tree-edit--current-note
      (denote-tree-edit--set-from-front-matter
       denote-tree-include-from-front-matter
       #'denote-tree-edit--save-match)
      (denote-tree-edit--widgetize-line))))

(defun denote-tree-edit--widgetize-line ()
  "Make line widgetized."
  (kill-region (+ (next-single-property-change
                   denote-tree-edit--current-line
                   'button-data)
                  (length denote-tree-node))
               (line-end-position))
  (goto-char (line-end-position))
  (dolist (el denote-tree-include-from-front-matter)
    (cond
     ((symbolp el)
      (widget-create 'editable-field
                     :size 13
                     :keymap (let ((map (make-sparse-keymap)))
                               (set-keymap-parent map widget-field-keymap)
                               (define-key map
                                           (kbd "C-c C-c")
                                           #'denote-tree-edit-commit-changes)
                               (define-key map
                                           (kbd "C-c C-k")
                                           #'denote-tree-edit-abort-changes)
                               map)
                     (substring-no-properties
                      (alist-get el denote-tree-edit--current-note))))
     ((stringp el)
      (widget-insert el)))
    (widget-insert " "))
  (use-local-map denote-tree-edit-mode-map)
  (widget-setup))

(defun denote-tree-edit--dewidgetize-line ()
  "Destroy widgets in line."
  (goto-char denote-tree-edit--current-line)
  (let ((possible-widgets (mapcar #'widget-at
                                  (mapcar #'overlay-start
                                          (overlays-in (line-beginning-position)
                                                       (line-end-position))))))
    (dolist (el possible-widgets)
      (widget-delete el))
    (kill-region (+ (next-single-property-change denote-tree-edit--current-line
                                                 'button-data)
                    (length denote-tree-node))
                 (line-end-position))))

(defun denote-tree-edit--set-from-front-matter
    (front-matter-els func &optional any)
  "Iterate over FRONT-MATTER-ELS applying FUNC to it.
Restrict search of props to the current line.

FUNC takes two positional arguments START END and ANY, which if not
set defaults to currently iterated over element of FRONT-MATTER-ELS."
  (dolist (el front-matter-els)
    (when-let* ((match (denote-tree-edit--prop-match el))
                (start (prop-match-beginning match))
                (end (prop-match-end match))
                (thing (if any any el)))
      (funcall func start end thing))))

(defun denote-tree-edit--prop-match (el)
  "Match prop of denote-tree--type EL in current line.
If EL is not a symbol or EL is not in line return nil."
  (when (symbolp el)
    (goto-char (line-beginning-position))
    (with-restriction (line-beginning-position) (line-end-position)
      (text-property-search-forward 'denote-tree--type el t))))

(defun denote-tree-edit-commit-changes ()
  "Replace front matter of note with user inputed data.
Denote wont ask you to confirm it, this is final."
  (interactive)
  (let ((copy (copy-tree denote-tree-edit--current-note))
        (denote-rename-confirmations nil)
        (denote-save-buffers t)
        (denote-kill-buffers t))
    (save-excursion
      (goto-char denote-tree-edit--current-line)
      (setq copy
            (denote-tree-edit--fix-current-note
             (denote-tree-edit--save-from-widgets copy))))
    (apply #'denote-rename-file (mapcar #'cdr copy)))
  (denote-tree-edit--clean-up))


(defun denote-tree-edit--fix-current-note (copy)
  "De-listify keywords alist element in COPY."
  (let (filetype)
    (with-temp-buffer
      (insert-file-contents (alist-get 'file copy))
      (goto-char (point-min))
      (setq filetype (denote-tree--find-filetype (current-buffer))))
    (when (listp (cdr (assq 'keywords copy)))
      (setcdr (assq 'keywords copy)
              (funcall (plist-get filetype :keywords-value-reverse-function)
                       (cdr (assq 'keywords copy))))))
  copy)

(defun denote-tree-edit--save-from-widgets ()
  "Save values from fields into `denote-tree-edit--current-note'."
  (let ((possible-widgets (mapcar #'widget-at
                                  (mapcar #'overlay-start
                                          (overlays-in (line-beginning-position)
                                                       (line-end-position)))))
        (front-matter denote-tree-include-from-front-matter))
    (dolist (el possible-widgets)
      (when (symbolp (car front-matter))
        (let ((value (widget-value el)))
          (setcdr (assq (car front-matter) denote-tree-edit--current-note)
                  value)))
      (setq front-matter (cdr front-matter)))))

(defun denote-tree-edit-abort-changes ()
  "Restore the note from `denote-tree-edit--current-note'."
  (interactive)
  (denote-tree-edit--clean-up))

(defun denote-tree-edit--restore-line ()
  "Restore edited note to previous state."
  (let ((front-pos (+ (next-single-property-change denote-tree-edit--current-line
                                                   'button-data)
                      (length denote-tree-node))))
    (goto-char front-pos)
    (dolist (el denote-tree-include-from-front-matter)
      (if (symbolp el)
          (insert (alist-get el denote-tree-edit--current-note) " ")
        (insert el " ")))))

(defun denote-tree-edit--clean-up ()
  "Return the line to read-only state."
  (let ((inhibit-read-only t))
    (save-excursion
      (denote-tree-edit--dewidgetize-line)
      (denote-tree-edit--restore-line)
      (setq denote-tree-edit--current-line nil)
      (denote-tree-mode))))

(defun denote-tree-edit--save-match (start end type)
  "Save match to `denote-tree-edit--current-note'."
  (setcdr (assq type denote-tree-edit--current-note)
          (buffer-substring start end)))


(provide 'denote-tree-edit)
;;; denote-tree-edit.el ends here
