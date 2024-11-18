;;; denote-tree-edit.el --- Edit note with widgets -*- lexical-binding: t -*-

;; Copyright 2024, Sararin

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

;;; Code:

(eval-when-compile
  (require 'denote)
  (require 'wid-edit)
  (require 'denote-tree))

(declare-function #'denote-tree-mode "./denote-tree.el")
(declare-function #'denote-tree--find-filetype "./denote-tree.el")


;;;; Variables

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


;;;; Mode definition and user-facing defuns

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

(defun denote-tree-edit-commit-changes ()
  "Replace front matter of note with user inputed data.
Denote wont ask you to confirm it, this is final."
  (interactive)
  (unwind-protect
      (progn
        (setq denote-tree-edit--current-note
              (denote-tree-edit--save-from-widgets denote-tree-edit--current-note
                                                   denote-tree-edit--current-line))
        (let ((copy (denote-tree-edit--fix-current-note
                     (copy-tree denote-tree-edit--current-note)))
              (denote-rename-confirmations nil)
              (denote-save-buffers t)
              (denote-kill-buffers t))
          (apply #'denote-rename-file (mapcar #'cdr copy))))
    (denote-tree-edit--clean-up)))

(defun denote-tree-edit-abort-changes ()
  "Restore the note from `denote-tree-edit--current-note'."
  (interactive)
  (denote-tree-edit--clean-up))


;;;; Clean-up and utilities

(defun denote-tree-edit--clean-up ()
  "Return the line to read-only state."
  (let ((inhibit-read-only t))
    (save-excursion
      (denote-tree-edit--dewidgetize-line)
      (denote-tree-edit--restore-line)
      (setq denote-tree-edit--current-line nil)
      (denote-tree-mode)
      (setq denote-tree--buffer-name (buffer-name)))))

(defun denote-tree-edit--restore-line ()
  "Restore edited note to previous state."
  (let ((front-pos (denote-tree-edit--after-button
                    denote-tree-edit--current-line)))
    (goto-char front-pos)
    (dolist (el denote-tree-include-from-front-matter)
      (if (symbolp el)
          (insert (alist-get el denote-tree-edit--current-note) " ")
        (insert el " ")))))


;;;; Widget-related defuns

(defun denote-tree-edit--widgetize-line ()
  "Make line widgetized."
  (kill-region (denote-tree-edit--after-button denote-tree-edit--current-line)
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
  (let ((possible-widgets (denote-tree-edit--widgets-in-line
                           denote-tree-edit--current-line)))
    (dolist (el possible-widgets)
      (widget-delete el))
    (kill-region (denote-tree-edit--after-button denote-tree-edit--current-line)
                 (line-end-position))))

(defun denote-tree-edit--widgets-in-line (loc)
  "Get all widgets from LOC."
  (goto-char loc)
  (let ((potential-widgets
         (mapcar #'widget-at
                 (mapcar #'overlay-start
                         (overlays-in (line-beginning-position)
                                      (line-end-position)))))
        lst)
    (dolist (el potential-widgets lst)
      (unless (null el)
        (push el lst)))))

(defun denote-tree-edit--construct-type-widget-alist (loc)
  "Construct an alist of (type . widget) starting from LOC."
  (let ((possible-widgets (denote-tree-edit--widgets-in-line loc))
        (front-matter denote-tree-include-from-front-matter)
        new-alist)
    (dolist (el front-matter new-alist)
      (when (symbolp el)
        (push (cons el (widget-value (car possible-widgets))) new-alist)
        (setq possible-widgets (cdr possible-widgets))))))

(defun denote-tree-edit--save-from-widgets (alist loc)
  "Save values from widgets in line LOC into ALIST."
  (let ((type-widget-alist (denote-tree-edit--construct-type-widget-alist loc))
        new-alist)
    (dolist (el alist new-alist)
      (if (assq (car el) type-widget-alist)
          (push (assq (car el) type-widget-alist) new-alist)
        (push el new-alist)))))

;;;; Helpers

(defun denote-tree-edit--after-button (pos)
  "Return position of prop \='button-data in line POS or nil."
  (goto-char pos)
  (+ (prop-match-end (denote-tree-edit--prop-match 'button-data nil))
     (length denote-tree-node)))

(defun denote-tree-edit--prop-match (type el)
  "Match prop of TYPE equal to EL in current line.
If TYPE or EL are not symbols or EL is not in line return nil."
  (when (or (symbolp el)
            (symbolp type))
    (goto-char (line-beginning-position))
    (save-restriction
      (narrow-to-region (line-beginning-position) (line-end-position))
      (text-property-search-forward type el t))))

(defun denote-tree-edit--save-match (start end type)
  "Save match to `denote-tree-edit--current-note'."
  (setcdr (assq type denote-tree-edit--current-note)
          (buffer-substring start end)))

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
  (nreverse copy))

(defun denote-tree-edit--set-from-front-matter
    (front-matter-els func &optional any)
  "Iterate over FRONT-MATTER-ELS applying FUNC to it.
Restrict search of props to the current line.

FUNC takes two positional arguments START END and ANY, which if not
set defaults to currently iterated over element of FRONT-MATTER-ELS."
  (dolist (el front-matter-els)
    (when-let* ((match (denote-tree-edit--prop-match 'denote-tree--type el))
                (start (prop-match-beginning match))
                (end (prop-match-end match))
                (thing (if any any el)))
      (funcall func start end thing))))

(provide 'denote-tree-edit)
;;; denote-tree-edit.el ends here
