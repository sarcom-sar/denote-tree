;;; denote-tree-edit.el --- Edit note with widgets -*- lexical-binding: t -*-

;; Copyright 2024, Sararin
;; Version: 0
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

;; Small extension package which allows you to edit notes in-buffer.

;;; Code:

(require 'denote)
(require 'wid-edit)
(require 'denote-tree)

(declare-function #'denote-tree-mode "./denote-tree.el")
(declare-function #'denote-tree--find-filetype "./denote-tree.el")
(declare-function #'denote-tree--get-prop "./denote-tree.el")


;;;; Variables

(defvar-local denote-tree-edit--current-note
    '((file) (title) (keywords) (signature) (date))
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
          (denote-get-path-by-id (denote-tree--get-prop 'button-data)))
  (save-excursion
    (let ((inhibit-read-only t))
      ;; save to denote-tree-edit--current-note from front-matter
      (with-temp-buffer
        (insert-file-contents (alist-get 'file denote-tree-edit--current-note))
        (goto-char (point-min))
        (let ((keywords-pairs
               (denote-tree--collect-keywords
                (current-buffer) '(title keywords signature date))))
          (dolist (el keywords-pairs)
            (let ((pair (assq (car el) denote-tree-edit--current-note)))
              (when (cdr el)
                (setcdr pair (cdr el)))))))
      ;; add info about neighbors
      (denote-tree-edit--set-from-tree
       denote-tree-node-description #'denote-tree-edit--save-match)
      (denote-tree-edit--widgetize-line))))

(defun denote-tree-edit-commit-changes ()
  "Replace front matter of note with user inputed data.
Denote wont ask you to confirm it, this is final."
  (interactive)
  (unwind-protect
      (progn
        (setq denote-tree-edit--current-note
              (nreverse
               (denote-tree-edit--save-from-widgets
                denote-tree-edit--current-note denote-tree-edit--current-line)))
        (let ((copy (denote-tree-edit--fix-current-note
                     (copy-tree denote-tree-edit--current-note)))
              (denote-rename-confirmations nil)
              (denote-save-buffers t))
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
  (let ((front-pos
         (denote-tree-edit--after-button denote-tree-edit--current-line)))
    (goto-char front-pos)
    (dolist (el denote-tree-node-description)
      (if (symbolp el)
          (insert (alist-get el denote-tree-edit--current-note) " ")
        (insert el " ")))))


;;;; Widget-related defuns

(defun denote-tree-edit--widgetize-line ()
  "Make line widgetized."
  (kill-region (denote-tree-edit--after-button denote-tree-edit--current-line)
               (line-end-position))
  (goto-char (line-end-position))
  (dolist (el denote-tree-node-description)
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
  (let ((possible-widgets
         (denote-tree-edit--widgets-in-line denote-tree-edit--current-line)))
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
  (let ((possible-widgets (nreverse (denote-tree-edit--widgets-in-line loc)))
        (front-matter denote-tree-node-description)
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
      (let ((new-pair (assq (car el) type-widget-alist))
            (old-pair (assq (car el) alist))
            props)
        (cond
         ;; if new pair doesn't have a value, push a hybrid
         ;; of key and old element
         ((and (null (cdr new-pair)) (cdr old-pair))
          (push (cons (car el) (cdr old-pair)) new-alist))
         ;; if new-pair has both value and key, push it
         (new-pair (push new-pair new-alist))
         ;; push old-pair if none of the above is true
         (t (push el new-alist)))))))

;;;; Helpers

(defun denote-tree-edit--after-button (pos)
  "Return position of prop \\='button-data in line POS or nil."
  (goto-char pos)
  (when-let* ((prop-pos (denote-tree-edit--next-prop-match 'button-data)))
    (+ prop-pos (length denote-tree-node))))

(defun denote-tree-edit--next-prop-match (type &optional el)
  "Match next prop of TYPE in current line and return it's position.

EL defaults to nil.  If TYPE is not in current line, or it doesn't match
EL, return nil."
  (when (symbolp type)
    (save-restriction
      (narrow-to-region (line-beginning-position) (line-end-position))
      (when-let ((pos (next-single-property-change (point) type)))
        ;; skip over prop of type, because otherwise -edit--set-from-tree
        ;; will run into same prop over and over
        ;; fall back on just pos if that prop is the last one in line
        (goto-char (or (next-single-property-change pos type) pos))
        (cond
         ((null el) pos)
         ((equal (get-text-property pos type) el) pos)
         (t nil))))))

(defun denote-tree-edit--save-match (start end type)
  "Save match of TYPE from START to END to `denote-tree-edit--current-note'."
  (setcdr (assq type denote-tree-edit--current-note)
          (buffer-substring start end)))

(defun denote-tree-edit--fix-current-note (copy)
  "Listify keywords value in COPY."
  (let (filetype)
    (with-temp-buffer
      (insert-file-contents (alist-get 'file copy))
      (goto-char (point-min))
      (setq filetype (cdr (denote-tree--find-filetype (current-buffer)))))
    (unless (listp (cdr (assq 'keywords copy)))
      (setcdr (assq 'keywords copy)
              (funcall (plist-get filetype :keywords-value-reverse-function)
                       (cdr (assq 'keywords copy))))))
  copy)

(defun denote-tree-edit--set-from-tree (front-matter-els func &optional any)
  "Iterate over FRONT-MATTER-ELS applying FUNC to it.
Restrict search of props to the current line.

FUNC takes two positional arguments START END and ANY, which if not
set defaults to currently iterated over element of FRONT-MATTER-ELS."
  (dolist (el front-matter-els)
    (when-let* ((start (denote-tree-edit--next-prop-match 'denote-tree--type el))
                (end (next-single-property-change start 'denote-tree--type))
                (thing (or any el)))
      (funcall func start end thing))))

(provide 'denote-tree-edit)
;;; denote-tree-edit.el ends here
