;;; denote-tree-link.el --- Visualize your notes as a tree -*- lexical-binding: t -*-

;; Copyright 2024, Sararin
;; URL: http://github.com/sarcom-sar/denote-tree.el

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

;; Provide a major mode for `org-capture'-like linking of files while using
;; denote-tree.

;;; Code:

(require 'denote-tree)

(declare-function denote-get-link-description "denote")
(declare-function denote-tree-redraw "denote-tree")

(defcustom denote-tree-link-insert-function #'denote-tree-link-insert-at-eof
  "Return the region at which the link is to be inserted.

The function takes no arguments and returns a pair of intergers.  The
range determines a buffer region in which text will be replace with a
link.  If the pair is the same integer, then perform the insertion in
place."
  :group 'denote-tree-link
  :type 'function)

(defvar denote-tree-link--plist '()
  "Plist of elements necessary while linking.

It should have three values :node-from, :node-to and :window-config.")

(defvar denote-tree-link-mode-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "C-c C-c" #'denote-tree-link-finalize)
    (keymap-set map "C-c C-k" #'denote-tree-link-kill)
    map)
  "Keymap for `denote-tree-link-mode', a minor mode.
Use this map to set additional keybindings for when denote-tree is used
for linking notes.")

(define-minor-mode denote-tree-link
  "Minor mode for inserting a link in a note."
  :lighter " Link"
  :interactive nil
  (if (and arg (= 1 arg))
      (setq-local
       header-line-format
       (substitute-command-keys
        "\\<denote-tree-link-mode-map> Add link buffer.  Finish \
`\\[denote-tree-link-finalize]', abort `\\[denote-tree-link-kill]'."))
    (setq-local header-line-format nil)))

(defun denote-tree-link-finalize ()
  "Insert a link between point and mark in the note buffer.

Restore window configuration."
  (interactive)
  (denote-tree-link--do-the-link
   (point) (or (mark) (point)) (plist-get denote-tree-link--plist :node-from))
  (denote-tree-link -1)
  (set-window-configuration (plist-get denote-tree-link--plist :window-config))
  (with-current-buffer (plist-get denote-tree-link--plist :denote-tree-buffer)
    (denote-tree-redraw)))

(defun denote-tree-link--do-the-link (pos mark link-this-file)
  "Link LINK-THIS-FILE in current buffer at region from POS to MARK.

If POS and MARK are the same, or MARK is not set, do it at POS."
  (or mark (setq mark pos))
  (goto-char pos)
  (denote-link
   link-this-file
   (car (save-excursion (denote-tree--find-filetype (current-buffer))))
   (if (eql pos mark)
       (if (boundp 'denote-link-description-format)
           ;; denote > 3.1.0
           (denote-get-link-description link-this-file)
         ;; denote <= 3.1.0
         (funcall denote-link-description-function link-this-file))
     (prog1 (buffer-substring pos mark)
       (delete-region pos mark))))
  (write-file (buffer-file-name) nil))

(defun denote-tree-link-kill ()
  "Abort the linking, restore window configuration."
  (interactive)
  (denote-tree-link -1)
  (set-window-configuration (plist-get :window-config denote-tree-link--plist)))

(defun denote-tree-link--helper (link-this to-this)
  "Link note LINK-THIS to note TO-THIS.

If `denote-tree-link-insert-function' is set, do it automatically.
Otherwise prompt the user for manual interaction.  This function sets
buffer-local `denote-tree-link--plist' in order to restore user window
configuration."
  (let ((to-this-buff (find-file-noselect to-this))
        (main-buff (current-buffer)))
    (with-current-buffer to-this-buff
      (setq-local denote-tree-link--plist
                  `(:node-from ,link-this
                    :node-to ,to-this
                    :denote-tree-buffer ,main-buff
                    :window-config ,(current-window-configuration))))
    (cond
     (denote-tree-link-insert-function
      (with-current-buffer to-this-buff
        (seq-let (pos mark) (funcall denote-tree-link-insert-function)
          (denote-tree-link--do-the-link
           pos mark link-this)))
      (denote-tree-redraw))
     (t
      (pop-to-buffer to-this-buff)
      (denote-tree-link 1)))))

(defun denote-tree-link-insert-at-eof ()
  "Return a pair at the end of the file."
  (list (point-max) (point-max)))

(defun denote-tree-link-insert-after-front-matter ()
  "Return the position after the front-matter."
  (when-let* ((front-matter
               (symbol-value
                (plist-get (cdr (denote-tree--find-filetype (current-buffer)))
                           :front-matter))))
    (save-excursion
      (goto-char (point-min))
      (forward-line
       (length (split-string front-matter "\n")))
      (list (point) (point)))))

(provide 'denote-tree-link)
;;; denote-tree-link.el ends here
