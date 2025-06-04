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


;;;; Customization and vars

(defgroup denote-tree-link ()
  "Functionalities for linking in denote-tree."
  :group 'convenience)

(defcustom denote-tree-link-insert-function #'denote-tree-link-insert-at-eof
  "Return the region at which the link is to be inserted.

The function takes no arguments and returns a pair of intergers.  The
range determines a buffer region in which text will be replace with a
link.  If the pair is the same integer, then perform the insertion in
place."
  :type 'function)

(defcustom denote-tree-link-after-link-insertion-hooks '()
  "List of hooks to run after the link was inserted.

This variable is primarily used to modify the text around inserted link.
Before this hook is ran the buffer is narrowed to just the link itself.
`point-min' points to the area behind the link, while the `point-max'
points to the area after the link."
  :type 'hook)

(defvar-local denote-tree-link--plist '()
  "Plist of elements necessary while linking.

It should consist of:
- `:link-this', the note that will be linked;
- `:to-this', the note to which it will be linked;
- `:denote-tree-buffer', current denote-tree buffer;
- `:window-config', window config to restore.")


;;;; Mode definition

(defvar denote-tree-link-mode-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "C-c C-c" #'denote-tree-link-finalize)
    (keymap-set map "C-c C-k" #'denote-tree-link-kill)
    map)
  "Keymap for `denote-tree-link-mode', a minor mode.
Use this map to set additional keybindings for when denote-tree is used
for linking notes.")

(define-minor-mode denote-tree-link-mode
  "Minor mode for inserting a link in a note.

\\{denote-tree-link-mode-map\}"
  :lighter " Link"
  :interactive nil
  (if (and arg (= 1 arg))
      (setq-local
       header-line-format
       (substitute-command-keys
        "\\<denote-tree-link-mode-map> Add link buffer.  Finish \
`\\[denote-tree-link-finalize]', abort `\\[denote-tree-link-kill]'."))
    (setq-local header-line-format nil)))


;;;; Interactive functions

(defun denote-tree-link-nodes (from-mark to-point)
  "Link node at FROM-MARK to TO-POINT.

If `denote-tree-link-insert-function' is set, then perform this based on
function's return value.  Otherwise open a TO-POINT file and let the
user decide where in TO-POINT node the link to FROM-MARK should be set."
  (interactive (list (mark) (point)))
  (when-let* ((node-from (denote-get-path-by-id
                          (denote-tree--get-prop 'button-data from-mark)))
              (node-to (denote-get-path-by-id
                        (denote-tree--get-prop 'button-data to-point))))
    (when (equal node-from node-to)
      (user-error "Trying to link a file to itself"))
    (denote-tree-link--helper node-from node-to)))

(defun denote-tree-link-unlink-node (pos)
  "Unlink the node at POS from it's parent.

Leave only the text that was there before the linking.  If the link
contains only an ID, delete entire line sans the newline."
  (interactive "d")
  (when-let* (;; node id linked with the file
              (real-node (denote-tree--get-prop 'button-data pos))
              ;; the gensyme'd id to make it unique
              (node (get-text-property pos 'denote-tree--identifier))
              (parent-pos (denote-tree--nested-value
                           denote-tree--tree-alist node :parent :pos))
              (parent-buff (find-file-noselect
                            (denote-get-path-by-id
                             (denote-tree--get-prop 'button-data parent-pos)))))
    (denote-tree-link--unlink real-node parent-buff)))

(defun denote-tree-link-spawn-child-node (pos)
  "Create new child node with node at POS as a parent."
  (interactive "d")
  (when-let* ((denote-save-buffers t)
              (buff (current-buffer))
              (node (denote-get-path-by-id
                     (denote-tree--get-prop 'button-data pos)))
              (child (call-interactively #'denote)))
    (with-current-buffer buff
      (denote-tree-link--helper child node))))

(defun denote-tree-link-finalize (&optional stay-with-capture)
  "Insert a link between point and mark in the note buffer.

With prefix argument STAY-WITH-CAPTURE, jump to the location of the
captured item after finalizing."
  (interactive "P")
  (denote-tree-link--do-the-link
   (point) (or (mark) (point)) (plist-get denote-tree-link--plist :link-this))
  (denote-tree-link-mode -1)
  (let ((marker (set-marker (make-marker) (point))))
    (set-window-configuration
     (plist-get denote-tree-link--plist :window-config))
    (with-current-buffer (plist-get denote-tree-link--plist :denote-tree-buffer)
      (denote-tree-redraw))
    (when stay-with-capture
      (when (or (> marker (point-max)) (< marker (point-min)))
	      (widen))
      (pop-to-buffer-same-window (marker-buffer marker))
      (goto-char marker))))

(defun denote-tree-link-kill ()
  "Abort the linking, restore window configuration.

Do not actually kill the buffer itself, since the user might wish to
examine it."
  (interactive)
  (denote-tree-link-mode -1)
  (let ((curr-buff (current-buffer)))
    (set-window-configuration
     (plist-get denote-tree-link--plist :window-config))
    (bury-buffer curr-buff)))


;;;; Module entry point

(defun denote-tree-link--helper (link-this to-this)
  "Link note LINK-THIS to note TO-THIS.

If `denote-tree-link-insert-function' is set, do it
automatically. Otherwise prompt the user for manual interaction.  This
function sets buffer-local `denote-tree-link--plist' in order to
\"smuggle\" the state beyond the function call."
  (let ((to-this-buff (find-file-noselect to-this))
        (main-buff (current-buffer)))
    (with-current-buffer to-this-buff
      (setq denote-tree-link--plist
            `(
              :link-this ,link-this
              :to-this ,to-this
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
      (denote-tree-link-mode 1)))))

(defun denote-tree-link--do-the-link (pos mark link-this-file)
  "Link LINK-THIS-FILE in current buffer at region from POS to MARK.

If POS and MARK are the same, do it at POS.  During the insertion buffer
is narrowed to region between POS and MARK."
  (goto-char pos)
  (save-restriction
    (widen)
    (narrow-to-region pos mark)
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
    (run-hooks 'denote-tree-link-after-link-insertion-hooks))
  (write-file (buffer-file-name) nil))

(defun denote-tree-link--unlink (node parent)
  "Unlink NODE in PARENT to just text."
  (with-current-buffer parent
    (goto-char (point-min))
    (when-let* ((file-type (denote-tree--find-filetype parent))
                (link-in-context
                 (thread-last
                   (plist-get (cdr file-type) :link-in-context-regexp)
                   symbol-value))
                (link-range
                 (thread-last
                   (plist-get (cdr file-type) :link)
                   symbol-value
                   (denote-tree-link--range node ".*?")))
                (link-string
                 (buffer-substring-no-properties
                  (car link-range) (cadr link-range))))
      (save-match-data
        (string-match link-in-context link-string)
        (goto-char (car link-range))
        (delete-region (car link-range) (cadr link-range))
        (when (match-beginning 2)
          (insert (substring link-string (match-beginning 2) (match-end 2))))))
    (write-file (buffer-file-name) nil))
  (denote-tree-redraw))

(defun denote-tree-link--range (node description link)
  "Find NODE with DESCRIPTION in LINK style.

If none present, return nil."
  (let ((regex-to-search
         (concat "\\("
                 (regexp-quote link)
                 "\\)"))
        (id-only-regex
         (concat "\\("
                 (regexp-quote denote-id-only-link-format)
                 "\\)")))
    (save-match-data
      (if (or (re-search-forward
               (format regex-to-search node description) nil t)
              (re-search-forward
               (format id-only-regex node) nil t))
          (list (match-beginning 0) (match-end 0))
        (error "No valid target for unlinking in node %s" node)))))


;;;; Default functions for denote-tree-link-insert-function

(defun denote-tree-link-insert-at-eof ()
  "Return a pair at the end of the file."
  (list (point-max) (point-max)))

(defun denote-tree-link-insert-after-front-matter ()
  "Return the position after the front-matter.

If it can not be determined, default to EoF."
  (if-let* ((front-matter
             (symbol-value
              (plist-get (cdr (denote-tree--find-filetype (current-buffer)))
                         :front-matter))))
    (save-excursion
      (goto-char (point-min))
      (forward-line
       (length (split-string front-matter "\n")))
      (list (point) (point)))
    (denote-tree-link-insert-at-eof)))

(provide 'denote-tree-link)
;;; denote-tree-link.el ends here
