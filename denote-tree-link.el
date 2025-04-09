;;; denote-tree-link.el --- Visualize your notes as a tree -*- lexical-binding: t -*-

(require 'denote-tree)

(declare-function denote-get-link-description "denote")

;;; Code

(defcustom denote-tree-link-insert-function #'denote-tree-link-insert-at-eof
  "Return the region at which the link is to be inserted.

The function takes no arguments and returns a pair of intergers.  The
range determines a buffer region in which text will be replace with a
link.  If the pair is the same integer, then perform the insertion in
place."
  :group 'denote-tree
  :type 'function)

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
  (setq-local
   header-line-format
   (substitute-command-keys
    "\\<denote-tree-link-mode-map> Add link buffer.  Finish \
`\\[denote-tree-link-finalize]', abort `\\[denote-tree-link-kill]'.")))

(defun denote-tree-link-finalize ()
  "Insert a link between point and mark in the note buffer.

Restore window configuration.")

(defun denote-tree-link-kill ()
  "Abort the linking, restore window configuration.")

(defun denote-tree-link--helper (node-from node-to)
  (cond
   (denote-tree-link-insert-function
    (with-current-buffer (find-file-noselect node-to)
      (seq-let (pos mark) (funcall denote-tree-link-insert-function)
        (let ((boundaries-of-link '()))
          (goto-char (car pos))
          (denote-link
           node-from
           (denote-tree--find-filetype (current-buffer))
           (if (eql pos mark)
               (if (boundp 'denote-link-description-format)
                   ;; denote > 3.1.0
                   (denote-get-link-description node-from)
                 ;; denote <= 3.1.0
                 (funcall denote-link-description-function node-from))
             (prog1 (buffer-substring pos mark)
               (delete-region pos mark))))))))
   (t
    (ignore))))

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
       (length (string-split front-matter "\n")))
      (list (point) (point)))))

(provide 'denote-tree-link)
