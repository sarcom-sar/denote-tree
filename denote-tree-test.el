;;; denote-tree-test.el --- Test denote-tree -*- lexical-binding: nil -*-

(require 'denote-tree)
(require 'ert)

(ert-deftest denote-tree-test--default-props ()
  "Tests for `denote-tree--default-props'."
  (should (equal-including-properties
           (denote-tree--default-props "a" 'b)
           (propertize "a" 'denote-tree--type 'b)))
  (should (equal-including-properties
           (denote-tree--default-props "" 'b)
           (propertize "" 'denote-tree--type 'b)))
  (should (equal-including-properties
           (denote-tree--default-props "a" 'b)
           (propertize "a" 'denote-tree--type 'b)))
  (should (equal-including-properties
           (denote-tree--default-props "a" '(b c))
           (propertize "a" 'denote-tree--type '(b c)))))

(defmacro denote-tree-test--prepare-buffer-space
    (start-bufs after-bufs visited)
  "Prepare an environment for testing `denote-tree--clean-up'.

Argument START-BUFS - starting \"buffers\";
Argument AFTER-BUFS - what BUFFERS should be after the call;
Argument VISITED    - \"buffers\" to be cleaned up."
  `(let ((fake-buffers ,start-bufs)
         (denote-tree--visited-buffers ,visited)
         (denote-tree--cyclic-buffers '()))
     (cl-letf (((symbol-function 'kill-buffer)
                (lambda (thing)
                  (if (member thing fake-buffers)
                      (setq fake-buffers (remove thing fake-buffers))
                    (error "No buffer named %s" thing))))
               ((symbol-function 'get-buffer)
                (lambda (thing)
                  (member thing fake-buffers))))
       (denote-tree--clean-up)
       (should (equal fake-buffers ,after-bufs)))))

(ert-deftest denote-tree-test--clean-up ()
  "Tests for `denote-tree--clean-up'."
  (denote-tree-test--prepare-buffer-space
   '(a b c d e f) '(a b c d e f) '())
  (denote-tree-test--prepare-buffer-space
   '(a b c d e f) '(b d e f) '(c a))
  (denote-tree-test--prepare-buffer-space
   '(a b c d e f) '(a b c d e f) '(g)))

(ert-deftest denote-tree-test--collect-keywords-as-string ()
  "Tests for `denote-tree--collect-keywords-as-string'."
  (cl-letf (((symbol-function 'denote-tree--collect-keywords)
             ;; real functions returns in reverse
             (lambda (_ _)
               '((a . "a")
                 (b . "b")
                 (c . "c")))))
    (should (equal (denote-tree--collect-keywords-as-string '_ '_)
                   "c b a")))
  (cl-letf (((symbol-function 'denote-tree--collect-keywords)
             ;; real functions returns in reverse
             (lambda (_ _)
               '((a . "a")
                 (b)
                 (c . "c")))))
    (should (equal (denote-tree--collect-keywords-as-string '_ '_)
                   "c a")))
  (cl-letf (((symbol-function 'denote-tree--collect-keywords)
             ;; real functions returns in reverse
             (lambda (_ _)
               '((a)
                 (b)
                 (c . "c")))))
    (should (equal (denote-tree--collect-keywords-as-string '_ '_)
                   "c")))
  (cl-letf (((symbol-function 'denote-tree--collect-keywords)
             ;; real functions returns in reverse
             (lambda (_ _)
               '((a)
                 (b)
                 (c)))))
    (should (equal (denote-tree--collect-keywords-as-string '_ '_)
                   ""))))

(ert-deftest denote-tree-test--find-filetype ()
  "Tests for `denote-tree--find-filetype'."
  (let ((denote-file-types '((org
                              :title-key-regexp "o:")
                             (markdown-yaml
                              :title-key-regexp "y:")
                             (markdown-toml
                              :title-key-regexp "t:")
                             (text
                              :title-key-regexp "x:"))))
    (with-temp-buffer
      (insert "o: title")
      (should (equal (car (denote-tree--find-filetype (current-buffer)))
                     'org)))
    (with-temp-buffer
      (insert "y: title")
      (should (equal (car (denote-tree--find-filetype (current-buffer)))
                     'markdown-yaml)))
    (with-temp-buffer
      (insert "t: title")
      (should (equal (car (denote-tree--find-filetype (current-buffer)))
                     'markdown-toml)))
    (with-temp-buffer
      (insert "x: title")
      (should (equal (car (denote-tree--find-filetype (current-buffer)))
                     'text)))
    (with-temp-buffer
      (insert "p: title")
      (should (equal (car (denote-tree--find-filetype (current-buffer)))
                     nil)))
    (with-temp-buffer
      (insert "")
      (should (equal (car (denote-tree--find-filetype (current-buffer)))
                     nil)))))
(ert-deftest )

(provide 'denote-tree-test)
