;;; denote-tree-test.el --- Test denote-tree -*- lexical-binding: nil -*-

(require 'denote-tree)
(require 'ert)

(defvar denote-tree-test-mock--denote-file-types-1
  '((org
     :title-key-regexp "o:"
     :identifier-key-regexp "b:"
     :keywords-key-regexp "c:"
     :signature-key-regexp "d:"
     :date-key-regexp "e:")
    (markdown-yaml
     :title-key-regexp "y:"
     :identifier-key-regexp "b:"
     :keywords-key-regexp "c:"
     :signature-key-regexp "d:"
     :date-key-regexp "e:")
    (markdown-toml
     :title-key-regexp "t:"
     :identifier-key-regexp "b:"
     :keywords-key-regexp "c:"
     :signature-key-regexp "d:"
     :date-key-regexp "e:")
    (text
     :title-key-regexp "x:"
     :identifier-key-regexp "b:"
     :keywords-key-regexp "c:"
     :signature-key-regexp "d:"
     :date-key-regexp "e:")))

(defvar denote-tree-test-mock--denote-file-types-2
  '((org
     :title-key-regexp "o:"
     :identifier-key-regexp "b:")
    (markdown-toml
     :title-key-regexp "t:"
     :identifier-key-regexp "b:")
    (text
     :title-key-regexp "x:"
     :identifier-key-regexp "b:")))

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
  (let ((denote-file-types denote-tree-test-mock--denote-file-types-1))
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

(ert-deftest denote-tree-test--collect-keywords ()
  "Tests for `denote-tree--collect-keywords'."
  (cl-letf (((symbol-function 'denote-tree--find-filetype)
             (lambda (_)
               (assq 'org denote-tree-test-mock--denote-file-types-1))))
    (with-temp-buffer
      (insert "o: foo\nb: bar\nc: baz\nd: foz\ne: fazboo\n")
      (should (equal-including-properties
               (denote-tree--collect-keywords (current-buffer)
                                   '(title
                                     identifier
                                     keywords
                                     signature
                                     date))
               `((date . ,(propertize "fazboo" 'denote-tree--type 'date))
                 (signature . ,(propertize "foz" 'denote-tree--type 'signature))
                 (keywords . ,(propertize "baz" 'denote-tree--type 'keywords))
                 (identifier . ,(propertize "bar" 'denote-tree--type 'identifier))
                 (title . ,(propertize "foo" 'denote-tree--type 'title))))))
    (with-temp-buffer
      (insert "o: foo\nb: bar\nd: foz\ne: fazboo\n")
      (should (equal-including-properties
               (denote-tree--collect-keywords (current-buffer)
                                   '(title
                                     identifier
                                     keywords
                                     signature
                                     date))
               `((date . ,(propertize "fazboo" 'denote-tree--type 'date))
                 (signature . ,(propertize "foz" 'denote-tree--type 'signature))
                 (keywords)
                 (identifier . ,(propertize "bar" 'denote-tree--type 'identifier))
                 (title . ,(propertize "foo" 'denote-tree--type 'title))))))
    (with-temp-buffer
      (should (equal-including-properties
               (denote-tree--collect-keywords (current-buffer)
                                   '())
               nil)))
    ;; possible extension point for future keywords
    (with-temp-buffer
      (should (equal-including-properties
               (denote-tree--collect-keywords (current-buffer)
                                   '(kazoo))
               '((kazoo)))))))

(ert-deftest denote-tree-test--build-full-filetype ()
  "Tests for `denote-tree--build-full-filetype'."
  (let ((type (assq 'org denote-tree-test-mock--denote-file-types-1)))
    (should (equal (denote-tree--build-full-filetype type)
                   '(org
                     :title-key-regexp "o:"
                     :identifier-key-regexp "b:"
                     :keywords-key-regexp "c:"
                     :signature-key-regexp "d:"
                     :date-key-regexp "e:"))))
  (let ((type (assq 'org denote-tree-test-mock--denote-file-types-2)))
    (should (equal (denote-tree--build-full-filetype type)
                   '(org
                     :title-key-regexp "o:"
                     :identifier-key-regexp "b:"
                     :date-key-regexp "^#\\+date\\s-*:"
                     :signature-key-regexp "^#\\+signature\\s-*:"
                     :identifier-key-regexp "^#\\+identifier\\s-*:")))))

(ert-deftest denote-tree-test--collect-links ()
  "Tests for `denote-tree--collect-links'."
  (cl-letf (((symbol-function 'denote-tree--open-link-maybe)
             (lambda (buffer)
               buffer)))
    (with-temp-buffer
      (insert "#+title: FOO
20231226T163250 20240119T164551 20240120T164558 20240121T164829 20240121T164914
20231227T163408 20231228T163736 20231229T164123 20240101T164316 20240117T164506")
      (goto-char (point-min))
      (should (equal (denote-tree--collect-links (current-buffer))
                     '("20231226T163250" "20240119T164551" "20240120T164558"
                       "20240121T164829" "20240121T164914" "20231227T163408"
                       "20231228T163736" "20231229T164123" "20240101T164316"
                       "20240117T164506"))))
    (with-temp-buffer
      (insert "#+title: FOO
#+identifier: 20231226T163250

20240119T164551 20240120T164558")
      (goto-char (point-min))
      (should (equal (denote-tree--collect-links (current-buffer))
                     '("20240119T164551" "20240120T164558"))))
    (with-temp-buffer
      (insert "#+title: FOO
#+identifier: 20231226T163250")
      (goto-char (point-min))
      (should (equal (denote-tree--collect-links (current-buffer))
                     nil)))
    (with-temp-buffer
      (insert "#+identifier: 20231226T163250")
      (goto-char (point-min))
      (should (equal (denote-tree--collect-links (current-buffer))
                     nil)))))


(provide 'denote-tree-test)
