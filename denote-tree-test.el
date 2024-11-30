;;; denote-tree-test.el --- Test denote-tree -*- lexical-binding: nil -*-

(require 'denote-tree)
(require 'ert)

(defvar denote-tree-test-mock--denote-file-types-1
  '((org
     :title-key-regexp "org-title:"
     :identifier-key-regexp "org-identifier:"
     :keywords-key-regexp "org-keywords:"
     :signature-key-regexp "org-signature:"
     :date-key-regexp "org-date:")
    (markdown-yaml
     :title-key-regexp "yaml-title:"
     :identifier-key-regexp "yaml-identifier:"
     :keywords-key-regexp "yaml-keywords:"
     :signature-key-regexp "yaml-signature:"
     :date-key-regexp "yaml-date:")
    (markdown-toml
     :title-key-regexp "toml-title:"
     :identifier-key-regexp "toml-identifier:"
     :keywords-key-regexp "toml-keywords:"
     :signature-key-regexp "toml-signature:"
     :date-key-regexp "toml-date:")
    (text
     :title-key-regexp "text-title:"
     :identifier-key-regexp "text-identifier:"
     :keywords-key-regexp "text-keywords:"
     :signature-key-regexp "text-signature:"
     :date-key-regexp "text-date:")))

(defvar denote-tree-test-mock--denote-file-types-2
  '((org
     :title-key-regexp "org-title:"
     :identifier-key-regexp "org-identifier:")
    (markdown-yaml
     :title-key-regexp "yaml-title:"
     :identifier-key-regexp "yaml-identifier:")
    (markdown-toml
     :title-key-regexp "toml-title:"
     :identifier-key-regexp "toml-identifier:")
    (text
     :title-key-regexp "text-title:"
     :identifier-key-regexp "text-identifier:")))

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
      (insert "org-title: title")
      (should (equal (car (denote-tree--find-filetype (current-buffer)))
                     'org)))
    (with-temp-buffer
      (insert "org-identifier: identifier")
      (should (equal (car (denote-tree--find-filetype (current-buffer)))
                     'org)))
    (with-temp-buffer
      (insert "org-keywords: keywords")
      (should (equal (car (denote-tree--find-filetype (current-buffer)))
                     'org)))
    (with-temp-buffer
      (insert "yaml-title: title")
      (should (equal (car (denote-tree--find-filetype (current-buffer)))
                     'markdown-yaml)))
    (with-temp-buffer
      (insert "yaml-identifier: identifier")
      (should (equal (car (denote-tree--find-filetype (current-buffer)))
                     'markdown-yaml)))
    (with-temp-buffer
      (insert "yaml-keywords: keywords")
      (should (equal (car (denote-tree--find-filetype (current-buffer)))
                     'markdown-yaml)))
    (with-temp-buffer
      (insert "toml-title: title")
      (should (equal (car (denote-tree--find-filetype (current-buffer)))
                     'markdown-toml)))
    (with-temp-buffer
      (insert "toml-identifier: identifier")
      (should (equal (car (denote-tree--find-filetype (current-buffer)))
                     'markdown-toml)))
    (with-temp-buffer
      (insert "toml-keywords: keywords")
      (should (equal (car (denote-tree--find-filetype (current-buffer)))
                     'markdown-toml)))
    (with-temp-buffer
      (insert "text-title: title")
      (should (equal (car (denote-tree--find-filetype (current-buffer)))
                     'text)))
    (with-temp-buffer
      (insert "text-identifier: identifier")
      (should (equal (car (denote-tree--find-filetype (current-buffer)))
                     'text)))
    (with-temp-buffer
      (insert "text-keywords: keywords")
      (should (equal (car (denote-tree--find-filetype (current-buffer)))
                     'text)))
    (with-temp-buffer
      (insert "p-title: title")
      (should (equal (car (denote-tree--find-filetype (current-buffer)))
                     nil)))
    (with-temp-buffer
      (insert "p-identifier: identifier")
      (should (equal (car (denote-tree--find-filetype (current-buffer)))
                     nil)))
    (with-temp-buffer
      (insert "p-keywords: keywords")
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
      (insert "org-title: foo
org-identifier: bar
org-keywords: baz
org-signature: foz
org-date: fazboo
")
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
      (insert "org-title: foo
org-identifier: bar
org-signature: foz
org-date: fazboo
")
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
    (let ((denote-tree-test-mock--denote-file-types-1 (copy-tree denote-tree-test-mock--denote-file-types-2)))
      (setf (alist-get 'org denote-tree-test-mock--denote-file-types-1)
            (append (alist-get 'org denote-tree-test-mock--denote-file-types-1)
                    '(:kazoo-key-regexp "org-kazoo:")))
      (with-temp-buffer
        (insert "org-kazoo: PRRT")
        (should (equal-including-properties
                 (denote-tree--collect-keywords (current-buffer)
                                     '(kazoo))
                 `((kazoo . ,(propertize "PRRT" 'denote-tree--type 'kazoo)))))))))

(ert-deftest denote-tree-test--build-full-filetype ()
  "Tests for `denote-tree--build-full-filetype'."
  (let ((type (assq 'org denote-tree-test-mock--denote-file-types-1)))
    (should (equal (denote-tree--build-full-filetype type)
                   '(org
                     :title-key-regexp "org-title:"
                     :identifier-key-regexp "org-identifier:"
                     :keywords-key-regexp "org-keywords:"
                     :signature-key-regexp "org-signature:"
                     :date-key-regexp "org-date:"))))
  (let ((type (assq 'org denote-tree-test-mock--denote-file-types-2)))
    (should (equal (denote-tree--build-full-filetype type)
                   '(org
                     :title-key-regexp "org-title:"
                     :identifier-key-regexp "org-identifier:"
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

(ert-deftest denote-tree-test--compare ()
  "Tests for `denote-tree--compare'."
  (should (eq (denote-tree--compare :title-key-regexp 'title)
              :title-key-regexp))
  (should (eq (denote-tree--compare 'title-key-regexp 'title)
              nil))
  (should (eq (denote-tree--compare :date-format 'date)
              nil))
  (should (eq (denote-tree--compare :foo-bar-baz-regexp 'foo)
              :foo-bar-baz-regexp))
  (should (eq (denote-tree--compare :foobar-regexp 'foobar)
              :foobar-regexp))
  (should (eq (denote-tree--compare :foo-regexp nil)
              nil))
  (should (eq (denote-tree--compare nil 'bar)
              nil)))


(provide 'denote-tree-test)
