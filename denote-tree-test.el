;;; denote-tree-test.el --- Test denote-tree -*- lexical-binding: t -*-

(require 'denote-tree)
(require 'ert)

;;; Code:

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
  "Tests for `denote-tree--default-props'.

`denote-tree--default-props' should return a string FOO with prop
'denote-tree--type BAR."
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

(defmacro denote-tree-test--prepare-buffer-space (start-bufs after-bufs visited)
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
                (lambda (thing) (member thing fake-buffers))))
       (denote-tree--clean-up)
       (should (equal fake-buffers ,after-bufs)))))

(ert-deftest denote-tree-test--clean-up ()
  "Tests for `denote-tree--clean-up'.

After `denote-tree--clean-up' state of START-BUFS \"buffer list\" should be
AFTER-BUFS. The VISITED buffers are the ones to disappear."
  (denote-tree-test--prepare-buffer-space '(a b c d e f) '(a b c d e f) '())
  (denote-tree-test--prepare-buffer-space '(a b c d e f) '(b d e f) '(c a))
  (denote-tree-test--prepare-buffer-space '(a b c d e f) '(a b c d e f) '(g)))

(ert-deftest denote-tree-test--collect-keywords-as-string ()
  "Tests for `denote-tree--collect-keywords-as-string'.

Collect arbitrary number of keywords and return them
as one string."
  (cl-letf (((symbol-function 'denote-tree--collect-keywords)
             (lambda (_ _) '((a . "a") (b . "b") (c . "c")))))
    (should (equal (denote-tree--collect-keywords-as-string '_ '_) "a b c")))
  (cl-letf (((symbol-function 'denote-tree--collect-keywords)
             (lambda (_ _) '((a . "a") (b) (c . "c")))))
    (should (equal (denote-tree--collect-keywords-as-string '_ '_) "a c")))
  (cl-letf (((symbol-function 'denote-tree--collect-keywords)
             (lambda (_ _) '((a) (b) (c . "c")))))
    (should (equal (denote-tree--collect-keywords-as-string '_ '_) "c")))
  (cl-letf (((symbol-function 'denote-tree--collect-keywords)
             (lambda (_ _) '((a) (b) (c)))))
    (should (equal (denote-tree--collect-keywords-as-string '_ '_) "")))
  (cl-letf (((symbol-function 'denote-tree--collect-keywords)
             (lambda (_ _) nil)))
    (should (equal (denote-tree--collect-keywords-as-string '_ '_) ""))))

(ert-deftest denote-tree-test--find-filetype ()
  "Tests for `denote-tree--find-filetype'.

`denote-tree--find-filetype' searches for any regex within the buffer that
allows to classify the type of front matter denote is dealing with."
  (let ((denote-tree--extended-filetype
         denote-tree-test-mock--denote-file-types-1))
    (with-temp-buffer
      (insert "org-title: title")
      (should (equal (car (denote-tree--find-filetype (current-buffer))) 'org)))
    (with-temp-buffer
      (insert "org-identifier: identifier")
      (should (equal (car (denote-tree--find-filetype (current-buffer))) 'org)))
    (with-temp-buffer
      (insert "org-keywords: keywords")
      (should (equal (car (denote-tree--find-filetype (current-buffer))) 'org)))
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
      (should-not (denote-tree--find-filetype (current-buffer))))
    (with-temp-buffer
      (insert "p-identifier: identifier")
      (should-not (denote-tree--find-filetype (current-buffer))))
    (with-temp-buffer
      (insert "p-keywords: keywords")
      (should-not (denote-tree--find-filetype (current-buffer))))
    (with-temp-buffer
      (should-not (denote-tree--find-filetype (current-buffer))))))

(ert-deftest denote-tree-test--collect-keywords ()
  "Tests for `denote-tree--collect-keywords'."
  (cl-letf (((symbol-function 'denote-tree--find-filetype)
             (lambda (_)
               (assq 'org denote-tree-test-mock--denote-file-types-1))))
    (with-temp-buffer
      (insert
       "org-title: foo\n"
       "org-identifier: bar\n"
       "org-keywords: baz\n"
       "org-signature: foz\n"
       "org-date: fazboo")
      (should
       (equal-including-properties
        (denote-tree--collect-keywords
         (current-buffer) '(title identifier keywords signature date))
        `((title . ,(propertize "foo" 'denote-tree--type 'title))
          (identifier . ,(propertize "bar" 'denote-tree--type 'identifier))
          (keywords . ,(propertize "baz" 'denote-tree--type 'keywords))
          (signature . ,(propertize "foz" 'denote-tree--type 'signature))
          (date . ,(propertize "fazboo" 'denote-tree--type 'date))))))
    (with-temp-buffer
      (insert
       "org-title: foo\n"
       "org-identifier: bar\n"
       "org-signature: foz\n"
       "org-date: fazboo")
      (should
       (equal-including-properties
        (denote-tree--collect-keywords
         (current-buffer) '(title identifier keywords signature date))
        `((title . ,(propertize "foo" 'denote-tree--type 'title))
          (identifier . ,(propertize "bar" 'denote-tree--type 'identifier))
          (keywords)
          (signature . ,(propertize "foz" 'denote-tree--type 'signature))
          (date . ,(propertize "fazboo" 'denote-tree--type 'date))))))
    (with-temp-buffer
      (should-not (denote-tree--collect-keywords (current-buffer) '())))
    (with-temp-buffer
      (should
       (equal
        (denote-tree--collect-keywords
         (current-buffer) '(title identifier))
        '((title) (identifier)))))
    ;; possible extension point for future keywords
    (let ((denote-tree-test-mock--denote-file-types-1
           (copy-tree denote-tree-test-mock--denote-file-types-2)))
      (setf (alist-get 'org denote-tree-test-mock--denote-file-types-1)
            (append
             (alist-get 'org denote-tree-test-mock--denote-file-types-1)
             '(:kazoo-key-regexp "org-kazoo:")))
      (with-temp-buffer
        (insert "org-kazoo: PRRT")
        (should
         (equal-including-properties
          (denote-tree--collect-keywords (current-buffer) '(kazoo))
          `((kazoo . ,(propertize "PRRT" 'denote-tree--type 'kazoo)))))))))

(ert-deftest denote-tree-test--build-extended-filetype ()
  "Tests for `denote-tree--build-extended-filetype'."
  (let ((base denote-tree-test-mock--denote-file-types-1)
        (add '()))
    (should (equal
             (assq 'org (denote-tree--build-extended-filetype base add))
             '(org :title-key-regexp "org-title:"
                   :identifier-key-regexp "org-identifier:"
                   :keywords-key-regexp "org-keywords:"
                   :signature-key-regexp "org-signature:"
                   :date-key-regexp "org-date:"))))
  (let ((base denote-tree-test-mock--denote-file-types-1)
        (add '((:foo-key-regexp org "org-foo:"))))
    (should
     (equal (assq 'org (denote-tree--build-extended-filetype base add))
            '(org :title-key-regexp "org-title:"
                  :identifier-key-regexp "org-identifier:"
                  :keywords-key-regexp "org-keywords:"
                  :signature-key-regexp "org-signature:"
                  :date-key-regexp "org-date:"
                  :foo-key-regexp "org-foo:")))
    (should
     (equal (assq 'text (denote-tree--build-extended-filetype base add))
            '(text :title-key-regexp "text-title:"
                   :identifier-key-regexp "text-identifier:"
                   :keywords-key-regexp "text-keywords:"
                   :signature-key-regexp "text-signature:"
                   :date-key-regexp "text-date:"
                   :foo-key-regexp nil))))
  (let ((base denote-tree-test-mock--denote-file-types-2)
        (add
         '((:keywords-key-regexp org "org-keywords:")
           (:signature-key-regexp org "org-signature:")
           (:date-key-regexp org "org-date:"))))
    (should
     (equal (assq 'org (denote-tree--build-extended-filetype base add))
            '(org :title-key-regexp "org-title:"
                  :identifier-key-regexp "org-identifier:"
                  :keywords-key-regexp "org-keywords:"
                  :signature-key-regexp "org-signature:"
                  :date-key-regexp "org-date:")))))


(ert-deftest denote-tree-test--collect-links ()
  "Tests for `denote-tree--collect-links'.

`denote-tree--collect-links' should NOT collect current buffer's id."
  (cl-letf (((symbol-function 'denote-tree--open-link-maybe)
             (lambda (buffer) buffer)))
    (let ((denote-tree--extended-filetype
           (denote-tree--build-extended-filetype
            denote-file-types denote-tree-extend-filetype-with)))
      (with-temp-buffer
        (insert
         "#+title: FOO\n"
         "20231226T163250 20240119T164551 20240120T164558\n"
         "20240121T164829 20240121T164914 20231227T163408\n"
         "20231228T163736 20231229T164123 20240101T164316\n"
         "20240117T164506")
        (goto-char (point-min))
        (should
         (equal (denote-tree--collect-links (buffer-name (current-buffer)))
                '(20231226T163250
                  20240119T164551
                  20240120T164558
                  20240121T164829
                  20240121T164914
                  20231227T163408
                  20231228T163736
                  20231229T164123
                  20240101T164316
                  20240117T164506))))
      (with-temp-buffer
        (insert
         "#+title: FOO\n"
         "#+identifier: 20231226T163250\n"
         "20240119T164551 20240120T164558")
        (goto-char (point-min))
        (should
         (equal (denote-tree--collect-links (buffer-name (current-buffer)))
                '(20240119T164551 20240120T164558))))
      (with-temp-buffer
        (insert
         "#+title: FOO\n"
         "#+identifier: 20231226T163250")
        (goto-char (point-min))
        (should
         (equal (denote-tree--collect-links (buffer-name (current-buffer)))
                nil)))
      (with-temp-buffer
        (insert "#+identifier: 20231226T163250")
        (goto-char (point-min))
        (should
         (equal (denote-tree--collect-links (buffer-name (current-buffer)))
                nil))))))

(ert-deftest denote-tree-test--extract-and-compare-symbols ()
  "Tests for `denote-tree--extract-and-compare-symbols'."
  (should
   (eq (denote-tree--extract-and-compare-symbols :title-key-regexp 'title)
       :title-key-regexp))
  (should
   (eq (denote-tree--extract-and-compare-symbols 'title-key-regexp 'title) nil))
  (should
   (eq (denote-tree--extract-and-compare-symbols :date-format 'date) nil))
  (should
   (eq (denote-tree--extract-and-compare-symbols :foo-bar-baz-regexp 'foo)
       :foo-bar-baz-regexp))
  (should
   (eq (denote-tree--extract-and-compare-symbols :foobar-regexp 'foobar)
       :foobar-regexp))
  (should-not (denote-tree--extract-and-compare-symbols :foo-regexp nil))
  (should-not (denote-tree--extract-and-compare-symbols nil 'bar)))

(ert-deftest denote-tree-test--get-regexps ()
  "Tests for `denote-tree--get-regexps'.

`denote-tree--get-regexps' returns a symbol if and only if it ends with -regexp
and it's value in plist is a string."
  (should-not (denote-tree--get-regexps '()))
  (should-not (denote-tree--get-regexps '("foor" "baz")))
  (should-not (denote-tree--get-regexps '(:regexp "foor")))
  (should
   (equal (denote-tree--get-regexps '(:foo-regexp "foor" :bar-regexp bar))
          '(:foo-regexp)))
  (should
   (equal
    (denote-tree--get-regexps '(:foo-regexp "foor" :bar "bar")) '(:foo-regexp)))
  (should
   (equal (denote-tree--get-regexps '(:foo-regexp "foor" :bar-regexp "baar"))
          '(:bar-regexp :foo-regexp))))

(ert-deftest denote-tree-test--open-link-maybe ()
  "Tests for `denote-tree--open-link-maybe'."
  (cl-letf (((symbol-function 'get-buffer) (lambda (x) t)))
    (should (string= (denote-tree--open-link-maybe "foo") "foo")))
  (with-temp-buffer
    (cl-letf (((symbol-function 'get-buffer) (lambda (x) x)))
      (let ((denote-tree--visited-buffers nil))
        (denote-tree--open-link-maybe "foo")
        (should (equal denote-tree--visited-buffers nil))))
    (erase-buffer)
    (cl-letf (((symbol-function 'denote-get-path-by-id) (lambda (x) t))
              ((symbol-function 'get-buffer-create)
               (lambda (x) (buffer-name (current-buffer))))
              ((symbol-function 'insert-file-contents)
               (lambda (x) (insert "foo"))))
      (let ((denote-tree--visited-buffers '("foo")))
        (denote-tree--open-link-maybe "foo")
        (should (equal denote-tree--visited-buffers '("foo")))))))

(ert-deftest denote-tree-test--get-prop ()
  "Tests for `denote-tree--get-prop'."
  (with-temp-buffer
    (insert
     " " (propertize "A" 'a "foo") "\n"
     "foo bar baz\n")
    (should (denote-tree--get-prop 'a 2))
    (should (denote-tree--get-prop 'a))
    (should-not (denote-tree--get-prop 'b 2))
    (should-not (denote-tree--get-prop 'a 4))))

(ert-deftest denote-tree-test--nested-value ()
  "Tests for `denote-tree--nested-value'."
  (let ((alist '((foo :1 1 :2 2) (bar :3 3 :4 4))))
    (should (equal (denote-tree--nested-value alist 'foo :2)
                   2)))
  (let ((alist '((foo :next bar) (bar :1 1))))
    (should (equal (denote-tree--nested-value alist 'foo :next :1)
                   1)))
  (let ((alist '((foo :next (bar far)) (bar :1 1) (far :2 2))))
    (should (equal (denote-tree--nested-value alist 'foo :next)
                   '(bar far))))
  (let ((alist '((foo :next (bar far)) (bar :1 1) (far :2 2))))
    (should (equal (denote-tree--nested-value alist 'foo :next :1)
                   1)))
  (let ((alist '((foo :next (bar far)) (bar :1 1) (far :2 2))))
    (should (equal (denote-tree--nested-value alist 'foo :next :2)
                   2)))
  (let ((alist '((foo :next bar) (bar :lol 5))))
    (should (equal (denote-tree--nested-value alist 'foo :next :5)
                   nil)))
  (let ((alist '((foo :next 5) (bar :lol 5))))
    (should (equal (denote-tree--nested-value alist 'foo :next :1)
                   nil))))

(ert-deftest denote-tree-test--unique-nodes ()
  "Test for `denote-tree--unique-nodes'."
  (should-not (equal (car (denote-tree--unique-nodes 'a t))
                     'a))
  (should (equal (car (denote-tree--unique-nodes 'a nil))
                 'a)))

(ert-deftest denote-tree-test--fix-children-in-alist ()
  "Tests for `denote-tree--fix-children-in-alist'."
  (let ((alist '((a12 :true-name a :children nil)
                 (a :true-name a :children (b c)))))
    (should (equal (denote-tree--fix-children-in-alist alist)
                   '((a12 :true-name a :children (b c))
                     (a :true-name a :children (b c))))))
  (let ((alist '((a :true-name a :children nil))))
    (should (equal (denote-tree--fix-children-in-alist alist)
                   '((a :true-name a :children nil)))))
  (let ((alist '((a12 :true-name a :children nil))))
    (should (equal (denote-tree--fix-children-in-alist alist)
                   '((a12 :true-name a :children nil)))))
  (let ((alist '((a12 :true-name a :children (b c))
                 (a :true-name a :children (d e)))))
    (should (equal (denote-tree--fix-children-in-alist alist)
                   '((a12 :true-name a :children (d e))
                     (a :true-name a :children (d e)))))))

(ert-deftest denote-tree-test--draw-node-foo ()
  "Tests for `denote-tree--draw-node-foo'."
  (with-temp-buffer
    (should (= (denote-tree--draw-node-foo
                'a '(:true-name a :descp "a" :last nil :pos nil) "")
               3)))
  (with-temp-buffer
    (denote-tree--draw-node-foo
     'a '(:true-name a :descp "a" :last nil :pos nil) "")
    (should (equal (buffer-substring-no-properties (point-min) (point-max))
                   "+-* a\n")))
  (with-temp-buffer
    (denote-tree--draw-node-foo
     'a '(:true-name a :descp "a" :last nil :pos nil) "")
    (should
     (equal (get-text-property (point-min) 'denote-tree--identifier) 'a))
    (should
     (equal (get-text-property
             (next-single-property-change (point-min) 'face)
             'face)
            'denote-tree-node)))
  (with-temp-buffer
    (should (= (denote-tree--draw-node-foo
                'a '(:true-name b :descp "a" :last nil :pos nil) "|")
               4)))
  (with-temp-buffer
    (denote-tree--draw-node-foo
     'a '(:true-name b :descp "a" :last nil :pos nil) "|")
    (should (equal (buffer-substring-no-properties (point-min) (point-max))
                   "|+-* a\n")))
  (with-temp-buffer
    (denote-tree--draw-node-foo
     'a '(:true-name b :descp "a" :last nil :pos nil) "|")
    (should
     (equal (get-text-property (point-min) 'denote-tree--identifier) 'a))
    (should
     (equal (get-text-property
             (next-single-property-change (point-min) 'face)
             'face)
            'denote-tree-circular-node))))

(ert-deftest denote-tree-test--next-node ()
  "Tests for `denote-tree-next-node'."
  (with-temp-buffer
    (let ((alist '((a :next-indent "|" :children (b c d)
                      :last t :depth t
                      :true-name a :descp "a"
                      :parent a :next a :prev a)
                   (b :next-indent "||" :children nil
                      :last t :depth t
                      :true-name d :descp "b"
                      :parent a :next c :prev d)
                   (c :next-indent "||" :children nil
                      :last t :depth t
                      :true-name c :descp "c"
                      :parent a :next d :prev b)
                   (d :next-indent "||" :children nil
                      :last t :depth t
                      :true-name d :descp "d"
                      :parent a :next b :prev c))))
      (setq denote-tree--tree-alist
            (denote-tree--draw-node-list alist 'a))
      (goto-char (point-min))
      (forward-line)
      (denote-tree-next-node)
      (should
       (equal (get-text-property (point) 'denote-tree--identifier) 'c))
      (denote-tree-next-node)
      (should
       (equal (get-text-property (point) 'denote-tree--identifier) 'd))
      (denote-tree-next-node)
      (should
       (equal (get-text-property (point) 'denote-tree--identifier) 'b))
      (denote-tree-next-node -1)
      (should
       (equal (get-text-property (point) 'denote-tree--identifier) 'd))
      (denote-tree-next-node 2)
      (should
       (equal (get-text-property (point) 'denote-tree--identifier) 'c)))))

(ert-deftest denote-tree-test--child-node ()
  "Tests for `denote-tree-next-node'."
  (with-temp-buffer
    (let ((alist '((a :next-indent "|" :children (b)
                      :last t :depth t
                      :true-name a :descp "a"
                      :parent nil :next a :prev a)
                   (b :next-indent "||" :children (c)
                      :last t :depth t
                      :true-name b :descp "b"
                      :parent a :next c :prev d)
                   (c :next-indent "|||" :children (d)
                      :last t :depth t
                      :true-name c :descp "c"
                      :parent b :next c :prev c)
                   (d :next-indent "||||" :children nil
                      :last t :depth t
                      :true-name d :descp "d"
                      :parent c :next d :prev d))))
      (setq denote-tree--tree-alist
            (denote-tree--draw-node-list alist 'a))
      (goto-char (point-min))
      (denote-tree-child-node)
      (should
       (equal (get-text-property (point) 'denote-tree--identifier) 'b))
      (denote-tree-child-node)
      (should
       (equal (get-text-property (point) 'denote-tree--identifier) 'c))
      (denote-tree-child-node)
      (should
       (equal (get-text-property (point) 'denote-tree--identifier) 'd))
      (denote-tree-child-node -1)
      (should
       (equal (get-text-property (point) 'denote-tree--identifier) 'c))
      (denote-tree-child-node -2)
      (should
       (equal (get-text-property (point) 'denote-tree--identifier) 'a))))
  (with-temp-buffer
    (let ((alist '((a :next-indent "|" :children (b c)
                      :last t :depth t
                      :true-name a :descp "a"
                      :parent nil :next a :prev a)
                   (b :next-indent "||" :children (d)
                      :last t :depth t
                      :true-name b :descp "b"
                      :parent a :next b :prev b)
                   (d :next-indent "||||" :children nil
                      :last t :depth t
                      :true-name d :descp "d"
                      :parent b :next d :prev d)
                   (c :next-indent "||" :children (b123)
                      :last t :depth t
                      :true-name c :descp "c"
                      :parent a :next c :prev c)
                   (b123 :next-indent "||" :children (d)
                         :last t :depth t
                         :true-name b :descp "b"
                         :parent c :next b123 :prev b123))))
      (setq denote-tree--tree-alist
            (denote-tree--draw-node-list alist 'a))
      (goto-char (point-min))
      (forward-line 4)
      (denote-tree-child-node)
      (should
       (equal (get-text-property (point) 'denote-tree--identifier) 'd))
      (denote-tree-child-node -1)
      (should
       (equal (get-text-property (point) 'denote-tree--identifier) 'b123)))))

(ert-deftest denote-tree-test--parent-node ()
  "Tests for `denote-tree-next-node'."
  (with-temp-buffer
    (let ((alist '((a :next-indent "|" :children (b)
                      :last t :depth t
                      :true-name a :descp "a"
                      :parent nil :next a :prev a)
                   (b :next-indent "||" :children (c)
                      :last t :depth t
                      :true-name b :descp "b"
                      :parent a :next c :prev d)
                   (c :next-indent "|||" :children (d)
                      :last t :depth t
                      :true-name c :descp "c"
                      :parent b :next c :prev c)
                   (d :next-indent "||||" :children nil
                      :last t :depth t
                      :true-name d :descp "d"
                      :parent c :next d :prev d))))
      (setq denote-tree--tree-alist
            (denote-tree--draw-node-list alist 'a))
      (goto-char (point-max))
      (forward-line -1)
      (denote-tree-parent-node)
      (should
       (equal (get-text-property (point) 'denote-tree--identifier) 'c))
      (denote-tree-parent-node)
      (should
       (equal (get-text-property (point) 'denote-tree--identifier) 'b))
      (denote-tree-parent-node)
      (should
       (equal (get-text-property (point) 'denote-tree--identifier) 'a))
      (denote-tree-parent-node -1)
      (should
       (equal (get-text-property (point) 'denote-tree--identifier) 'b))
      (denote-tree-parent-node -2)
      (should
       (equal (get-text-property (point) 'denote-tree--identifier) 'd))))
  (with-temp-buffer
    (let ((alist '((a :next-indent "|" :children (b c)
                      :last t :depth t
                      :true-name a :descp "a"
                      :parent nil :next a :prev a)
                   (b :next-indent "||" :children (d)
                      :last t :depth t
                      :true-name b :descp "b"
                      :parent a :next b :prev b)
                   (d :next-indent "||||" :children nil
                      :last t :depth t
                      :true-name d :descp "d"
                      :parent b :next d :prev d)
                   (c :next-indent "||" :children (b123)
                      :last t :depth t
                      :true-name c :descp "c"
                      :parent a :next c :prev c)
                   (b123 :next-indent "||" :children (d)
                         :last t :depth t
                         :true-name b :descp "b"
                         :parent c :next b123 :prev b123))))
      (setq denote-tree--tree-alist
            (denote-tree--draw-node-list alist 'a))
      (goto-char (point-max))
      (forward-line -1)
      (denote-tree-parent-node -1)
      (should
       (equal (get-text-property (point) 'denote-tree--identifier) 'd))
      (denote-tree-parent-node)
      (should
       (equal (get-text-property (point) 'denote-tree--identifier) 'b123)))))

(defun denote-tree-test-mock-make-next-links (lst-of-links)
  "Closure returning next element from LST-OF-LINKS."
  (let ((x lst-of-links))
    (lambda ()
      (prog1 x
        (setq x (cdr x))))))

(defmacro denote-tree-test-mock-draw-tree (lst-of-links)
  "Execute `denote-tree--walk-links-iteratively' over LST-OF-LINKS."
  `(let ((denote-tree-max-traversal-depth t)
         (tree-alist '())
         (next (denote-tree-test-mock-make-next-links ,lst-of-links)))
     (cl-letf (((symbol-function 'denote-tree--collect-links)
                (lambda (x)
                  (car (funcall next))))
               ((symbol-function 'denote-tree--open-link-maybe)
                (lambda (x)
                  (intern x)))
               ((symbol-function 'denote-tree--collect-keywords-as-string)
                (lambda (x _)
                  (propertize x 'denote-tree--type 'title))))
       (unwind-protect
           (setq tree-alist
                 (denote-tree--fix-children-in-alist
                  (denote-tree--walk-links-iteratively
                   (caar (funcall next)) "" t t)))
           (denote-tree--clean-up)))
     tree-alist))

(ert-deftest denote-tree-test--determine-node-bounds ()
  "Tests for `denote-tree--determine-node-bounds'."
  (with-temp-buffer
    (let ((alist (denote-tree-test-mock-draw-tree
                  '(("a") (b c d) (b1 b2) nil nil (c1 c2) nil nil (d1 d2)))))
      (denote-tree--draw-node-list alist 'a)
      (goto-char (point-min))
      ;; at b1
      (forward-line 2)
      (should
       (equal '(15 25)
              (denote-tree--determine-node-bounds
               (get-text-property (point) 'denote-tree--identifier) alist)))
      (goto-char (point-min))
      ;; at b2
      (forward-line 3)
      (should
       (equal '(26 36)
              (denote-tree--determine-node-bounds
               (get-text-property (point) 'denote-tree--identifier) alist)))
      (goto-char (point-min))
      ;; at b
      (forward-line 1)
      (should
       (equal '(7 36)
              (denote-tree--determine-node-bounds
               (get-text-property (point) 'denote-tree--identifier) alist)))
      (goto-char (point-min))
      ;; at a
      (should
       (equal '(1 96)
              (denote-tree--determine-node-bounds
               (get-text-property (point) 'denote-tree--identifier) alist)))))
  (with-temp-buffer
    (let ((alist (denote-tree-test-mock-draw-tree
                  '(("a") (b c d) (b1) (b2) (b3) nil (c1 c2) nil nil (d1 d2)))))
      (denote-tree--draw-node-list alist 'a)
      (goto-char (point-min))
      ;; at b3
      (forward-line 4)
      (should
       (equal '(39 53)
              (denote-tree--determine-node-bounds
               (get-text-property (point) 'denote-tree--identifier) alist)))))
  (with-temp-buffer
    (let ((alist (denote-tree-test-mock-draw-tree
                  '(("a") (b c d) (b1) nil (c1) nil (d1 d2 d3)))))
      (denote-tree--draw-node-list alist 'a)
      (goto-char (point-min))
      ;; at b3
      (forward-line 8)
      (should
       (equal '(75 85)
              (denote-tree--determine-node-bounds
               (get-text-property (point) 'denote-tree--identifier) alist))))))

(ert-deftest denote-tree-test--deepen-traversal-trivial-redraw ()
  "Tests for `denote-tree--deepen-traversal'."
  (with-temp-buffer
    (let ((alist (denote-tree-test-mock-draw-tree
                  '(("a") (b c d) (b1) nil nil nil)))
          (buffer-look))
      (denote-tree--draw-node-list alist 'a)
      (setq buffer-look (buffer-string))
      (goto-char (point-min))
      ;; at b
      (forward-line 1)
      (let ((denote-tree-max-traversal-depth t)
            (next (denote-tree-test-mock-make-next-links '((b1) nil))))
        (cl-letf (((symbol-function 'denote-tree--collect-links)
                   (lambda (x)
                     (car (funcall next))))
                  ((symbol-function 'denote-tree--open-link-maybe)
                   (lambda (x)
                     (intern x)))
                  ((symbol-function 'denote-tree--collect-keywords-as-string)
                   (lambda (x _)
                     (propertize x 'denote-tree--type 'title))))
          (unwind-protect
              (denote-tree--deepen-traversal alist)
            (denote-tree--clean-up)))
        (should (equal buffer-look
                       (buffer-string)))))))

(ert-deftest denote-tree-test--deepen-traversal-add-nodes ()
  "Tests for `denote-tree--deepen-traversal'."
  (with-temp-buffer
    (let ((alist (denote-tree-test-mock-draw-tree
                  '(("a") (b c d) (b1) nil nil nil)))
          (buffer-look))
      (denote-tree--draw-node-list alist 'a)
      (setq buffer-look (buffer-string))
      (goto-char (point-min))
      ;; at b
      (forward-line 1)
      (let ((denote-tree-max-traversal-depth t)
            (tree-alist '())
            (next (denote-tree-test-mock-make-next-links '((b1 b2 b3) (b1a) nil nil nil))))
        (cl-letf (((symbol-function 'denote-tree--collect-links)
                   (lambda (x)
                     (car (funcall next))))
                  ((symbol-function 'denote-tree--open-link-maybe)
                   (lambda (x)
                     (intern x)))
                  ((symbol-function 'denote-tree--collect-keywords-as-string)
                   (lambda (x _)
                     (propertize x 'denote-tree--type 'title))))
          (unwind-protect
              (setq tree-alist (cadr (denote-tree--deepen-traversal alist)))
            (denote-tree--clean-up)))
        (should-not (equal buffer-look
                           (buffer-string)))
        (should (denote-tree--nested-value tree-alist 'b2 :true-name))
        (should (denote-tree--nested-value tree-alist 'b1 :true-name))
        (should (denote-tree--nested-value tree-alist 'b1a :true-name))))))

(ert-deftest denote-tree-test--deepen-traversal-delete-nodes ()
  "Tests for `denote-tree--deepen-traversal'."
  (with-temp-buffer
    (let ((alist (denote-tree-test-mock-draw-tree
                  '(("a") (b c d) (b1 b2 b3) (b1a) nil nil nil nil nil nil)))
          (buffer-look))
      (denote-tree--draw-node-list alist 'a)
      (setq buffer-look (buffer-string))
      (goto-char (point-min))
      ;; at b
      (forward-line 1)
      (let ((denote-tree-max-traversal-depth t)
            (tree-alist '())
            (next (denote-tree-test-mock-make-next-links '((b1) nil))))
        (cl-letf (((symbol-function 'denote-tree--collect-links)
                   (lambda (x)
                     (car (funcall next))))
                  ((symbol-function 'denote-tree--open-link-maybe)
                   (lambda (x)
                     (intern x)))
                  ((symbol-function 'denote-tree--collect-keywords-as-string)
                   (lambda (x _)
                     (propertize x 'denote-tree--type 'title))))
          (unwind-protect
              (setq tree-alist (cadr (denote-tree--deepen-traversal alist)))
            (denote-tree--clean-up)))
        (should-not (denote-tree--nested-value tree-alist 'b2 :true-name))
        (should-not (denote-tree--nested-value tree-alist 'b3 :true-name))
        (should-not (denote-tree--nested-value tree-alist 'b1a :true-name))))))

(ert-deftest denote-tree-test--deepen-traversal-circular-nodes ()
  "Tests for `denote-tree--deepen-traversal'."
  (with-temp-buffer
    (let ((alist (denote-tree-test-mock-draw-tree
                  '(("a") (b c d) (b1) (c) nil nil)))
          (buffer-look))
      (denote-tree--draw-node-list alist 'a)
      (setq buffer-look (buffer-string))
      (goto-char (point-min))
      ;; at b
      (forward-line 1)
      (let ((denote-tree-max-traversal-depth t)
            (tree-alist '())
            (next (denote-tree-test-mock-make-next-links '((b1) (c) nil))))
        (cl-letf (((symbol-function 'denote-tree--collect-links)
                   (lambda (x)
                     (car (funcall next))))
                  ((symbol-function 'denote-tree--open-link-maybe)
                   (lambda (x)
                     (intern x)))
                  ((symbol-function 'denote-tree--collect-keywords-as-string)
                   (lambda (x _)
                     (propertize x 'denote-tree--type 'title))))
          (unwind-protect
              (setq tree-alist (cadr (denote-tree--deepen-traversal alist)))
            (denote-tree--clean-up)))
        (should (equal buffer-look
                       (buffer-string)))))))

(ert-deftest denote-tree-test--deepen-traversal-circular-nodes-delete ()
  "Tests for `denote-tree--deepen-traversal'."
  (with-temp-buffer
    (let ((alist (denote-tree-test-mock-draw-tree
                  '(("a") (b d) (c) (c1 c2) nil nil (c) nil nil)))
          (buffer-look))
      (denote-tree--draw-node-list alist 'a)
      (goto-char (point-min))
      ;; at b
      (forward-line 1)
      (let ((denote-tree-max-traversal-depth t)
            (tree-alist '())
            (next (denote-tree-test-mock-make-next-links '((nil) (c) (c1 c2)))))
        (cl-letf (((symbol-function 'denote-tree--collect-links)
                   (lambda (x)
                     (car (funcall next))))
                  ((symbol-function 'denote-tree--open-link-maybe)
                   (lambda (x)
                     (intern x)))
                  ((symbol-function 'denote-tree--collect-keywords-as-string)
                   (lambda (x _)
                     (propertize x 'denote-tree--type 'title))))
          (unwind-protect
              (setq tree-alist (cadr (denote-tree--deepen-traversal alist)))
            (denote-tree--clean-up)))
        (should
         (equal (denote-tree--nested-value alist 'c :true-name)
                (denote-tree--nested-value tree-alist 'c :true-name)))
        (should
         (equal (denote-tree--nested-value alist 'c :children)
                (denote-tree--nested-value tree-alist 'c :children)))
        (should
         (equal (denote-tree--nested-value tree-alist 'c :parent)
                'd))
        (should
         (equal (denote-tree--nested-value alist 'c1 :true-name)
                (denote-tree--nested-value tree-alist 'c1 :true-name)))
        (should
         (equal (denote-tree--nested-value alist 'c1 :children)
                (denote-tree--nested-value tree-alist 'c1 :children)))
        (should
         (equal (denote-tree--nested-value alist 'c1 :parent)
                (denote-tree--nested-value tree-alist 'c1 :parent)))
        (should
         (equal (denote-tree--nested-value alist 'c2 :true-name)
                (denote-tree--nested-value tree-alist 'c2 :true-name)))
        (should
         (equal (denote-tree--nested-value alist 'c2 :children)
                (denote-tree--nested-value tree-alist 'c2 :children)))
        (should
         (equal (denote-tree--nested-value alist 'c2 :parent)
                (denote-tree--nested-value tree-alist 'c2 :parent)))))))

(ert-deftest denote-tree-test--calculate-indent ()
  "Tests for `denote-tree--calculate-indent'."
  (should (equal
           (denote-tree--calculate-indent "" t)
           "  "))
  (should (equal
           (denote-tree--calculate-indent "" nil)
           "| "))
  (should (equal
           (denote-tree--calculate-indent "xx" t)
           "xx  "))
  (should (equal
           (denote-tree--calculate-indent "xx" nil)
           "xx| ")))

(ert-deftest denote-tree-test--next-sibling ()
  "Tests for `denote-tree--next-sibling'."
  (should (equal (denote-tree--next-sibling 'a '(a b c))
                 'b))
  (should (equal (denote-tree--next-sibling 'c '(a b c))
                 'a))
  (should (equal (denote-tree--next-sibling 'b '(a b c))
                 'c))
  (should (equal (denote-tree--next-sibling 'a '(a))
                 'a))
  (should-not (denote-tree--next-sibling 'a '(b c d)))
  (should-not (denote-tree--next-sibling nil '())))

(ert-deftest denote-tree-test--walk-region ()
  "Tests for `denote-tree--walk-region'."
  (let ((alist
         (denote-tree-test-mock-draw-tree
          '(("a") (b c d) (b1 b2) (b3) nil nil nil))))
    (with-temp-buffer
      (denote-tree--draw-node-list alist 'a)
      (save-restriction
        (apply #'narrow-to-region (denote-tree--determine-node-bounds 'b alist))
        (goto-char (point-min))
        (should-not
         (seq-difference
            (denote-tree--walk-region
             (lambda () (get-text-property (point) 'denote-tree--identifier)))
            '(b2 b3 b1 b))))))
  (let ((alist
         (denote-tree-test-mock-draw-tree
          '(("a") (b c d) (b1 b2) (c) nil nil nil))))
    (with-temp-buffer
      (denote-tree--draw-node-list alist 'a)
      (save-restriction
        (apply #'narrow-to-region (denote-tree--determine-node-bounds 'b alist))
        (goto-char (point-min))
        ;; should have one element = gensymed c
        (should
         (= 1
            (length
             (seq-difference
              (denote-tree--walk-region
               (lambda () (get-text-property (point) 'denote-tree--identifier)))
              '(b2 b1 b))))))))
  (let ((alist
         (denote-tree-test-mock-draw-tree
          '(("a") (b c d) (b1 b2) (c) nil nil nil))))
    (with-temp-buffer
      (denote-tree--draw-node-list alist 'a)
      (save-restriction
        (apply #'narrow-to-region (denote-tree--determine-node-bounds 'c alist))
        (goto-char (point-min))
        (should-not
         (seq-difference
          (denote-tree--walk-region
           (lambda () (get-text-property (point) 'denote-tree--identifier)))
          '(c)))))))

(ert-deftest denote-tree-test--alist-in-region ()
  "Tests for `denote-tree--alist-in-region'."
  (let ((alist
         (denote-tree-test-mock-draw-tree
          '(("a") (b c d) nil (c1 c2) (c3) nil nil))))
    (with-temp-buffer
      (cl-letf (((symbol-function 'denote-tree--walk-region)
                 (lambda (_)
                   '(c1 c2 c3 c))))
        (should
         (equal '(c c3 c2 c1)
                (mapcar #'car (denote-tree--alist-in-region alist))))))))

(ert-deftest denote-tree-test--redraw-node ()
  "Tests for `denote-tree--redraw-node'."
  (with-temp-buffer
    (let ((alist
           (denote-tree-test-mock-draw-tree
            '(("a") (b c d)))))
      (denote-tree--draw-node-list alist 'a)
      (cl-letf (((symbol-function 'denote-tree--collect-keywords-as-string)
                 (lambda (_ _)
                   "Foo")))
        (goto-char (point-min))
        (should (string= (denote-tree--nested-value
                          (denote-tree--redraw-node "a" alist) 'a :descp)
                         "Foo")))))
  (with-temp-buffer
    (let ((alist
           (denote-tree-test-mock-draw-tree
            '(("a") (b c d))))
          (prev-buffer-string))
      (denote-tree--draw-node-list alist 'a)
      (setq prev-buffer-string (buffer-string))
      (cl-letf (((symbol-function 'denote-tree--collect-keywords-as-string)
                 (lambda (_ _)
                   "a")))
        (goto-char (point-min))
        (denote-tree--redraw-node "a" alist)
        (should (equal (buffer-string)
                       prev-buffer-string)))))
  (with-temp-buffer
    (let ((alist
           (denote-tree-test-mock-draw-tree
            '(("a") (b c d))))
          (prev-mark))
      (denote-tree--draw-node-list alist 'a)
      (setq prev-mark
            (marker-position (denote-tree--nested-value alist 'a :pos)))
      (cl-letf (((symbol-function 'denote-tree--collect-keywords-as-string)
                 (lambda (_ _)
                   "a")))
        (goto-char (point-min))
        (should (equal (marker-position
                        (denote-tree--nested-value
                         (denote-tree--redraw-node "a" alist) 'a :pos))
                       prev-mark))))))

(ert-deftest denote-tree-test--grow-alist-and-stack ()
  "Tests for `denote-tree--grow-alist-and-stack'."
  (cl-letf (((symbol-function 'denote-tree--collect-links)
             (lambda (x)
               '(b c d)))
            ((symbol-function 'denote-tree--open-link-maybe)
             (lambda (x)
               t))
            ((symbol-function 'denote-tree--collect-keywords-as-string)
             (lambda (x y)
               x)))
    (let* ((alist (list (denote-tree--node-plist
                         '(a . a) 'a 'a nil "" t t)))
           (ret (denote-tree--grow-alist-and-stack 'a alist nil (list 'a)))
           (new-alist (cadr ret))
           (new-stack (cadddr ret))
           (new-node (car ret)))
      (should-not
       (equal alist new-alist))
      (should
       (equal '(b c d) (denote-tree--nested-value new-alist 'a :children)))
      (should
       (equal '(b c d) new-stack))
      (should
       (equal 'b new-node))))
  (cl-letf (((symbol-function 'denote-tree--collect-links)
             (lambda (x)
               '(b c d)))
            ((symbol-function 'denote-tree--open-link-maybe)
             (lambda (x)
               (unless (equal x "c") t)))
            ((symbol-function 'denote-tree--collect-keywords-as-string)
             (lambda (x y)
               x)))
    (let* ((alist (list (denote-tree--node-plist
                         '(a . a) 'a 'a nil "" t t)))
           (ret (denote-tree--grow-alist-and-stack 'a alist nil (list 'a)))
           (new-alist (cadr ret))
           (new-stack (cadddr ret))
           (new-node (car ret)))
      (should-not
       (equal alist new-alist))
      (should-not
       (alist-get 'c new-alist))
      (should
       (equal '(b d) (denote-tree--nested-value new-alist 'a :children)))
      (should
       (equal 'd (denote-tree--nested-value new-alist 'b :next)))
      (should
       (equal 'd (denote-tree--nested-value new-alist 'b :prev)))
      (should
       (equal '(b d) new-stack))
      (should
       (equal 'b new-node)))))

(ert-deftest denote-tree-test--draw-node-list-helper ()
  "Tests for `denote-tree--draw-node-list-helper'."
  (with-temp-buffer
    (let* ((alist (denote-tree-test-mock-draw-tree
                   '(("a") (b c d))))
           (ret (denote-tree--draw-node-list-helper
                 'a alist (alist-get 'a alist) '(a)))
           (new-node (car ret))
           (new-alist (cadr ret))
           (new-info (caddr ret))
           (new-stack (cadddr ret)))
      (should (equal 'b new-node))
      (should (equal new-alist alist))
      (should (equal new-info (alist-get 'b alist)))
      (should (equal new-stack '(b c d))))))

(ert-deftest denote-tree-test--find-orphans ()
  "Tests for `denote-tree--find-orphans'."
  (should
   (equal (denote-tree--find-orphans '(a) '((a5 foo) (b bar)))
          '((a5 foo))))
  (should
   (equal (denote-tree--find-orphans '(a c) '((a5 foo) (b bar) (c4 baz)))
          '((c4 baz)
            (a5 foo))))
  (should
   (equal (denote-tree--find-orphans '(a) '((a baz) (a5 foo) (b bar)))
          '((a5 foo)))))

(provide 'denote-tree-test)
;;; denote-tree-test.el ends here
