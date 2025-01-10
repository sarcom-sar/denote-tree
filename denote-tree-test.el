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
                '("20231226T163250"
                  "20240119T164551"
                  "20240120T164558"
                  "20240121T164829"
                  "20240121T164914"
                  "20231227T163408"
                  "20231228T163736"
                  "20231229T164123"
                  "20240101T164316"
                  "20240117T164506"))))
      (with-temp-buffer
        (insert
         "#+title: FOO\n"
         "#+identifier: 20231226T163250\n"
         "20240119T164551 20240120T164558")
        (goto-char (point-min))
        (should
         (equal (denote-tree--collect-links (buffer-name (current-buffer)))
                '("20240119T164551" "20240120T164558"))))
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

(defun denote-tree-test-helper--make-marker-at (pos)
  (cond
   ((numberp pos)
    (set-marker (make-marker) pos))
   ((listp pos)
    (mapcar (lambda (x)
              (set-marker (make-marker) x))
            pos))))

(ert-deftest denote-tree-test--add-props-to-children ()
  "Tests for `denote-tree--add-props-to-children'."
  (should (equal (denote-tree--add-props-to-children '() '()) nil))
  (with-temp-buffer
    (insert (concat "'-* A\n" "  '-* B"))
    (goto-char (point-min))
    (denote-tree--add-props-to-children
     (denote-tree-test-helper--make-marker-at '(7))
     (denote-tree-test-helper--make-marker-at 1))
    (let ((props (text-properties-at 1)))
      (should
       (equal `(denote-tree--child ,(denote-tree-test-helper--make-marker-at 7))
              props)))
    (let ((props (text-properties-at 7)))
      (should
       (equal
        `( denote-tree--parent ,(denote-tree-test-helper--make-marker-at 1)
           denote-tree--prev ,(denote-tree-test-helper--make-marker-at 7)
           denote-tree--next ,(denote-tree-test-helper--make-marker-at 7))
        props))))
  (with-temp-buffer
    (insert
     "A\n"
     " B\n"
     " B\n"
     " B\n"
     " B\n")
    (goto-char (point-min))
    (denote-tree--add-props-to-children
     (denote-tree-test-helper--make-marker-at '(4 7 10 13))
     (denote-tree-test-helper--make-marker-at 1))
    (let ((props (text-properties-at 1)))
      (should
       (equal `(denote-tree--child ,(denote-tree-test-helper--make-marker-at 4))
              props)))
    (let ((props (text-properties-at 4)))
      (should
       (equal
        `( denote-tree--parent ,(denote-tree-test-helper--make-marker-at 1)
           denote-tree--prev ,(denote-tree-test-helper--make-marker-at 13)
           denote-tree--next ,(denote-tree-test-helper--make-marker-at 7))
        props))))
  (with-temp-buffer
    (insert
     " B\n"
     " B\n"
     "A\n"
     " B\n"
     " B\n")
    (goto-char (point-min))
    (denote-tree--add-props-to-children
     (denote-tree-test-helper--make-marker-at '(2 4 10 13))
     (denote-tree-test-helper--make-marker-at 7))
    (let ((props (text-properties-at 7)))
      (should
       (equal `(denote-tree--child ,(denote-tree-test-helper--make-marker-at 2))
              props)))
    (let ((props (text-properties-at 2)))
      (should
       (equal
        `( denote-tree--parent ,(denote-tree-test-helper--make-marker-at 7)
           denote-tree--prev ,(denote-tree-test-helper--make-marker-at 13)
           denote-tree--next ,(denote-tree-test-helper--make-marker-at 4))
        props))))
  (should-not (denote-tree--add-props-to-children '(4 5 6) nil)))

(defmacro denote-tree-test-mock--draw-node-macro (properties cyclic &rest body)
  "Execute BODY with `denote-tree--cyclic-buffers' set to CYCLIC and
`denote-tree--collect-keywords-as-string' set to return PROPERTIES."
  (declare (indent 2))
  `(cl-letf (((symbol-function 'denote-tree--collect-keywords-as-string)
              (lambda (_ _) (concat ,@properties))))
     (let ((denote-tree--cyclic-buffers ,cyclic))
       (with-temp-buffer
         ,@body))))

(ert-deftest denote-tree-test-properties--draw-node ()
  "Tests of changed properties for `denote-tree--draw-node'."
  (denote-tree-test-mock--draw-node-macro
      ((propertize "A" 'denote-tree--type 'a))
      nil
    (denote-tree--draw-node "name" "" t)
    (should (equal (text-properties-at 3) '(face denote-tree-node)))
    (should (equal (text-properties-at 5) '(denote-tree--type a))))
  (denote-tree-test-mock--draw-node-macro
      ((propertize "A" 'denote-tree--type 'a))
      nil
    (denote-tree--draw-node "name" "   " t)
    (should (equal (text-properties-at 8) '(denote-tree--type a))))
  (denote-tree-test-mock--draw-node-macro nil nil
    (should-error (denote-tree--draw-node nil nil nil)))
  (denote-tree-test-mock--draw-node-macro
      ((propertize "A " 'denote-tree--type 'a) (propertize "B" 'denote-tree--type 'b))
      nil
    (denote-tree--draw-node "name" "" t)
    (should (equal (text-properties-at 3) '(face denote-tree-node)))
    (should (equal (text-properties-at 5) '(denote-tree--type a)))
    (should (equal (text-properties-at 7) '(denote-tree--type b))))
  (denote-tree-test-mock--draw-node-macro
      ((propertize "A " 'denote-tree--type 'a) (propertize "B" 'denote-tree--type 'b))
      '("name")
    (denote-tree--draw-node "name" "" t)
    (should (equal (text-properties-at 3) '(face denote-tree-circular-node)))
    (should (equal (text-properties-at 5) '(denote-tree--type a)))
    (should (equal (text-properties-at 7) '(denote-tree--type b))))
  (denote-tree-test-mock--draw-node-macro nil nil
    (denote-tree--draw-node "name" "" nil)
    (should (equal (text-properties-at 5) nil))))

(ert-deftest denote-tree-test-ret--draw-node ()
  "Tests of return values for `denote-tree--draw-node'."
  (denote-tree-test-mock--draw-node-macro
      ((propertize "A" 'denote-tree--type 'a))
      nil
    (let ((ret-val (denote-tree--draw-node "name" "" nil)))
      (goto-char 3)
      (should (equal ret-val (list (point-marker) "| ")))))
  (denote-tree-test-mock--draw-node-macro
      ((propertize "A" 'denote-tree--type 'a))
      nil
    (let ((ret-val (denote-tree--draw-node "name" "| | |" nil)))
      (goto-char 8)
      (should (equal ret-val (list (point-marker) "| | || ")))))
  (denote-tree-test-mock--draw-node-macro
      ((propertize "A" 'denote-tree--type 'a))
      nil
    (let ((ret-val (denote-tree--draw-node "name" "" t)))
      (goto-char 3)
      (should (equal ret-val (list (point-marker) "  "))))))

(ert-deftest denote-tree-test--add-props-to-cycles ()
  "Tests for `denote-tree--add-props-to-cycles'.

If any 'button-data value repeats, then child of that node is
somewhere earlier, find it."
  (let ((denote-tree--cyclic-buffers nil))
    (with-temp-buffer
      (insert (propertize "* " 'button-data "name"))
      (denote-tree--add-props-to-cycles)
      (should (equal (text-properties-at 1) '(button-data "name")))))
  (with-temp-buffer
    (insert
     "'-"
     (propertize "* " 'face 'denote-tree-node 'button-data "name" 'denote-tree--child 10)
     "A\n"
     "  '-"
     (propertize "* " 'button-data "eman" 'denote-tree--child 22)
     "B\n"
     "     '-"
     (propertize "* " 'face 'denote-tree-circular-node 'button-data "name"))
    (let ((denote-tree--cyclic-buffers '("name")))
      (goto-char (point-min))
      (denote-tree--add-props-to-cycles))
    (let ((pos (get-text-property 22 'denote-tree--child)))
      (should (= pos 10))))
  (with-temp-buffer
    (insert
     "-"
     (propertize "* " 'face 'denote-tree-node 'button-data "name" 'denote-tree--child 8)
     "A1\n "
     (propertize "* " 'button-data "eman" 'denote-tree--child 19)
     "B1\n  "
     (propertize "* " 'face 'denote-tree-circular-node 'button-data "name" 'denote-tree--child nil)
     "A2\n "
     (propertize "* " 'face 'denote-tree-circular-node 'button-data "name")
     "A3\n")
    (let ((denote-tree--cyclic-buffers '("name")))
      (goto-char (point-min))
      (denote-tree--add-props-to-cycles))
    (let ((pos (get-text-property 15 'denote-tree--child)))
      (should (= pos 8)))
    (let ((pos (get-text-property 21 'denote-tree--child)))
      (should (= pos 8))))
  (with-temp-buffer
    (insert
     "-"
     (propertize "* " 'button-data "foo")
     "B\n "
     (propertize "* " 'face 'denote-tree-node 'button-data "name")
     "A\n")
    (let ((denote-tree--cyclic-buffers '("name")))
      (goto-char (point-min))
      (denote-tree--add-props-to-cycles))
    (let ((pos (get-text-property 7 'denote-tree--child)))
      (should (equal pos nil))))
  (with-temp-buffer
    (insert
     "-"
     (propertize "* " 'button-data "foo")
     "B\n "
     (propertize "* " 'face denote-tree-node 'button-data "name" 'denote-tree--child 7)
     "A\n")
    (let ((denote-tree--cyclic-buffers '("name")))
      (goto-char (point-min))
      (denote-tree--add-props-to-cycles))
    (let ((pos (get-text-property 7 'denote-tree--child)))
      (should (= pos 7)))))

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

(defvar denote-tree-test-mock-next-links nil)
(defun denote-tree-test-mock-make-next-links (lst-of-links)
  "Closure returning next element from LST-OF-LINKS."
  (let ((x lst-of-links))
    (setq denote-tree-test-mock-next-links
          (lambda ()
            (prog1 x
              (setq x (cdr x)))))))

(defmacro denote-tree-test-mock--walk-links-macro
    (cyc-bufs lst-of-links &rest body)
  "Execute BODY over mocked functions with CYC-BUFS and LST-OF-LINKS.

       Mock functions - `denote-tree--collect-links',
                        `denote-tree--collect-keywords-as-string'.
    Argument CYC-BUFS - set `denote-tree--cyclic-buffers' to that value.
Argument LST-OF-LINKS - list of links the `denote-tree--walk-links' will
                        iterate over.  To simulate branching paths one
                        has to insert empty list periodically.  For
                        example '((\"a\") (\"b\" \"c\") nil (\"d\")) will
                        construct a drawing where \"d\" is a child of \"c\";"
  (declare (indent 2))
  ;; ugly by hand check
  `(let ((denote-tree-max-traversal-depth t)
         (denote-tree--cyclic-buffers ,cyc-bufs)
         (denote-tree--visited-buffers nil))
     (denote-tree-test-mock-make-next-links ,lst-of-links)
     (cl-letf (((symbol-function 'denote-tree--collect-links)
                (lambda (x)
                  (let ((y (funcall denote-tree-test-mock-next-links)))
                    (car y))))
               ((symbol-function 'denote-tree--collect-keywords-as-string)
                (lambda (x _)
                  (when (string-match "temp" x)
                    (setq x " *temp*"))
                  (propertize x 'denote-tree--type 'title)))
               ((symbol-function 'denote-tree--open-link-maybe)
                (lambda (x) (get-buffer x) x)))
       (unwind-protect
           (progn
             ,@body)
         (denote-tree--clean-up)))))

(ert-deftest denote-tree-test-draw--walk-links ()
  "Tests for how `denote-tree--walk-links' draws."
  (with-temp-buffer
    (denote-tree-test-mock--walk-links-macro nil '(("a"))
      (denote-tree--walk-links (buffer-name (current-buffer)) "" t t)
      (should
       (string=
        (buffer-substring-no-properties (point-min) (point-max))
        (concat "'-*  *temp*\n"
                "  '-* a\n")))))
  (with-temp-buffer
    (denote-tree-test-mock--walk-links-macro nil '(("a") ("b" "c" "d"))
      (denote-tree--walk-links (buffer-name (current-buffer)) "" t t)
      (should
       (string=
        (buffer-substring-no-properties (point-min) (point-max))
        (concat
         "'-*  *temp*\n"
         "  '-* a\n"
         "    +-* b\n"
         "    +-* c\n"
         "    '-* d\n")))))
  (with-temp-buffer
    (denote-tree-test-mock--walk-links-macro nil
        '(("a") ("b" "c" "d") () ("e" "f"))
      (denote-tree--walk-links (buffer-name (current-buffer)) "" t t)
      (should
       (string=
        (buffer-substring-no-properties (point-min) (point-max))
        (concat
         "'-*  *temp*\n"
         "  '-* a\n"
         "    +-* b\n"
         "    +-* c\n"
         "    | +-* e\n"
         "    | '-* f\n"
         "    '-* d\n")))))
  (with-temp-buffer
    (denote-tree-test-mock--walk-links-macro '("a") '(("a") ("b" "c" "d" "a"))
      (denote-tree--walk-links (buffer-name (current-buffer)) "" t t)
      (should
       (string=
        (buffer-substring-no-properties (point-min) (point-max))
        (concat "'-*  *temp*\n"
                "  '-* a\n"))))))

(ert-deftest denote-tree-test-props--walk-links ()
  "Tests for how `denote-tree--walk-links' adds props."
  (with-temp-buffer
    (denote-tree-test-mock--walk-links-macro nil nil
      (denote-tree--walk-links "FOO" "" t t)
      (should
       (equal
        (text-properties-at 3)
        `(button-data "FOO"
          action denote-tree-enter-node
          category default-button
          button (t)
          face denote-tree-node)))))
  (with-temp-buffer
    (denote-tree-test-mock--walk-links-macro nil '(("a"))
      (denote-tree--walk-links "FOO" "" t t)
      (let ((props (text-properties-at 13)))
        (setq props
              (mapcar
               (lambda (x)
                 (if (markerp x)
                     (marker-position x)
                   x))
               props))
        (should
         (equal
          props
          '(denote-tree--parent 3
            denote-tree--prev 13
            denote-tree--next 13
            button-data "a"
            action denote-tree-enter-node
            category default-button
            button (t)
            face denote-tree-node))))))
  (with-temp-buffer
    (denote-tree-test-mock--walk-links-macro nil '(("b" "c" "d"))
      (denote-tree--walk-links "FOO" "" t t)
      (let ((props (text-properties-at 22)))
        (setq props
              (mapcar
               (lambda (x)
                 (if (markerp x)
                     (marker-position x)
                   x))
               props))
        (should
         (equal
          props
          '(denote-tree--parent 3
            denote-tree--prev 13
            denote-tree--next 29
            button-data "c"
            action denote-tree-enter-node
            category default-button
            button (t)
            face denote-tree-node))))))
  (with-temp-buffer
    (denote-tree-test-mock--walk-links-macro '("c")
        '(("a") ("b" "d") ("c") ("e" "f") ("c"))
      (denote-tree--walk-links "FOO" "" t t)
      (should (equal (get-text-property 35 'face) 'denote-tree-circular-node))
      (should
       (equal (get-text-property 57 'face) 'denote-tree-circular-node)))))

(ert-deftest denote-tree-child-node ()
  "Tests for `denote-tree-child-node'."
  (with-temp-buffer
    (insert
     "*" (propertize "A" 'denote-tree--child 6 'button-data "foo") "\n"
     (propertize "**B"))
    (goto-char 2)
    (should (equal (denote-tree-child-node 1) 6))
    (should (equal denote-tree--teleport-stack nil)))
  (with-temp-buffer
    (insert
     "*" (propertize "A" 'denote-tree--child 6 'button-data "foo") "\n"
     "**" (propertize "B" 'denote-tree--child 2 'button-data "bar"))
    (goto-char 6)
    (should (equal (denote-tree-child-node 1) 2))
    (should
     (equal
      denote-tree--teleport-stack
      `((,(denote-tree-test-helper--make-marker-at 6) 2)))))
  (with-temp-buffer
    (insert
     "*" (propertize "A" 'denote-tree--child 6 'button-data "foo") "\n"
     (propertize "**B" 'denote-tree--parent 2))
    (goto-char 6)
    (should (equal (denote-tree-child-node '-) 2))
    (should (equal denote-tree--teleport-stack nil)))
  (with-temp-buffer
    (insert
     "*" (propertize "A" 'denote-tree--child 6 'button-data "foo") "\n"
     "**" (propertize "B" 'denote-tree--child 2 'button-data "bar"))
    (goto-char 6)
    (should (equal (denote-tree-child-node '(4)) 2))
    (should (equal denote-tree--teleport-stack nil)))
  (with-temp-buffer
    (insert
     "*" (propertize "A" 'denote-tree--child 6 'button-data "foo") "\n"
     "**" (propertize "B" 'denote-tree--child 2 'button-data "bar"))
    (goto-char 6)
    (should (equal (denote-tree-child-node '(16)) 2))
    (should (equal denote-tree--teleport-stack nil))))

(ert-deftest denote-tree-parent-node ()
  "Tests for `denote-tree-parent-node'."
  (with-temp-buffer
    (insert
     "*" (propertize "A" 'denote-tree--child 6 'button-data "foo") "\n"
     (propertize "**B" 'denote-tree--parent 2))
    (goto-char 6)
    (should (equal (denote-tree-parent-node 1) 2))
    (should (equal denote-tree--teleport-stack nil)))
  ;; *A
  ;; **B
  ;; ***C
  ;; the point got teleported from C to B
  ;; if one wants go one lever back one should
  ;; arrive at C
  (with-temp-buffer
    (let ((denote-tree--teleport-stack '((11 6))))
      (insert
       "*"
       (propertize "A"
                   'denote-tree--parent nil
                   'denote-tree--child 6
                   'button-data "foo")
       "\n"
       "**"
       (propertize "B"
                   'denote-tree--parent 2
                   'denote-tree--child 11
                   'button-data "bar")
       "\n"
       "***"
       (propertize "C" 'denote-tree--child 6 'button-data "baz")
       "\n")
      (goto-char 6)
      (should (equal (denote-tree-parent-node 1) 11))
      (should (equal denote-tree--teleport-stack nil))))
  (with-temp-buffer
    (insert
     "*" (propertize "A" 'denote-tree--child 6 'button-data "foo") "\n"
     (propertize "**B"))
    (goto-char 2)
    (should (equal (denote-tree-parent-node -1) 6))
    (should (equal denote-tree--teleport-stack nil))))

(ert-deftest denote-tree-test--next-node ()
  "Tests for `denote-tree-next-node'.

No need to test `denote-tree-prev-node', because it calls
`denote-tree-next-node'."
  (with-temp-buffer
    (insert
     "*" (propertize "A" 'denote-tree--next 5 'denote-tree--prev 5) "\n"
     "*" (propertize "B" 'denote-tree--next 2 'denote-tree--prev 2))
    (goto-char 2)
    (should (equal (denote-tree-next-node 1) 5))
    (should (equal (denote-tree-next-node -1) 2)))
  (with-temp-buffer
    (insert
     "*" (propertize "A" 'denote-tree--next 5 'denote-tree--prev 5) "\n"
     "*" (propertize "B" 'denote-tree--next 8 'denote-tree--prev 2) "\n"
     "*" (propertize "C" 'denote-tree--next 2 'denote-tree--prev 5))
    (goto-char 5)
    (should (equal (denote-tree-next-node 1) 8))
    (should (equal (denote-tree-next-node 2) 5))
    (should (equal (denote-tree-next-node -2) 5))))

(ert-deftest denote-tree-test--enter-node ()
  "Tests for `denote-tree-enter-node'."
  (cl-letf (((symbol-function 'find-file-other-window) (lambda (x) x))
            ((symbol-function 'denote-get-path-by-id) (lambda (x) x)))
    (with-temp-buffer
      (insert "*" (propertize "A") "\n")
      (make-text-button 2 3 'action #'denote-tree-enter-node 'button-data "foo")
      (goto-char 2)
      (should (equal (push-button) t))
      (should (equal (denote-tree-enter-node "foo") "foo")))))

(ert-deftest denote-tree-test--redraw-node ()
  "Tests for `denote-tree-redraw-node'."
  (cl-letf (((symbol-function 'denote-tree--collect-keywords-as-string)
             (lambda (x y) "FOO baz bar")))
    (with-temp-buffer
      (insert "* FOO bar baz")
      (denote-tree--redraw-node "foo" 3)
      (should
       (equal
        (buffer-substring-no-properties 3 (line-end-position)) "FOO baz bar")))
    (with-temp-buffer
      (insert
       "* " (propertize "FOO" 'a 'b)
       " " (propertize "bar" 'a 'b)
       " " (propertize "baz" 'a 'b))
      (denote-tree--redraw-node "foo" 3)
      (should
       (equal (buffer-substring 3 (line-end-position))
              (concat (propertize "FOO baz bar" 'a 'b)))))))

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

(ert-deftest denote-tree-test--deepen-traversal ()
  "Tests for `denote-tree--deepen-traversal'."
  ;; simplest case, lone node
  ;; '-* test
  ;;   '-* a
  (let (prev next parent)
    (with-temp-buffer
      (denote-tree-test-mock--walk-links-macro nil '(("a"))
        (denote-tree--walk-links
         (buffer-name (current-buffer)) "" t t)
        ;; go to "a"
        (goto-line 2)
        (setq prev (marker-position
                    (get-text-property (point) 'denote-tree--prev)))
        (setq next (marker-position
                    (get-text-property (point) 'denote-tree--next)))
        (setq parent (marker-position
                      (get-text-property (point) 'denote-tree--parent))))
      (denote-tree-test-mock--walk-links-macro nil nil
        (denote-tree-redraw))
      ;; how marker changed
      (should (= prev (get-text-property (point) 'denote-tree--prev)))
      (should (= (get-text-property prev 'denote-tree--next) (point)))
      (should (= next (get-text-property (point) 'denote-tree--next)))
      (should (= (get-text-property next 'denote-tree--prev) (point)))
      (should (= parent (get-text-property (point) 'denote-tree--parent)))
      (should (= (get-text-property parent 'denote-tree--child) (point)))))
  ;; simple case, node to be redrawn has both prev and next nodes
  ;; '-* temp
  ;;   +-* a
  ;;   +-* b
  ;;   '-* c
  (let (prev next parent)
    (with-temp-buffer
      (denote-tree-test-mock--walk-links-macro nil '(("a" "b" "c"))
        (denote-tree--walk-links
         (buffer-name (current-buffer)) "" t t)
        ;; go to "b"
        (goto-line 3)
        (setq prev (marker-position
                    (get-text-property (point) 'denote-tree--prev)))
        (setq next (marker-position
                    (get-text-property (point) 'denote-tree--next)))
        (setq parent (marker-position
                      (get-text-property (point) 'denote-tree--parent))))
      (denote-tree-test-mock--walk-links-macro nil nil
        (denote-tree-redraw))
      ;; how marker changed
      (should (= prev (get-text-property (point) 'denote-tree--prev)))
      (should (= (get-text-property prev 'denote-tree--next) (point)))
      (should (= next (get-text-property (point) 'denote-tree--next)))
      (should (= (get-text-property next 'denote-tree--prev) (point)))))
  ;; simple case, next is behind prev
  ;; '-* test
  ;;   +-* a
  ;;   +-* b
  ;;   '-* c
  (let (prev next parent)
    (with-temp-buffer
      (denote-tree-test-mock--walk-links-macro nil '(("a" "b" "c"))
        (denote-tree--walk-links
         (buffer-name (current-buffer)) "" t t)
        ;; go to "c"
        (goto-line 4)
        (setq prev (marker-position
                    (get-text-property (point) 'denote-tree--prev)))
        (setq next (marker-position
                    (get-text-property (point) 'denote-tree--next)))
        (setq parent (marker-position
                      (get-text-property (point) 'denote-tree--parent))))
      (denote-tree-test-mock--walk-links-macro nil nil
        (denote-tree-redraw))
      ;; how marker changed
      (should (= prev (get-text-property (point) 'denote-tree--prev)))
      (should (= (get-text-property prev 'denote-tree--next) (point)))
      (should (= next (get-text-property (point) 'denote-tree--next)))
      (should (= (get-text-property next 'denote-tree--prev) (point)))
      (should (= parent (get-text-property (point) 'denote-tree--parent)))))
  ;; harder case, node is first
  ;; '-* temp
  ;;   +-* a
  ;;     +-* ab
  ;;     '-* ac
  ;;   +-* b
  ;;   '-* c
  (let (prev next parent)
    (with-temp-buffer
      (denote-tree-test-mock--walk-links-macro nil '(("a" "b" "c") ("ab" "ac"))
        (denote-tree--walk-links
         (buffer-name (current-buffer)) "" t t)
        ;; go to "a"
        (goto-line 2)
        (setq prev (marker-position
                    (get-text-property (point) 'denote-tree--prev)))
        (setq next (marker-position
                    (get-text-property (point) 'denote-tree--next)))
        (setq parent (marker-position
                      (get-text-property (point) 'denote-tree--parent))))
      (denote-tree-test-mock--walk-links-macro nil '(("ab" "ac"))
        (denote-tree-redraw))
      ;; how marker changed
      (should (= prev (get-text-property (point) 'denote-tree--prev)))
      (should (= (get-text-property prev 'denote-tree--next) (point)))
      (should (= next (get-text-property (point) 'denote-tree--next)))
      (should (= (get-text-property next 'denote-tree--prev) (point)))
      (should (= parent (get-text-property (point) 'denote-tree--parent)))
      (should (= (get-text-property parent 'denote-tree--child) (point)))))
  ;; harder case, node to be redrawn has both prev and next nodes
  ;; '-* temp
  ;;   +-* a
  ;;   +-* b
  ;;     +-* ba
  ;;     '-* bc
  ;;   '-* c
  (let (prev next parent)
    (with-temp-buffer
      (denote-tree-test-mock--walk-links-macro nil '(("a" "b" "c") nil ("ba" "bc"))
        (denote-tree--walk-links
         (buffer-name (current-buffer)) "" t t)
        ;; go to "b"
        (goto-line 3)
        (setq prev (marker-position
                    (get-text-property (point) 'denote-tree--prev)))
        (setq next (marker-position
                    (get-text-property (point) 'denote-tree--next)))
        (setq parent (marker-position
                      (get-text-property (point) 'denote-tree--parent))))
      (denote-tree-test-mock--walk-links-macro nil '(("ba" "bc"))
        (denote-tree-redraw))
      ;; how marker changed
      (should (= prev (get-text-property (point) 'denote-tree--prev)))
      (should (= (get-text-property prev 'denote-tree--next) (point)))
      (should (= next (get-text-property (point) 'denote-tree--next)))
      (should (= (get-text-property next 'denote-tree--prev) (point)))
      (should (= parent (get-text-property (point) 'denote-tree--parent)))))
  ;; harder case, node to be redrawn is last
  ;; '-* temp
  ;;   +-* a
  ;;   +-* b
  ;;   '-* c
  ;;     +-* ca
  ;;     '-* cb
  (let (prev next parent)
    (with-temp-buffer
      (denote-tree-test-mock--walk-links-macro nil '(("a" "b" "c") nil nil ("ca" "cb"))
        (denote-tree--walk-links
         (buffer-name (current-buffer)) "" t t)
        ;; go to "c"
        (goto-line 4)
        (setq prev (marker-position
                    (get-text-property (point) 'denote-tree--prev)))
        (setq next (marker-position
                    (get-text-property (point) 'denote-tree--next)))
        (setq parent (marker-position
                      (get-text-property (point) 'denote-tree--parent))))
      (denote-tree-test-mock--walk-links-macro nil '(("ca" "cb"))
        (denote-tree-redraw))
      ;; how marker changed
      (should (= prev (get-text-property (point) 'denote-tree--prev)))
      (should (= (get-text-property prev 'denote-tree--next) (point)))
      (should (= next (get-text-property (point) 'denote-tree--next)))
      (should (= (get-text-property next 'denote-tree--prev) (point)))
      (should (= parent (get-text-property (point) 'denote-tree--parent)))))
  ;; "the usual" case, deeply nested node
  ;; '-* temp
  ;;   +-* a
  ;;   | '-* aa
  ;;   |   '-* aaa
  ;;   |     +-* aaab
  ;;   |     '-* aaac
  ;;   +-* b
  (let (prev next parent)
    (with-temp-buffer
      (denote-tree-test-mock--walk-links-macro nil '(("a" "b") ("aa") ("aaa") ("aaab" "aaac"))
        (denote-tree--walk-links
         (buffer-name (current-buffer)) "" t t)
        ;; go to "aaac"
        (goto-line 6)
        (setq prev (marker-position
                    (get-text-property (point) 'denote-tree--prev)))
        (setq next (marker-position
                    (get-text-property (point) 'denote-tree--next)))
        (setq parent (marker-position
                      (get-text-property (point) 'denote-tree--parent))))
      (denote-tree-test-mock--walk-links-macro nil nil
        (denote-tree-redraw))
      ;; how marker changed
      (should (= prev (get-text-property (point) 'denote-tree--prev)))
      (should (= (get-text-property prev 'denote-tree--next) (point)))
      (should (= next (get-text-property (point) 'denote-tree--next)))
      (should (= (get-text-property next 'denote-tree--prev) (point)))
      (should (= parent (get-text-property (point) 'denote-tree--parent)))))
  ;; "the usual" case, deeply nested node
  ;; '-* temp
  ;;   +-* a
  ;;   | '-* aa
  ;;   |   '-* aaa
  ;;   |     +-* aaab
  ;;   |     '-* aaac
  ;;   +-* b
  (let (prev next parent)
    (with-temp-buffer
      (denote-tree-test-mock--walk-links-macro nil '(("a" "b") ("aa") ("aaa") ("aaab" "aaac"))
        (denote-tree--walk-links
         (buffer-name (current-buffer)) "" t t)
        ;; go to "aaab"
        (goto-line 5)
        (setq prev (marker-position
                    (get-text-property (point) 'denote-tree--prev)))
        (setq next (marker-position
                    (get-text-property (point) 'denote-tree--next)))
        (setq parent (marker-position
                      (get-text-property (point) 'denote-tree--parent))))
      (denote-tree-test-mock--walk-links-macro nil nil
        (denote-tree-redraw))
      ;; how marker changed
      (should (= prev (get-text-property (point) 'denote-tree--prev)))
      (should (= (get-text-property prev 'denote-tree--next) (point)))
      (should (= next (get-text-property (point) 'denote-tree--next)))
      (should (= (get-text-property next 'denote-tree--prev) (point)))
      (should (= parent (get-text-property (point) 'denote-tree--parent)))
      (should (= (get-text-property parent 'denote-tree--child) (point)))))
  ;; "true tree"
  ;; '-* temp
  ;;   +-* a
  ;;   | '-* aa
  ;;   |   '-* aaa
  ;;   |     +-* aaab
  ;;   |     '-* aaac
  ;;   +-* b
  ;; what first pass sees
  ;; '-* temp
  ;;   +-* a
  ;;   '-* b
  (let (parent props)
    (with-temp-buffer
      (denote-tree-test-mock--walk-links-macro nil '(("a" "b"))
        (denote-tree--walk-links
         (buffer-name (current-buffer)) "" t t)
        ;; go to "a"
        (goto-line 2)
        (setq parent (marker-position
                      (get-text-property (point) 'denote-tree--parent))))
      (denote-tree-test-mock--walk-links-macro nil '(("aa") ("aaa") ("aaab" "aaac"))
        (denote-tree-redraw))
      ;; how marker changed
      (save-excursion
        (goto-line 3)
        (setq props (next-single-property-change
                     (line-beginning-position) 'button-data)))
      (should (= (point) (get-text-property props 'denote-tree--parent)))))
  ;; "true tree"
  ;; '-* temp
  ;;   +-* a
  ;;   | '-* aa
  ;;   |   '-* aaa
  ;;   |     +-* aaab
  ;;   |     '-* aaac
  ;;   +-* b
  ;; what first pass sees
  ;; '-* temp
  ;;   +-* a
  ;;   '-* b
  (let (parent props)
    (with-temp-buffer
      (denote-tree-test-mock--walk-links-macro nil '(("a" "b"))
        (denote-tree--walk-links
         (buffer-name (current-buffer)) "" t t)
        ;; go to "a"
        (goto-line 2)
        (setq parent (marker-position
                      (get-text-property (point) 'denote-tree--parent))))
      (denote-tree-test-mock--walk-links-macro nil '(("aa") ("aaa") ("aaab" "aaac"))
        (denote-tree-redraw))
      ;; how marker changed
      (save-excursion
        (goto-line 7)
        (setq props (next-single-property-change
                     (line-beginning-position) 'button-data)))
      (should (= props (get-text-property (point) 'denote-tree--prev)))
      (should (= (get-text-property props 'denote-tree--next) (point)))
      (should (= props (get-text-property (point) 'denote-tree--next)))
      (should (= (get-text-property props 'denote-tree--prev) (point)))
      (should (= parent (get-text-property (point) 'denote-tree--parent)))
      (should (= (get-text-property parent 'denote-tree--child) (point)))))
  ;; "true tree"
  ;; '-* temp
  ;;   +-* a
  ;;   | +-* aa
  ;;   | +-* ab
  ;;   | +-* ac
  ;;   | '-* ad
  ;;   +-* b
  ;; what first pass sees
  ;; '-* temp
  ;;   +-* a
  ;;   | +-* aa
  ;;   | '-* ad
  ;;   '-* b
  (let (props parent)
    (with-temp-buffer
      (denote-tree-test-mock--walk-links-macro nil '(("a" "b") ("aa" "ad"))
        (denote-tree--walk-links
         (buffer-name (current-buffer)) "" t t)
        ;; go to "a"
        (goto-line 2)
        (setq parent (marker-position
                      (get-text-property (point) 'denote-tree--parent))))
      (denote-tree-test-mock--walk-links-macro nil '(("aa" "ab" "ac" "ad"))
        (denote-tree-redraw))
      ;; how marker changed
      (save-excursion
        (goto-line 7)
        (setq props (next-single-property-change
                     (line-beginning-position) 'button-data)))
      (should (= props (get-text-property (point) 'denote-tree--prev)))
      (should (= (get-text-property props 'denote-tree--next) (point)))
      (should (= props (get-text-property (point) 'denote-tree--next)))
      (should (= (get-text-property props 'denote-tree--prev) (point)))
      (should (= parent (get-text-property (point) 'denote-tree--parent)))
      (should (= (get-text-property parent 'denote-tree--child) (point)))))
  ;; redraw entire tree from root
  ;; '-* temp
  ;;   +-* a
  ;;   | '-* aa
  ;;   |   '-* aaa
  ;;   |     +-* aaab
  ;;   |     '-* aaac
  ;;   +-* b
  (let (buffer-string)
    (with-temp-buffer
      (denote-tree-test-mock--walk-links-macro nil '(("a" "b") ("aa") ("aaa") ("aaab" "aaac"))
        (denote-tree--walk-links
         (buffer-name (current-buffer)) "" t t)
        ;; go to "temp"
        (goto-line 1)
        (setq buffer-string
              (buffer-substring-no-properties (point-min) (point-max))))
      (denote-tree-test-mock--walk-links-macro nil '(("a" "b") ("aa") ("aaa") ("aaab" "aaac"))
        (denote-tree-redraw))
      (should
       (equal (buffer-substring-no-properties (point-min) (point-max))
              buffer-string)))))

(ert-deftest denote-tree-test--compare-and-insert-to--no-props-trivial ()
  "Tests for `denote-tree--compare-and-insert-new-to'."
  (with-temp-buffer ;; old buffer
    (let ((old-buffer (buffer-name))
          (new-buffer-contents))
      (insert
         "'-* a\n"
         "  +-* b\n"
         "  +-* c\n"
         "  '-* d\n")
      (with-temp-buffer ;; new-buffer
        (insert
         "'-* a\n"
         "  +-* a1\n"
         "  +-* b\n"
         "  | +-* b1\n"
         "  | '-* b2\n"
         "  +-* c\n"
         "  | '-* c1\n"
         "  |   '-* c1a\n"
         "  '-* d\n")
        (setq new-buffer-contents (buffer-substring (point-min) (point-max)))
        (denote-tree--compare-and-insert-new-to old-buffer 1 1))
      (should
       (equal (buffer-substring (point-min) (point-max))
              new-buffer-contents))))
  (with-temp-buffer ;; old buffer
    (let ((old-buffer (buffer-name))
          (new-buffer-contents))
      (insert
         "'-* a\n"
         "  +-* b\n"
         "  +-* c\n"
         "  '-* d\n")
      (with-temp-buffer ;; new-buffer
        (insert
         "'-* a\n"
         "  +-* a1\n"
         "  +-* b\n"
         "  | +-* b1\n"
         "  | '-* b2\n"
         "  +-* c\n"
         "  | '-* c1\n"
         "  |   '-* c1a\n"
         "  '-* d\n"
         "    '-* d1\n")
        (setq new-buffer-contents (buffer-substring (point-min) (point-max)))
        (denote-tree--compare-and-insert-new-to old-buffer 1 1))
      (should
       (equal (buffer-substring (point-min) (point-max))
              new-buffer-contents)))))

(ert-deftest denote-tree-test--sanitize-deleted-entries ()
  "Tests for `denote-tree--sanitize-deleted-entries'."
  (with-temp-buffer
    (let ((buf (buffer-name))
          (buf-cont))
      (denote-tree-test-mock--walk-links-macro nil '(("a" "b" "c"))
        (denote-tree--walk-links (buffer-name) "" t 3))
      (goto-line 2)
      (with-temp-buffer
        (denote-tree-test-mock--walk-links-macro nil '(("a" "c"))
          (denote-tree--walk-links (buffer-name) "" t 3))
        (goto-line 2)
        (setq buf-cont (buffer-substring-no-properties
                        (line-beginning-position) (point-max)))
        (with-current-buffer (buffer-name)
          (denote-tree--sanitize-deleted-entries buf)))
      (should (equal (buffer-substring-no-properties
                      (line-beginning-position) (point-max))
                     buf-cont))))
  (with-temp-buffer
    (let ((buf (buffer-name))
          (buf-cont))
      (denote-tree-test-mock--walk-links-macro nil '(("a" "b" "d" "c"))
        (denote-tree--walk-links (buffer-name) "" t 3))
      (goto-line 2)
      (with-temp-buffer
        (denote-tree-test-mock--walk-links-macro nil '(("a" "c"))
          (denote-tree--walk-links (buffer-name) "" t 3))
        (goto-line 2)
        (setq buf-cont (buffer-substring-no-properties
                        (line-beginning-position) (point-max)))
        (with-current-buffer (buffer-name)
          (denote-tree--sanitize-deleted-entries buf)))
      (should (equal (buffer-substring-no-properties
                      (line-beginning-position) (point-max))
                     buf-cont))))
  (with-temp-buffer
    (let ((buf (buffer-name))
          (buf-cont))
      (denote-tree-test-mock--walk-links-macro nil '(("a" "b" "c" "d") nil ("b1" "b2"))
        (denote-tree--walk-links (buffer-name) "" t 3))
      (goto-line 2)
      (with-temp-buffer
        (denote-tree-test-mock--walk-links-macro nil '(("a" "d"))
          (denote-tree--walk-links (buffer-name) "" t 3))
        (goto-line 2)
        (setq buf-cont (buffer-substring-no-properties
                        (line-beginning-position) (point-max)))
        (with-current-buffer (buffer-name)
          (denote-tree--sanitize-deleted-entries buf)))
      (should (equal (buffer-substring-no-properties
                      (line-beginning-position) (point-max))
                     buf-cont)))))

(defun denote-tree-test-helper--iterate-over-solutions (lst)
  "Iterate of LST in `denote-tree--link-next-and-prev-node'.

LST looks like (START PROP END)."
  (dolist (pos-prop lst)
    (goto-line (car pos-prop))
    (should
     (= (get-text-property (point) (cadr pos-prop))
        (save-excursion
          (goto-line (caddr pos-prop))
          (next-single-property-change
           (line-beginning-position)
           'button-data))))))

(ert-deftest denote-tree-test--link-next-and-prev-node ()
  "Tests for `denote-tree--link-next-and-prev-node'."
  (with-temp-buffer
    (denote-tree-test-mock--walk-links-macro nil '(("a" "b" "c"))
      (denote-tree--walk-links (buffer-name) "" nil t))
    ;; node "b"
    (denote-tree--link-next-and-prev-node (goto-line 3))
    (denote-tree-test-helper--iterate-over-solutions
     '((2 denote-tree--next 4) (2 denote-tree--prev 4)
       (4 denote-tree--next 2) (4 denote-tree--prev 2))))
  (with-temp-buffer
    (denote-tree-test-mock--walk-links-macro nil '(("a" "b" "c"))
      (denote-tree--walk-links (buffer-name) "" nil t))
    ;; node "a"
    (denote-tree--link-next-and-prev-node (goto-line 2))
    (denote-tree-test-helper--iterate-over-solutions
     '((4 denote-tree--next 3) (4 denote-tree--prev 3)
       (3 denote-tree--next 4) (3 denote-tree--prev 4))))
  (with-temp-buffer
    (denote-tree-test-mock--walk-links-macro nil '(("a" "b" "c"))
      (denote-tree--walk-links (buffer-name) "" nil t))
    ;; node "c"
    (denote-tree--link-next-and-prev-node (goto-line 4))
    (denote-tree-test-helper--iterate-over-solutions
     '((2 denote-tree--next 3) (2 denote-tree--prev 3)
       (3 denote-tree--next 2) (3 denote-tree--prev 2))))
  (with-temp-buffer
    (denote-tree-test-mock--walk-links-macro nil '(("a"))
      (denote-tree--walk-links (buffer-name) "" nil t))
    ;; node "a"
    (goto-line 2)
    (denote-tree--link-next-and-prev-node 17)
    (denote-tree-test-helper--iterate-over-solutions
     '((2 denote-tree--next 2) (2 denote-tree--prev 2)))
    (should-not
     (marker-position (get-text-property (goto-line 1) 'denote-tree--child))))
  (with-temp-buffer
    (denote-tree-test-mock--walk-links-macro nil '(("a" "b"))
      (denote-tree--walk-links (buffer-name) "" nil t))
    ;; node "b"
    (denote-tree--link-next-and-prev-node (goto-line 3))
    (denote-tree-test-helper--iterate-over-solutions
     '((2 denote-tree--next 2)
       (2 denote-tree--prev 2)
       (1 denote-tree--child 2)))))

(provide 'denote-tree-test)
;;; denote-tree-test.el ends here
