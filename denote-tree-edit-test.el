;;; denote-tree-edit-test.el --- Test denote-tree-edit -*- lexical-binding: t -*-

(require 'denote-tree)
(require 'denote-tree-edit)
(require 'ert)

;;; Code:

(ert-deftest denote-tree-edit-test--prop-match ()
  "Tests for `denote-tree-edit--prop-match'."
  (with-temp-buffer
    (insert "'- "
            (propertize "* " 'foo 'bar)
            "A\n"
            "foos")
    (should (equal (denote-tree-edit--prop-match 'foo 'bar)
                   nil))
    (goto-char 0)
    (should (equal (denote-tree-edit--prop-match 'foo 'bar)
                   4)))
  (should (equal (denote-tree-edit--prop-match "foo" 'bar)
                 nil))
  (with-temp-buffer
    (insert "'-"
            (propertize "* A " 'foo "title")
            "A TITLE"
            "\n")
    (goto-char 1)
    (should (equal (denote-tree-edit--prop-match 'foo 'bar)
                   nil)))
  (with-temp-buffer
    (insert "'-"
            (propertize "* A " 'foo 'bar)
            "A TITLE"
            "\n")
    ;; after propertized bit
    (goto-char 8)
    (should (equal (denote-tree-edit--prop-match 'foo 'bar)
                   nil))))

(ert-deftest denote-tree-edit-test--after-button ()
  "Tests for `denote-tree-edit--after-button'."
  (with-temp-buffer
    (insert "'-"
            (propertize "* " 'button-data "foo")
            "A\n")
    (should (equal (denote-tree-edit--after-button 0)
                   5)))
  (with-temp-buffer
    (insert "'-"
            (propertize "* " 'button "bar")
            "A\n")
    (should (equal (denote-tree-edit--after-button 0)
                   nil))))

(ert-deftest denote-tree-edit-test--set-from-tree ()
  "Tests for `denote-tree-edit--set-from-tree'."
  (let ((dat-struct '()))
    (with-temp-buffer
      (insert "'-*"
              (propertize "Foo" 'denote-tree--type 'tootle)
              " "
              (propertize "Bar" 'denote-tree--type 'bartle)
              "\n")
      (goto-char 0)
      (denote-tree-edit--set-from-tree
       '(tootle bartle)
       (lambda (s e t)
         (setq dat-struct (append
                           dat-struct
                           (list (buffer-substring-no-properties s e))))))
      (should (equal dat-struct
                     '("Foo" "Bar")))))
  (let ((dat-struct '()))
    (with-temp-buffer
      (insert "'-*"
              (propertize "Foo" 'denote-tree--type 'tootle)
              " "
              (propertize "Baz" 'denote-tree--type 'baztle)
              "\n")
      (goto-char 0)
      (denote-tree-edit--set-from-tree
       '(tootle bartle)
       (lambda (s e t)
         (setq dat-struct (append
                           dat-struct
                           (list (buffer-substring-no-properties s e))))))
      (should (equal dat-struct
                     '("Foo")))))
  (let ((dat-struct '()))
    (with-temp-buffer
      (insert "'-*"
              (propertize "Foo" 'denote-tree--type 'tootle)
              " "
              (propertize "Baz" 'denote-tree--type 'baztle)
              " "
              (propertize "Bar" 'denote-tree--type 'bartle)
              "\n")
      (goto-char 0)
      (denote-tree-edit--set-from-tree
       ;; nil here is allowed, because normally this list is built from
       ;; `denote-tree-node-description' and will /always/ have this
       ;; arrangement, none the less, it should for arbitrary output
       '(tootle nil bartle)
       (lambda (s e t)
         (setq dat-struct (append
                           dat-struct
                           (list (buffer-substring-no-properties s e))))))
      (should (equal dat-struct
                     '("Foo" "Bar"))))))

(ert-deftest denote-tree-edit-test--widgetize-line ()
  "Tests for `denote-tree-edit--widgetize-line'."
  (let ((denote-tree-node-description '(title))
        (denote-tree-edit--current-note '((title . "I am a title")))
        (denote-tree-edit--current-line 1))
    (with-temp-buffer
      (insert "'-"
              (propertize "* " 'button-data "foo")
              (propertize "I am a title" 'denote-tree-edit--type 'title)
              "\n")
      (denote-tree-edit--widgetize-line)
      (should (widgetp (widget-at 5)))))
  (let ((denote-tree-node-description '(title iden))
        (denote-tree-edit--current-note '((title . "I am a title")
                                          (iden . "I am iden")))
        (denote-tree-edit--current-line 1))
    (with-temp-buffer
      (insert "'-"
              (propertize "* " 'button-data "foo")
              (propertize "I am a title" 'denote-tree-edit--type 'title)
              " "
              (propertize "I am a iden" 'denote-tree-edit--type 'iden)
              "\n")
      (denote-tree-edit--widgetize-line)
      (should (widgetp (widget-at 19)))))
  (let ((denote-tree-node-description '(title "foo" iden))
        (denote-tree-edit--current-note '((title . "I am a title")
                                          (iden . "I am iden")))
        (denote-tree-edit--current-line 1))
    (with-temp-buffer
      (insert "'-"
              (propertize "* " 'button-data "foo")
              (propertize "I am a title" 'denote-tree-edit--type 'title)
              " foo "
              (propertize "I am a iden" 'denote-tree-edit--type 'iden)
              "\n")
      (denote-tree-edit--widgetize-line)
      (should (not (widgetp (widget-at 19))))
      (should (widgetp (widget-at 23)))))
  (let ((denote-tree-node-description '(title "foo" iden))
        (denote-tree-edit--current-note '((title . "I am a title")
                          (iden . "I am a iden")))
        (denote-tree-edit--current-line 1))
    (with-temp-buffer
      ;; not possible in theory, because `denote-tree-node-description'
      ;; determines order of arguments
      (insert "'-"
              (propertize "* " 'button-data "foo")
              (propertize "I am a iden" 'denote-tree-edit--type 'iden)
              " foo "
              (propertize "I am a title" 'denote-tree-edit--type 'title)
              "\n")
      (denote-tree-edit--widgetize-line)
      (should (equal (widget-get (widget-at 5) :value)
                     "I am a title"))
      (should (equal (widget-get (widget-at 23) :value)
                     "I am a iden")))))

(provide 'denote-tree-edit-test)
;;; denote-tree-edit-test.el ends here
