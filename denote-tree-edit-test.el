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

(provide 'denote-tree-edit-test)
;;; denote-tree-edit-test.el ends here
