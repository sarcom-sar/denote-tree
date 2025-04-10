;;; denote-tree-link-test.el --- Test denote-tree -*- lexical-binding: t -*-

(require 'denote)
(require 'denote-tree-link)
(require 'erc)

;;; Code

(defvar denote-tree-link-test-mock--extended-filetype
  (denote-tree--build-extended-filetype
   denote-file-types denote-tree-extend-filetype-with))

(ert-deftest denote-tree-link-test-insert-at-eof ()
  "Tests for `denote-tree-link-insert-at-eof'."
  (with-temp-buffer
    (insert "\n")
    (should
     (equal (denote-tree-link-insert-at-eof)
            '(2 2))))
  (with-temp-buffer
    (should
     (equal (denote-tree-link-insert-at-eof)
            '(1 1)))))

(ert-deftest denote-tree-link-test-insert-after-front-matter ()
  "Tests for `denote-tree-insert-after-front-matter'."
  (let ((denote-tree--extended-filetype denote-tree-link-test-mock--extended-filetype))
    (with-temp-buffer
      (insert "\n")
      (should-not (denote-tree-link-insert-after-front-matter)))
    (with-temp-buffer
      (let ((proper-len (+ 2 (length denote-org-front-matter))))
        (insert denote-org-front-matter
                "\n"
                "FOO BAR BAZ\n")
        (should
         (equal (denote-tree-link-insert-after-front-matter)
                (list proper-len proper-len)))
        (should
         (equal (char-after 86)
                ?F))))))

(provide 'denote-tree-link-test)
;;; denote-tree-link ends here
