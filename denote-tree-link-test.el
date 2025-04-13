;;; denote-tree-link-test.el --- Test denote-tree -*- lexical-binding: t -*-

(require 'denote)
(require 'denote-tree-link)
(require 'ert)
(require 'ert-x)
(require 'calendar)

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
         (equal (char-after proper-len)
                ?F))))))

(ert-deftest denote-tree-link-test--do-the-link ()
  "Tests for `denote-tree-link--do-the-link'."
  (let ((denote-tree--extended-filetype
         denote-tree-link-test-mock--extended-filetype)
        (id (denote-get-identifier (calendar-current-date))))
    ;; normal linking of form-node to to-node
    (ert-with-temp-file from-node-file
      :buffer from-node-buffer
      :suffix ".org"
      :prefix (concat id "--")
      (insert "#+title: from-node-buffer\n"
              "#+date:  2137-12-14\n"
              "\n"
              "Bye!\n")
      (ert-with-temp-file to-node-file
        :buffer to-node-buffer
        :suffix ".org"
        (insert "#+title: to-node-buffer\n"
                "#+date:  2137-12-15\n"
                "\n"
                "Hello there!\n")
        (denote-tree-link--do-the-link
         (point-max) (point-max) from-node-file)
        (goto-char (point-min))
        (should (search-forward id nil t))))
    ;; now select a range
    (ert-with-temp-file from-node-file
      :buffer from-node-buffer
      :suffix ".org"
      :prefix (concat id "--")
      (insert "#+title: from-node-buffer\n"
              "#+date:  2137-12-14\n"
              "\n"
              "Bye!\n")
      (ert-with-temp-file to-node-file
        :buffer to-node-buffer
        :suffix ".org"
        (insert "#+title: to-node-buffer\n"
                "#+date:  2137-12-15\n"
                "\n"
                "Hello there!\n")
        (goto-char (point-min))
        (search-forward "Hello")
        (denote-tree-link--do-the-link
         (line-beginning-position) (point) from-node-file)
        (progn
          (goto-char (point-min))
          (should (search-forward id nil t)))
        (progn
          (goto-char (point-min))
          (should (search-forward "Hello" nil t)))))
    ;; it tries to link to itself
    ;; yes, -link--do-the-link should not care about it
    (ert-with-temp-file from-node-file
      :buffer from-node-buffer
      :suffix ".org"
      :prefix (concat id "--")
      (insert "#+title: from-node-buffer\n"
              "#+date:  2137-12-14\n"
              "\n"
              "Bye!\n")
      (denote-tree-link--do-the-link
       (point-max) (point-max) from-node-file)
      (goto-char (point-min))
      (should (search-forward id nil t)))))

(provide 'denote-tree-link-test)
;;; denote-tree-link ends here
