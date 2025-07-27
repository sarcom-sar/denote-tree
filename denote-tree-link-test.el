;;; denote-tree-link-test.el --- Test denote-tree-link -*- lexical-binding: t -*-

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
      (should
       (equal (denote-tree-link-insert-after-front-matter)
              (list (point-max) (point-max)))))
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

(ert-deftest denote-tree-link-test--link ()
  "Tests for `denote-tree-link--link'."
  (cl-letf (((symbol-function 'write-file)
             (lambda (_ _)
               nil)))
    (let ((denote-tree--extended-filetype
           denote-tree-link-test-mock--extended-filetype)
          (denote-tree-link-insert-function nil)
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
          (cl-letf (((symbol-function 'find-file-noselect)
                     (lambda (x)
                       (get-buffer to-node-buffer))))
            (denote-tree-link--link from-node-file to-node-file))
          (with-current-buffer to-node-buffer
            (should denote-tree-link-mode)
            (should denote-tree-link--plist)
            (should
             (equal (plist-get denote-tree-link--plist :link-this)
                    from-node-file))
            (should
             (equal (plist-get denote-tree-link--plist :to-this)
                    to-node-file))
            (should (plist-get denote-tree-link--plist :window-config))))))))

(ert-deftest denote-tree-link-test--unlink ()
  "Tests for `denote-tree-link--unlink'."
  (let ((denote-tree--extended-filetype
         (denote-tree--build-extended-filetype
          denote-file-types denote-tree-extend-filetype-with)))
    (with-temp-buffer
      (insert "#+title: f\n"
              "\n"
              "[[denote:12345678T123456][BAR]]\n"
              "\n"
              "Some text no one cares about\n")
      (goto-char (point-min))
      (cl-letf (((symbol-function 'write-file)
                 (lambda (_ _)
                   nil))
                ((symbol-function 'denote-tree-redraw)
                 (lambda ()
                   nil)))
        (denote-tree-link--unlink "12345678T123456" (current-buffer))
        (should
         (equal (concat "#+title: f\n"
                        "\n"
                        "BAR\n"
                        "\n"
                        "Some text no one cares about\n")
                (buffer-substring (point-min) (point-max)))))))
  (let ((denote-tree--extended-filetype
         (denote-tree--build-extended-filetype
          denote-file-types denote-tree-extend-filetype-with)))
    (with-temp-buffer
      (insert "#+title: f\n"
              "\n"
              "[[denote:12345678T123456]]\n"
              "\n"
              "Some text no one cares about\n")
      (goto-char (point-min))
      (cl-letf (((symbol-function 'write-file)
                 (lambda (_ _)
                   nil))
                ((symbol-function 'denote-tree-redraw)
                 (lambda ()
                   nil)))
        (denote-tree-link--unlink "12345678T123456" (current-buffer))
        (goto-line 3)
        (should
         (equal (concat "#+title: f\n"
                        "\n"
                        "\n"
                        "\n"
                        "Some text no one cares about\n")
                (buffer-substring (point-min) (point-max)))))))
  (let ((denote-tree--extended-filetype
         (denote-tree--build-extended-filetype
          denote-file-types denote-tree-extend-filetype-with)))
    (with-temp-buffer
      (insert "#+title: f\n"
              "\n"
              "[[denote:12345678T123456]]\n"
              "\n"
              "Some text no one cares about\n")
      (goto-char (point-min))
      (cl-letf (((symbol-function 'write-file)
                 (lambda (_ _)
                   nil))
                ((symbol-function 'denote-tree-redraw)
                 (lambda ()
                   nil)))
        ;; not testing for errors, only for not clobbering a buffer
        (condition-case nil
            (denote-tree-link--unlink "BAR" (current-buffer))
          (error nil))
        (should
         (equal (concat "#+title: f\n"
                        "\n"
                        "[[denote:12345678T123456]]\n"
                        "\n"
                        "Some text no one cares about\n")
                (buffer-substring (point-min) (point-max)))))))
  (let ((denote-tree--extended-filetype
         (denote-tree--build-extended-filetype
          denote-file-types denote-tree-extend-filetype-with)))
    (with-temp-buffer
      (insert "#+title: f\n"
              "\n"
              "[[denote:12345678T123456][BAR]]\n"
              "\n"
              "Some text no one cares about\n")
      (goto-char (point-min))
      (cl-letf (((symbol-function 'write-file)
                 (lambda (_ _)
                   nil))
                ((symbol-function 'denote-tree-redraw)
                 (lambda ()
                   nil)))
        ;; not testing for errors, only for not clobbering a buffer
        (condition-case nil
            (denote-tree-link--unlink "BAR" (current-buffer))
          (error nil))
        (should
         (equal (concat "#+title: f\n"
                        "\n"
                        "[[denote:12345678T123456][BAR]]\n"
                        "\n"
                        "Some text no one cares about\n")
                (buffer-substring (point-min) (point-max))))))))

(ert-deftest denote-tree-link-test--range ()
  "Tests for `denote-tree-link--range'."
  (with-temp-buffer
    (insert "#+title: f\n"
            "\n"
            "[[denote:FOO][BAR]]\n"
            "\n"
            "Some text no one cares about\n")
    (goto-char (point-min))
    (should
     (equal (denote-tree-link--range "FOO" "BAR" "[[denote:%s][%s]]")
            '(13 . 32))))
  (with-temp-buffer
    (insert "#+title: f\n"
            "\n"
            "[[denote:FOO][BAR]]\n"
            "\n"
            "Some text no one cares about\n")
    (goto-char (point-min))
    (should-error (denote-tree-link--range "blzgh" "BAR" "[[denote:%s][%s]]")))
  (with-temp-buffer
    (insert "#+title: f\n"
            "\n"
            "[[denote:FOO]]\n"
            "\n"
            "Some text no one cares about\n")
    (goto-char (point-min))
    (should
     (equal (denote-tree-link--range "FOO" "BAR" "[[denote:%s][%s]]")
            '(13 . 27))))
  (with-temp-buffer
    (insert "#+title: f\n"
            "\n"
            "[[denote:FOO][this is very arbitrary]]\n"
            "\n"
            "Some text no one cares about\n")
    (goto-char (point-min))
    (should
     (equal (denote-tree-link--range "FOO" ".*?" "[[denote:%s][%s]]")
            '(13 . 51))))
  (with-temp-buffer
    (insert "#+title: f\n"
            "\n"
            "[[denote:FOO]]\n"
            "\n"
            "Some text no one cares about\n")
    (goto-char (point-min))
    (should
     (equal (denote-tree-link--range "FOO" ".*?" "[[denote:%s][%s]]")
            '(13 . 27)))))

(provide 'denote-tree-link-test)
;;; denote-tree-link ends here
