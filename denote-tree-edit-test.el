;;; denote-tree-edit-test.el --- Test denote-tree-edit -*- lexical-binding: t -*-

(require 'denote-tree)
(require 'denote-tree-test)
(require 'denote-tree-edit)
(require 'ert)

;;; Code:

(ert-deftest denote-tree-edit-test--next-prop-match ()
  "Tests for `denote-tree-edit--next-prop-match'."
  (with-temp-buffer
    (insert "'- " (propertize "* " 'foo 'bar) "A\n" "foos")
    (should (equal (denote-tree-edit--next-prop-match 'foo 'bar) nil))
    (goto-char 0)
    (should (equal (denote-tree-edit--next-prop-match 'foo 'bar) 4)))
  (should-not (denote-tree-edit--next-prop-match "foo" 'bar))
  (with-temp-buffer
    (insert "'-" (propertize "* A " 'foo "title") "A TITLE" "\n")
    (goto-char 1)
    (should-not (denote-tree-edit--next-prop-match 'foo 'bar)))
  (with-temp-buffer
    (insert "'-" (propertize "* A " 'foo 'bar) "A TITLE" "\n")
    ;; after propertized bit
    (goto-char 8)
    (should-not (denote-tree-edit--next-prop-match 'foo 'bar))))

(ert-deftest denote-tree-edit-test--after-button ()
  "Tests for `denote-tree-edit--after-button'."
  (with-temp-buffer
    (insert "'-" (propertize "* " 'button-data "foo") "A\n")
    (should (equal (denote-tree-edit--after-button 0) 5)))
  (with-temp-buffer
    (insert "'-" (propertize "* " 'button "bar") "A\n")
    (should (equal (denote-tree-edit--after-button 0) nil))))

(ert-deftest denote-tree-edit-test--set-from-tree ()
  "Tests for `denote-tree-edit--set-from-tree'."
  (let ((dat-struct '()))
    (with-temp-buffer
      (insert
       "'-*"
       (propertize "Foo" 'denote-tree--type 'tootle)
       " "
       (propertize "Bar" 'denote-tree--type 'bartle)
       "\n")
      (goto-char 0)
      (denote-tree-edit--set-from-tree
       '(tootle bartle)
       (lambda (s e t)
         (setq dat-struct
               (append
                dat-struct (list (buffer-substring-no-properties s e))))))
      (should (equal dat-struct '("Foo" "Bar")))))
  (let ((dat-struct '()))
    (with-temp-buffer
      (insert
       "'-*"
       (propertize "Foo" 'denote-tree--type 'tootle)
       " "
       (propertize "Baz" 'denote-tree--type 'baztle)
       "\n")
      (goto-char 0)
      (denote-tree-edit--set-from-tree
       '(tootle bartle)
       (lambda (s e t)
         (setq dat-struct
               (append
                dat-struct (list (buffer-substring-no-properties s e))))))
      (should (equal dat-struct '("Foo")))))
  (let ((dat-struct '()))
    (with-temp-buffer
      (insert
       "'-*"
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
         (setq dat-struct
               (append
                dat-struct (list (buffer-substring-no-properties s e))))))
      (should (equal dat-struct '("Foo" "Bar"))))))

(ert-deftest denote-tree-edit-test--widgetize-line ()
  "Tests for `denote-tree-edit--widgetize-line'."
  (let ((denote-tree-node-description '(title))
        (denote-tree-edit--current-note '((title . "I am a title")))
        (denote-tree-edit--current-line 1))
    (with-temp-buffer
      (insert
       "'-"
       (propertize "* " 'button-data "foo")
       (propertize "I am a title" 'denote-tree-edit--type 'title)
       "\n")
      (denote-tree-edit--widgetize-line)
      (should (widgetp (widget-at 5)))))
  (let ((denote-tree-node-description '(title iden))
        (denote-tree-edit--current-note
         '((title . "I am a title") (iden . "I am iden")))
        (denote-tree-edit--current-line 1))
    (with-temp-buffer
      (insert
       "'-"
       (propertize "* " 'button-data "foo")
       (propertize "I am a title" 'denote-tree-edit--type 'title)
       " "
       (propertize "I am a iden" 'denote-tree-edit--type 'iden)
       "\n")
      (denote-tree-edit--widgetize-line)
      (should (widgetp (widget-at 19)))))
  (let ((denote-tree-node-description '(title "foo" iden))
        (denote-tree-edit--current-note
         '((title . "I am a title") (iden . "I am iden")))
        (denote-tree-edit--current-line 1))
    (with-temp-buffer
      (insert
       "'-"
       (propertize "* " 'button-data "foo")
       (propertize "I am a title" 'denote-tree-edit--type 'title)
       " foo "
       (propertize "I am a iden" 'denote-tree-edit--type 'iden)
       "\n")
      (denote-tree-edit--widgetize-line)
      (should (not (widgetp (widget-at 19))))
      (should (widgetp (widget-at 23)))))
  (let ((denote-tree-node-description '(title "foo" iden))
        (denote-tree-edit--current-note
         '((title . "I am a title") (iden . "I am a iden")))
        (denote-tree-edit--current-line 1))
    (with-temp-buffer
      ;; not possible in theory, because `denote-tree-node-description'
      ;; determines order of arguments
      (insert
       "'-"
       (propertize "* " 'button-data "foo")
       (propertize "I am a iden" 'denote-tree-edit--type 'iden)
       " foo "
       (propertize "I am a title" 'denote-tree-edit--type 'title)
       "\n")
      (denote-tree-edit--widgetize-line)
      (should (equal (widget-get (widget-at 5) :value) "I am a title"))
      (should (equal (widget-get (widget-at 23) :value) "I am a iden")))))

(ert-deftest denote-tree-edit-test--restore-line ()
  "Tests for `denote-tree-edit--restore-line'."
  (let ((denote-tree-edit--current-line 1)
        (denote-tree-edit--current-note '((title . "foo") (tootle . "bar")))
        (denote-tree-node-description '(title "far" tootle)))
    (with-temp-buffer
      (insert "'-" (propertize "* " 'button-data "fooz"))
      (denote-tree-edit--restore-line)
      (should (equal (buffer-substring-no-properties 5 16) "foo far bar"))))
  (let ((denote-tree-edit--current-line 1)
        (denote-tree-edit--current-note '((title . "foo") (tootle . "bar")))
        (denote-tree-node-description '(title "far")))
    (with-temp-buffer
      (insert "'-" (propertize "* " 'button-data "fooz"))
      (denote-tree-edit--restore-line)
      (should (equal (buffer-substring-no-properties 5 12) "foo far")))))

(ert-deftest denote-tree-edit-test--dewidgetize-line ()
  "Tests for `denote-tree-edit--dewidgetize-line'."
  (with-temp-buffer
    (let ((inhibit-read-only t)
          (denote-tree-edit--current-line 1)
          (denote-tree-edit--current-note '((title . "foo")))
          (denote-tree-node-description '(title)))
      (insert
       "'-"
       (propertize "* " 'button-data "foo")
       (propertize "foo" 'denote-tree--type 'title)
       "\n")
      (denote-tree-edit--widgetize-line)
      (denote-tree-edit--dewidgetize-line)
      (should (not (overlays-in (point-min) (point-max))))))
  (with-temp-buffer
    (let ((inhibit-read-only t)
          (denote-tree-edit--current-line 1)
          (denote-tree-edit--current-note '((title . "foo") (tootle . "bar")))
          (denote-tree-node-description '(title "far" tootle)))
      (insert
       "'-"
       (propertize "* " 'button-data "fooz")
       (propertize "foo" 'denote-tree--type 'title)
       " "
       (propertize "bar" 'denote-tree--type 'tootle)
       "\n")
      (denote-tree-edit--widgetize-line)
      (denote-tree-edit--dewidgetize-line)
      (should (not (overlays-in (point-min) (point-max))))))
  (with-temp-buffer
    (let ((inhibit-read-only t)
          (denote-tree-edit--current-line 1)
          (denote-tree-edit--current-note '((title . "foo") (tootle . "bar")))
          (denote-tree-node-description '(title "far")))
      (insert
       "'-"
       (propertize "* " 'button-data "fooz")
       (propertize "foo" 'denote-tree--type 'barzle)
       "far\n")
      (denote-tree-edit--widgetize-line)
      (denote-tree-edit--dewidgetize-line)
      (should (not (overlays-in (point-min) (point-max)))))))

(ert-deftest denote-tree-edit-test--save-from-widgets ()
  "Tests for `denote-tree-edit--save-from-widgets'."
  (cl-letf (((symbol-function 'denote-tree-edit--construct-type-widget-alist)
             (lambda (_) '((foo . "bar") (bar . "baz") (baz . "foo")))))
    (should
     (equal (denote-tree-edit--save-from-widgets '((foo) (bar) (baz)) 1)
            '((baz . "foo") (bar . "baz") (foo . "bar")))))
  (cl-letf (((symbol-function 'denote-tree-edit--construct-type-widget-alist)
             (lambda (_)
               `((foo . ,(propertize "bar" 'a 'b))
                 (bar . "baz")
                 (baz . "foo")))))
    (should
     (equal (denote-tree-edit--save-from-widgets '((foo) (bar) (baz)) 1)
            `((baz . "foo") (bar . "baz") (foo . ,(propertize "bar" 'a 'b))))))
  (cl-letf (((symbol-function 'denote-tree-edit--construct-type-widget-alist)
             (lambda (_)
               `((foo . "bar")
                 (bar . "baz")
                 (baz . ,(propertize "foo" 'a 'b))))))
    (should
     (equal (denote-tree-edit--save-from-widgets '((foo . "faz") (bar) (baz)) 1)
            `((baz . ,(propertize "foo" 'a 'b)) (bar . "baz") (foo . "bar")))))
  (cl-letf (((symbol-function 'denote-tree-edit--construct-type-widget-alist)
             (lambda (_) '((foo . "bar") (bar) (baz . "foo")))))
    (should
     (equal (denote-tree-edit--save-from-widgets '((foo) (bar . "foz") (baz)) 1)
            '((baz . "foo") (bar . "foz") (foo . "bar")))))
  (cl-letf (((symbol-function 'denote-tree-edit--construct-type-widget-alist)
             (lambda (_)
               `((foo . "bar")
                 (bar . ,(propertize "foz" 'a 'b))
                 (baz . "foo")))))
    (should
     (equal (denote-tree-edit--save-from-widgets
             `((foo) (bar . ,(propertize "foz" 'c 'd)) (baz)) 1)
            `((baz . "foo") (bar . ,(propertize "foz" 'a 'b)) (foo . "bar")))))
  (cl-letf (((symbol-function 'denote-tree-edit--construct-type-widget-alist)
             (lambda (_) `((foo . "bar") (bar) (baz . "foo")))))
    (should
     (equal (denote-tree-edit--save-from-widgets `((foo) (bar) (baz)) 1)
            `((baz . "foo") (bar) (foo . "bar"))))))

(ert-deftest denote-tree-edit-test--fix-current-note ()
  "Tests for `denote-tree-edit--fix-current-note'."
  (cl-letf (((symbol-function 'insert-file-contents) (lambda (_) nil))
            ((symbol-function 'denote-tree--find-filetype)
             (lambda (_) (assq 'org denote-file-types))))
    (should
     (equal (denote-tree-edit--fix-current-note
             '((keywords . ":d:e:f:")))
            '((keywords "d" "e" "f")))))
  (cl-letf (((symbol-function 'insert-file-contents) (lambda (_) nil))
            ((symbol-function 'denote-tree--find-filetype)
             (lambda (_) (assq 'org denote-file-types))))
    (should
     (equal (denote-tree-edit--fix-current-note
             '((title . "foo") (keywords . ":d:e:f:")))
            '((title . "foo") (keywords "d" "e" "f")))))
  (cl-letf (((symbol-function 'insert-file-contents) (lambda (_) nil))
            ((symbol-function 'denote-tree--find-filetype)
             (lambda (_) (assq 'org denote-file-types))))
    (should
     (equal (denote-tree-edit--fix-current-note
             '((title . "foo") (keywords)))
            '((title . "foo") (keywords))))))


(provide 'denote-tree-edit-test)
;;; denote-tree-edit-test.el ends here
