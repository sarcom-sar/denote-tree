(require 'denote-tree)
(require 'ert)

(ert-deftest denote-tree--default-props-test ()
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

