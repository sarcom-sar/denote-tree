EMACS              ?= emacs
DEPS               ?= ert denote cl-lib
PACKAGE-FILES      := denote-tree.el denote-tree-edit.el
LOAD-PACKAGE-TEST  := -l denote-tree-test.el -l denote-tree-edit-test.el
RM                 := rm -f

INIT-PACKAGES="(progn \
  (require 'package) \
  (push '(\"melpa\" . \"https://melpa.org/packages/\") package-archives) \
  (package-initialize) \
  (dolist (pkg '(${DEPS})) \
    (unless (package-installed-p pkg) \
      (unless (assoc pkg package-archive-contents) \
	(package-refresh-contents)) \
      (package-install pkg))) \
  (unless package-archive-contents (package-refresh-contents)))"

%.elc: $(PACKAGE-FILES)
	${EMACS} -batch --eval ${INIT-PACKAGES} -L . -f batch-byte-compile $^

all: test lisp

.PHONY: help
help:
	$(info Available targets)
	$(info )
	$(info [all]     Run 'test' and 'lisp' (default))
	$(info clean     Clean the build directory)
	$(info test      Run tests)
	$(info lisp      Byte-compile Elisp sources)
	@:

lisp: %.elc

.PHONY: test
test:
	${EMACS} -Q -batch --eval ${INIT-PACKAGES} -L . ${LOAD-PACKAGE-TEST} -f ert-run-tests-batch-and-exit

.PHONY: clean
clean:
	$(RM) *.elc
