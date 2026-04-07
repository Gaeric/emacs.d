EMACS ?= emacs

compile:
	$(EMACS) -Q --batch -L . -f batch-byte-compile *.el

test:
	$(EMACS) -Q -L . -l wesl-ts-mode.el -l wesl-tests.el

run:
	$(EMACS) -Q -L . -l wesl-ts-mode.el
