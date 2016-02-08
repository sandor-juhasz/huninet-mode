emacs ?= emacs
all: test

test: clean
	cask exec emacs -Q -batch -L . -l test/huninet-test.el -f ert-run-tests-batch-and-exit

compile:
	$(emacs) -Q -batch -f batch-byte-compile huninet.el

clean:
	rm -f *.elc

.PHONY:	all test
