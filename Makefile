CASK ?= cask
EMACS ?= emacs

all: test

test:
	${MAKE} unit
	${MAKE} sqlite
	${MAKE} compile
	${MAKE} clean-elc

docs:
	${CASK} exec ${EMACS} -Q -batch \
       -l dev/org-sql-doc.el \
       -f create-docs-file

stateless:
	${CASK} exec buttercup -L . -l dev/org-sql-test-stateless.el

stateful:
	${CASK} exec buttercup -L . -l dev/org-sql-test-stateful.el

compile:
	${CASK} build
	${MAKE} stateless
	${MAKE} stateful

clean-elc:
	${CASK} clean-elc

.PHONY:	all test unit
