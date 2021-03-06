CASK ?= cask
EMACS ?= emacs

all: test

test:
	${MAKE} stateless
	${MAKE} stateful
	${MAKE} compile

docs:
	${CASK} exec ${EMACS} -Q -batch -L . \
       -l doc/org-sql-doc.el \
       -f create-docs-file \
       -f org-sql-create-all-erds

stateless:
	${CASK} exec buttercup -L . -l test/org-sql-test-stateless.el

stateful:
	${CASK} exec buttercup -L . -l test/org-sql-test-stateful.el

compile:
	${CASK} build
	${MAKE} stateless
	${MAKE} stateful
	${MAKE} clean-elc

clean-elc:
	${CASK} clean-elc

.PHONY:	all test unit
