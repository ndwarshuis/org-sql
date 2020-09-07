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

unit:
	${CASK} exec buttercup -L . -l dev/org-sql-test-internal.el

sqlite:
	${CASK} exec buttercup -L . -l dev/org-sql-test-sqlite.el

compile:
	${CASK} build
	${MAKE} unit
	${MAKE} sqlite

clean-elc:
	${CASK} clean-elc

.PHONY:	all test unit
