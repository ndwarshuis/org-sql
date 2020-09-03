CASK ?= cask
EMACS ?= emacs

all: test

test:
	${MAKE} unit
	${MAKE} compile
	${MAKE} clean-elc

docs:
	${CASK} exec ${EMACS} -Q -batch \
       -l dev/org-sql-doc.el \
       -f create-docs-file

unit:
	${CASK} exec buttercup -L .

compile:
	${CASK} build
	${MAKE} unit

clean-elc:
	${CASK} clean-elc

.PHONY:	all test unit
