EMACS ?= emacs -Q --batch --load init.el -l org-sql.el

all: test

test:
	${MAKE} stateless
	${MAKE} stateful
	${MAKE} compile

docs:
	${EMACS} \
       -l doc/org-sql-doc.el \
       -f create-docs-file \
       -f org-sql-create-all-erds

stateless:
	${EMACS} -l test/org-sql-test-stateless.el -f buttercup-run-discover

sqlite:
	${EMACS} -l test/org-sql-test-stateful.el \
    -l test/org-sql-test--sqlite.el \
    -f buttercup-run-discover

postgres:
	${EMACS} -l test/org-sql-test-stateful.el \
    -l test/org-sql-test--postgres.el \
    -f buttercup-run-discover

mariadb:
	${EMACS} -l test/org-sql-test-stateful.el \
    -l test/org-sql-test--mariadb.el \
    -f buttercup-run-discover

sqlserver:
	${EMACS} -l test/org-sql-test-stateful.el \
    -l test/org-sql-test--sqlserver.el \
    -f buttercup-run-discover

compile:
	${EMACS} build
	${MAKE} stateless
	${MAKE} sqlite
	${MAKE} postgres
	${MAKE} mariadb
	${MAKE} sqlserver
	${MAKE} clean-elc

clean-elc:
	${EMACS} clean-elc

# install all development packages for the current version
install:
	${EMACS} --eval '(print "Install finished")'

# write lockfile for current emacs version given each repo dependency
freeze:
	${EMACS} -f straight-freeze-versions

thaw:
	${EMACS} -f straight-thaw-versions

.PHONY:	all test unit
