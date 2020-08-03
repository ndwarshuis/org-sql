CASK ?= cask

all: test

test:
	${MAKE} unit
	${MAKE} compile
	${MAKE} clean-elc

unit:
	${CASK} exec buttercup -L .

compile:
	${CASK} build
	${MAKE} unit

clean-elc:
	${CASK} clean-elc

.PHONY:	all test unit
