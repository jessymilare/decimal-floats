### -*- Mode: Makefile -*-

### File modified from Lispbuilder-SDL library
### Author: Matthew Danish <mrd@debian.org>
### Modified by: Luke Crook <luke@balooga.com>

all: Makefile.opts
	cp -f Makefile.decNumber decNumber/Makefile
	${MAKE} -C decNumber

clean: Makefile.opts
	${MAKE} -C decNumber clean
	$(RM) Makefile.opts
	$(RM) decNumber/Makefile

Makefile.opts:
	cp -f Makefile.$(DECSYSTEM) Makefile.opts

install:
	${MAKE} -C decNumber install

uninstall:
	${MAKE} -C decNumber uninstall
