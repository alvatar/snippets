# Makefile                  -*- Makefile -*-

# Created:    <2003-01-15 10:25:01 foof>
# Time-stamp: <2003-01-26 21:55:13 foof>
# Author:     Alex Shinn <foof@synthcode.com>

.PHONY: test clean distclean dist install

GOSH = gosh
GAUCHE_CONFIG = gauche-config
INSTALL = /usr/bin/install -c
INSTALL_TYPE = site
SCM_INSTALL_DIR = `$(GAUCHE_CONFIG) --$(INSTALL_TYPE)libdir`
VERSION=`cat VERSION`
TESTENV=

# nothing to build
all:

install: all
	@for f in `cd lib && find . -name \*.scm`; do      \
	  $(INSTALL) -D -m 644 lib/$$f $(SCM_INSTALL_DIR)/$$f; \
	done

test: all
	cd test;                  \
	for f in t-*.scm; do      \
	  $(TESTENV) $(GOSH) $$f; \
	done

clean:
	rm -f `find . -name \*~`

distclean: clean

# build distribution tarballs
dist: all distclean
	dir=`basename $$PWD`             && \
	name=$${dir}-$(VERSION)          && \
	cd ..                            && \
	tar cjvf $$name.tar.bz2 -h --exclude=CVS $$dir && \
	cd $$dir

