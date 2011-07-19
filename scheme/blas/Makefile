#
# Makefile for cblas module
# Pierre-Alexandre Fournier
# fournier@carretechnologies.com
# Copyright 2007
#

GSC=gsc
GSCFLAGS=-warnings -ld-options "-lblas"
VERSION=1.0

all: blas.o1

blas.o1: blas.scm
	${GSC} ${GSCFLAGS} blas.scm

tarball:
	(cd ..;							\
	tar cvvf gambit-blas-${VERSION}.tar blas/blas.scm	\
	blas/Makefile blas/blas.html blas/README;		\
	gzip --verbose gambit-blas-${VERSION}.tar;		\
	mv gambit-blas-${VERSION}.tar.gz blas/ ;)

clean: 
	rm -f *.o*
