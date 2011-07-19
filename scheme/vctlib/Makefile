#
# Makefile for vctlib library
# Copyright 2006-2007
# Author: Pierre-Alexandre Fournier
# vctlib@carretechnologies.com

CFLAGS=-O4 -Wall -W -Wno-unused -g -L/usr/local/Gambit-C/current/lib/ -I/usr/local/Gambit-C/current/include/
SHAREFLAGS=-fPIC  -shared -D___LIBRARY -D___SHARED
LIBS=-lm -ldl -lgambc -lutil -lfftw3 -lfftw3f -lfftw3l -lblas
CC=gcc
GSC=gsc
GSCFLAGS=-warnings -link
VERSION=1.2

all: utils.o1 vct.o1 vct-C.o1 vct-blas.o1 matrix.o1 matrix-ndim.o1 ppm.o1

utils.o1: utils.scm
	${GSC} -warnings utils.scm

vct.o1: vct.scm
	${GSC} -warnings vct.scm

vct-C.o1: vct-C.scm
	${GSC} -warnings vct-C.scm

vct-blas.o1: vct-blas.scm
	${GSC} -warnings vct-blas.scm

matrix.o1: matrix.scm
	${GSC} -warnings matrix.scm

matrix-ndim.o1: matrix-ndim.scm
	${GSC} -warnings matrix-ndim.scm

ppm.o1: ppm.scm
	${GSC} -warnings ppm.scm

clean: 
	rm -f *.o*

#TODO!
#install:
#	rm -f /usr/local/include/libsilexvoice.h
#	rm -f /usr/local/lib/libsilexvoice.so
#	rm -f /usr/include/libsilexvoice.h
#	rm -f /usr/lib/libsilexvoice.so
#	cp -v libsilexvoice.so.${VERSION} /usr/lib/
#	ln -vs /usr/lib/libsilexvoice.so.${VERSION} /usr/lib/libsilexvoice.so
#	cp -v libsilexvoice.h  /usr/include/
#	cp -v libsilexvoice_error_codes.h  /usr/include/
#	@echo 
#	@echo " Install successful."
#	/sbin/ldconfig
#	@echo 
#	@echo " Library libsilexvoice is ready."
