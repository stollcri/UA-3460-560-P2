### 
### This is an automagically generated sample makefile for building lisp as a
### shared library examples using Allegro CL version 8.2.final.18 on a
### macosx86 host with *features* :anymac, :macosx86, :loadacl, :macosx86,
### :smp-macros, :use-thread-libs, :dynload-acl-library, :ssl-support, :dlmac.
### 

CC = cc
AS = as
LD = $(CC)

LNKACL_NAME = liblnkacl.dylib

CFLAGS = -DACL_MAJ_VER=8 -DACL_MIN_VER=2 -DUSE_THREAD_LIBS -DACLSMPBASE -DAcl32Bit -m32 -arch i386

env = DYLD_LIBRARY_PATH=.; export DYLD_LIBRARY_PATH;

LDFLAGS = $(CFLAGS)

LIBRARIES = -framework Cocoa -framework Foundation -lc

RUNLISP := $(shell if test -d ../c; then echo ../lispi -I dcli.dxl; else echo ../../mlisp; fi)

SHLIBS_H_ARG := -I$(shell if test -d ../c; then echo ../c; else echo ../../misc; fi)

all: fact/ftest test

all2: fact/ftest test2

fact/ftest: ftest fact.dxl

ftest: ftest.c
	$(env) $(LD) -o ftest $(SHLIBS_H_ARG) $(LDFLAGS) ftest.c -L. -llnkacl $(LDFLAGS) $(LIBRARIES)

FACT_CMD = '(generate-application "fact" "fact/" \
		(list "lnkacl.fasl" "fact.fasl") \
		:application-type :exe \
		:application-files (list "ftest" "$(LNKACL_NAME)") \
		:read-init-files nil \
		:restart-init-function nil \
		:restart-app-function  nil \
		:include-compiler nil)'

fact.dxl: fact.cl
	rm -f build.tmp
	rm -fr fact
	echo '(compile-file "fact")' >> build.tmp
	echo $(FACT_CMD) >> build.tmp
	echo '(exit 0)' >> build.tmp
	$(env) $(RUNLISP) -L build.tmp
	rm -f build.tmp

test: fact/ftest FORCE
	$(env) if test "`(cd fact; ./ftest 5)`" -eq 120; then \
		echo Test for lnkacl-unix completed successfully.; \
	else \
		echo "lnkacl test failed."; \
		exit 1; \
	fi

test2: fact/ftest FORCE
	$(env) cd fact; ./ftest 5

clean:	FORCE
	rm -f autoloads.out
	rm -f fact.fasl
	rm -f ftest ftest.o
	rm -Rf fact
	rm -f so_locations
	rm -f build.tmp

FORCE:

