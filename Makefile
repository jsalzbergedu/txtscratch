CC=clang
OPT=-O3
lib: base.scm
	csc $(OPT) -cc $(CC) -C $(OPT) ./base.scm
all: base.so txtscratch.scm
	csc -cc $(CC) -L base.so ./txtscratch.scm
release: lib txtscratch.scm
	csc $(OPT) -cc $(CC) -C $(OPT) -L base.so ./txtscratch.scm
.PHONY: all release lib
