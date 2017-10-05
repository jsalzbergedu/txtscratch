all:
	csc -cc clang ./txtscratch.scm
release:
	csc -02 -cc clang -C 02
.PHONY: all release
