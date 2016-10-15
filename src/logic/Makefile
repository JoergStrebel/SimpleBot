
pkg-config := $(shell pkg-config --cflags --libs swipl)

all:pl_if_test.o
.PHONY:all

pl_if_test.o: pl_if_test.c
	gcc $(pkg-config) -c -o pl_if_test.o pl_if_test.c
