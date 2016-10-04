
pkg-config := $(shell pkg-config --cflags --libs swipl)

all:iftest
.PHONY:all

iftest: pl_if_test.c
	gcc $(pkg-config) -o iftest pl_if_test.c
