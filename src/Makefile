# this lists the binaries to produce, the (non-PHONY, binary) targets in
# the previous manual Makefile

CFLAGS = -pedantic -Wall -O2
LDFLAGS = -lreadline -lhunspell
pkg-config := $(shell pkg-config --libs swipl)

all: simplebot
.PHONY: all

simplebot: simplebot.o logic/pl_if_test.o
	gcc $(LDFLAGS) $(pkg-config) -o simplebot simplebot.o logic/pl_if_test.o

simplebot.o: simplebot.c simplebot.h logic/pl_if_test.h common.h
	gcc -c $(CFLAGS) -o simplebot.o simplebot.c

logic/pl_if_test.o: logic/pl_if_test.c
	cd logic;make all

