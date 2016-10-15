# whatever flags you want to pass to the C compiler & linker
AM_CFLAGS = -pedantic -Wall -O2
AM_LDFLAGS = -lreadline -lhunspell
SUBDIRS = logic

# this lists the binaries to produce, the (non-PHONY, binary) targets in
# the previous manual Makefile
bin_PROGRAMS = simplebot 
simplebot_SOURCES = simplebot.c simplebot.h logic/pl_if_test.c common.h



