# this lists the binaries to produce, the (non-PHONY, binary) targets in
# the previous manual Makefile
LEX=flex

all: exp1.tab.c lex.yy.c
.PHONY: all

exp1.tab.c exp1.tab.h: exp1.y
	bison -g -d exp1.y

lex.yy.c lex.yy.h: lexp1.l exp1.tab.h
	flex --header-file=lex.yy.h lexp1.l

