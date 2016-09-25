# this lists the binaries to produce, the (non-PHONY, binary) targets in
# the previous manual Makefile
LEX=flex

all: exp1.tab.c lex.yy.c

exp1.tab.c: exp1.y
	bison -f exp1.y

lex.yy.c: lexp1.l
	flex --header-file=lex.yy.h lexp1.l

