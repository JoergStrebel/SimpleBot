all: iftest
.PHONY: all

iftest: pl_if_test.c
	gcc -I/home/jstrebel/devel/prolog/swipl-7.2.3/include/ -L/usr/local/lib64/swipl-7.2.3/lib/x86_64-linux/ -lswipl -o iftest pl_if_test.c
