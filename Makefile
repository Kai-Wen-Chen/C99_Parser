all:
	flex hw2.l
	byacc -d -v hw2.y
	gcc -o parser lex.yy.c y.tab.c
clean:
	rm -f lex.yy.c parser y.tab.h y.tab.c y.output
