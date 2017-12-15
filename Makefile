cmplr = g++
hwo = HW8

all: $(hwo) PP
clean:
	rm lex.yy.c flex-lisp parser.tab.bison $(hwo) expression asm out.spim parser.output pparser.output pparser.tab.bison test.out plex.yy.c *preprocesspout PP

reset:
	reset

$(hwo):	lex.yy.c expression
	$(cmplr) asm expression lex.yy.c -lfl -std=c++17 -o $(hwo)

lex.yy.c:	grammar.flex parser.tab.bison Makefile
	flex grammar.flex

expression: expression.cpp expression.hpp asm
	$(cmplr) asm -c expression.cpp -std=c++17 -o expression

asm: ASM.hpp ASM.cpp
	$(cmplr) -c ASM.cpp -std=c++17 -o asm

parser.tab.bison:	parser.bison Makefile
	bison -k -r all parser.bison

PP: plex.yy.c
	$(cmplr) plex.yy.c -lfl -std=c++17 -o PP

plex.yy.c: pgrammar.flex pparser.tab.bison Makefile
	flex -o plex.yy.c pgrammar.flex

pparser.tab.bison: pparser.bison Makefile
	bison -k -r all pparser.bison
