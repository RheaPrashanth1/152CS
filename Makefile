all:
                        bison -v -d --file-prefix=y pt2.y
                        flex m.lex
                        gcc -o parser y.tab.c lex.yy.c -lfl     
                        cat mytest.min | parser
clean:          
                        rm -rf lex.yy.c lexer
                        rm -rf parser y.output y.tab.h y.tab.c
