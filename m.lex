%{

#include "y.tab.h"
int position = 1;
int currentLine = 1;
%}

Letter [a-zA-Z]
Digit [0-9]
Comm   ##.* 
%%
[ \t ]+ { position = position + yyleng;}
{Comm} { position = position + yyleng;}
function        { position += yyleng; return FUNCTION; }
beginparams     { position += yyleng; return BEGINPARAMS; }
endparams       { position += yyleng; return ENDPARAMS; }
beginlocals     {position += yyleng; return BEGINLOCALS; }
endlocals       { position += yyleng; return ENDLOCALS; }
beginbody       { position += yyleng; return BEGINBODY; }
endbody         { position += yyleng; return ENDBODY; }
integer         { position += yyleng; return INTEGER; }
array           { position += yyleng; return ARRAY; }
of              { position += yyleng; return OF; }
if              { position += yyleng; return IF; }
then            { position += yyleng; return THEN; }
endif           { position += yyleng; return ENDIF; }
else            { position += yyleng; return ELSE; }
while           { position += yyleng; return WHILE; }
do              { position += yyleng; return DO; }
beginloop       { position += yyleng; return BEGINLOOP; }
endloop         { position += yyleng; return ENDLOOP; }
continue        { position += yyleng; return CONTINUE; }
read            { position += yyleng; return READ; }
write           { position += yyleng; return WRITE; }
and             { position += yyleng; return AND; }
or              { position += yyleng; return OR; }
not             { position += yyleng; return NOT; }
true            { position += yyleng; return TRUE; }
false           { position += yyleng; return FALSE; }
return          { position += yyleng; return RETURN; }

"-"             { position += yyleng; return SUB; }
"+"             { position += yyleng; return ADD; }
"*"             { position += yyleng; return MULT; }
"/"             { position += yyleng; return DIV; }
"%"             { position += yyleng; return MOD; }

"=="    { position += yyleng; return EQ; }
"<>"    { position += yyleng; return NEQ; }
"<"             { position += yyleng; return LT; }
">"             { position += yyleng; return GT; }
"<="    { position += yyleng; return LTE; }
">="    { position += yyleng; return GTE; }
";"             { position += yyleng; return SEMICOLON; }
":"             { position += yyleng; return COLON; }
","             { position += yyleng; return COMMA; }
"("             { position += yyleng; return LPAREN; }
")"             { position += yyleng; return RPAREN; }
"["             { position += yyleng; return LSQUARE; }
"]"             { position += yyleng; return RSQUARE; }
":="    { position += yyleng; return ASSIGN; }

[0-9]+{Letter}((({Letter}|{Digit})|_)*({Letter}|{Digit})+)* {printf("Error at line %d, column %d: identifier \"%s\" must begin with a letter\n", currentLine, position, yytext); exit(1);}
[0-9]+ {position += yyleng; yylval.ival = atoi(yytext); return NUMBER;}
{Letter}((({Letter}|{Digit})|_)*({Letter}|{Digit})+)*_ {printf("Error at line %d, column %d: identifier \"%s\" cannot end with an underscore\n", currentLine, position, yytext); exit(1);}
{Letter}((({Letter}|{Digit})|_)*({Letter}|{Digit})+)* {position += yyleng; yylval.strval = strdup(yytext); return IDENTIFIERS;}
(_)+({Letter}((({Letter}|{Digit})|_)*({Letter}|{Digit})+)*) {printf("Error at line %d, column %d: identifier \"%s\" must begin with a letter\n", currentLine, position, yytext); exit(1);}


"\n" {currentLine = currentLine + 1; position = 1;}
. {printf("Error on line %d, column %d: unrecognized symbol \"%s\"\n", currentLine, position, yytext); exit(0);}

%%
