%{
#include <stdio.h>

#define YY_NO_UNPUT
void yyerror(const char *msg);
extern int currentLine;
extern int position;
%}

%union{
char* strval;
int ival;
}

%start Program

%token <ival> NUMBER
%token <strval> IDENTIFIERS



%token FUNCTION BEGINPARAMS ENDPARAMS BEGINLOCALS ENDLOCALS BEGINBODY ENDBODY INTEGER ARRAY OF IF THEN ENDIF ELSE WHILE DO BEGINLOOP ENDLOOP CONTINUE READ WRITE TRUE FALSE RETURN SEMICOLON COLON COMMA LPAREN RPAREN LSQUARE RSQUARE
%right ASSIGN
%left OR
%left AND
%right NOT
%left LT LTE GT GTE NEQ EQ
%left ADD SUB
%left MULT DIV MOD
%right UMINUS
%left LSQUARE RSQUARE
%left LPAREN RPAREN


%%

Program:         %empty
                 {printf("Program -> epsilon\n");}
                 | Function Program
                 {printf("Program -> Function Program\n");}
                 ;

Function:       FUNCTION IDENTIFIERS SEMICOLON BEGINPARAMS DeclarationL ENDPARAMS BEGINLOCALS DeclarationL ENDLOCALS BEGINBODY StatementL ENDBODY
                {printf("Function -> FUNCTION IDENTIFIERS SEMICOLON BEGINPARAMS DeclarationL ENDPARAMS BEGINLOCALS DeclarationL ENDLOCALS BEGINBODY StatementL ENDBODY\n");}
                ;

Declaration:    Id COLON INTEGER 
                {printf("Declaration -> IDENTIFIERS COLON INTEGER\n");}
                | Id COLON ARRAY LSQUARE NUMBER RSQUARE OF INTEGER
                {printf("Declaration -> Id COLON ARRAY LSQUARE NUMBER RSQUARE OF INTEGER\n");}
                ;

Id:             IDENTIFIERS
                {printf("Id -> IDENTIFIERS %s\n", $1);}
                | IDENTIFIERS COMMA Id
                {printf("Id -> IDENTIFIERS %s COMMA Id\n", $1);}
                ;

DeclarationL:  %empty
               {printf("DeclarationL -> epsilon\n");}
               | Declaration SEMICOLON DeclarationL     
               {printf("DeclarationL -> Declaration SEMICOLON DeclarationL\n");}
               ;

Statement:       Var ASSIGN Expression
                 {printf("Statement -> Var ASSIGN Expression\n");}
                 | IF BoolExp THEN StatementL ElseStatement ENDIF
                 {printf("Statement -> IF BoolExp THEN StatementL ElseStatement ENDIF\n");}              
                 | WHILE BoolExp BEGINLOOP StatementL ENDLOOP
                 {printf("Statement -> WHILE BoolExp BEGINLOOP StatementL ENDLOOP\n");}
                 | DO BEGINLOOP StatementL ENDLOOP WHILE BoolExp
                 {printf("Statement -> DO BEGINLOOP StatementL ENDLOOP WHILE BoolExp\n");}
                 | READ V
                 {printf("Statement -> READ V\n");}
                 | WRITE V
                 {printf("Statement -> WRITE V\n");}
                 | CONTINUE
                 {printf("Statement -> CONTINUE\n");}
                 | RETURN Expression
                 {printf("Statement -> RETURN Expression\n");}          
                ;

StatementL:      Statement SEMICOLON StatementL
                {printf("StatementL -> Statement SEMICOLON StatementL\n");}
                | Statement SEMICOLON
                {printf("Statements -> Statement SEMICOLON\n");}
                ;

Expression:     MultExp
                {printf("Expression -> MultExp\n");}
                | Expression ADD MultExp
                 {printf("Expression -> Expression ADD MultExp\n");}
                | Expression SUB MultExp
                 {printf("Expression -> Expression SUB MultExp\n");}
                ;

Var:           IDENTIFIERS
               {printf("Var -> INDENTIFIERS %s\n", $1);}        
               | IDENTIFIERS LSQUARE Expression RSQUARE
               {printf("Var -> IDENTIFIERS %s LSQUARE Expression RSQUARE\n", $1);}
               ;

V:            Var
              {printf("V -> Var\n");}      
              | Var COMMA V
              {printf("V -> Var Comma V\n");}
              ;

ElseStatement:   %empty
                {printf("ElseStatement -> epsilon\n");}
                | ELSE StatementL
                 {printf("ElseStatement -> ELSE StatementL\n");}
                ;

MultExp :       Term
                {printf("MultExp -> Term\n");}
                | MultExp MOD Term
                 {printf("MultExp -> MultExp MOD Term\n");}
                | MultExp DIV Term
                 {printf("MultExp -> MultExp DIV Term\n");}
                | MultExp MULT Term
                 {printf("MultExp -> MultExp MULT Term\n");}
                ;

BoolExp:        RelAndExp
                {printf("BoolExp -> RelAndExp\n");}
                | BoolExp OR RelAndExp
                {printf("BoolExp -> BoolExp OR  RelAndExp\n");}
                ;

RelAndExp:      RelExp
                {printf("RelAndExp -> RelExp\n");}
                | RelAndExp AND RelExp
                {printf("RelAndExp -> RelAndExp AND RelExp\n");}
                ;
                
RelExp:         Expression Comp Expression
                {printf("RelExp -> Expression Comp Expression\n");}
                | NOT Expression Comp Expression
                {printf("RelExp -> NOT Expression Comp Expression\n");}
                | TRUE
                {printf("RelExp -> TRUE\n");}
                | NOT TRUE
                {printf("RelExp -> NOT TRUE\n");}
                | FALSE
                {printf("RelExp -> FALSE\n");}
                | NOT FALSE
                {printf("RelExp -> NOT FALSE\n");}
                | LPAREN BoolExp RPAREN
                {printf("RelExp -> LPAREN BoolExp RPAREN\n");}
                | NOT LPAREN BoolExp RPAREN
                {printf("RelExp -> NOT LPAREN BoolExp RPAREN\n");}
                ;

Comp:            EQ
                 {printf("Comp -> EQ\n");}
                 | NEQ
                 {printf("Comp -> NEQ\n");}
                 | LT
                 {printf("Comp -> LT\n");}
                 | GT
                 {printf("Comp -> GT\n");}
                 | LTE
                 {printf("Comp -> LTE\n");}
                 | GTE
                 {printf("Comp -> GTE\n");}
                 ;

Term:            Var
                {printf("Term -> Var\n");}
                 | SUB Var
                 {printf("Term -> SUB Var\n");}
                 | NUMBER
                 {printf("Term -> NUMBER %d\n", $1);}
                 | SUB NUMBER
                 {printf("Term -> SUB NUMBER %d\n", $2);}
                 | LPAREN Expression RPAREN
                 {printf("Term -> LPAREN Expression RPAREN\n");}
                 | SUB LPAREN Expression RPAREN
                 {printf("Term -> SUB LPAREN Expression RPAREN\n");}
                 | IDENTIFIERS LPAREN ExpressionL RPAREN
                 {printf("Term -> IDENTIFIERS %s LPAREN ExpressionL RPAREN\n", $1);}
                 ;

ExpressionL:    Expression
                {printf("ExpressionL -> Expression\n");}
                |Expression COMMA ExpressionL
                 {printf("ExpressionL -> Expression COMMA ExpressionL\n");}
                ;

%%

int main(int argc, char ** argv) {
        yyparse();

}

void yyerror(const char *m) {
  printf("Error at line %d, position %d: %s \n", currentLine, position, m);

}
