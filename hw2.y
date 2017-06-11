%{
    #include <stdio.h>
    #include <stdlib.h>
    
    extern int lineCount;
    extern char token[1000];
    extern char *yytext;
    int Funct_Count = 0;
%}

%start StartProduction

%token ID

%token INT VOID DOUBLE FLOAT BOOL CHAR
%token INTEGER DBLE CHR STR
%token NUL FOR WHILE DO IF ELSE SWITCH 
%token RETURN BREAK CONTINUE CONST
%token TRUE FALSE CASE DEFAULT

%left PLUS SUB MUL DIV MOD
%left EQ GTE LTE GT LT NOT NOTEQ
%left AND OR 
%left PO SO 

%%

StartProduction: Code
               | StartProduction Code
               ;

Code: Declaration
    | Funct_Type { Funct_Count++; }
    ;

Declaration: CONST Datatype Const_Stmts ';'
           | Datatype Initial_Stmts ';'
           | Funct_Declaration
           ;

Const_Stmts: Const_Stmt
           | Const_Stmts ',' Const_Stmt
           ;

Const_Stmt: ID '=' INTEGER
          | ID '=' DBLE
          | ID '=' CHR
          | ID '=' STR
          | ID '=' TRUE
          | ID '=' FALSE
          ;

Initial_Stmts: Initial_Stmt
             | Initial_Stmts ',' Initial_Stmt
             ;

Initial_Stmt: ID
            | Array
            | ID '=' Expression
            | Array '=' Array_Content
            ;

Funct_Declaration: Datatype Funct_Body ';'
                 | VOID Funct_Body ';'
                 | Datatype Funct_Params ';'
                 | VOID Funct_Params ';'
                 ;

Funct_Type: Datatype Funct_Body Compound
          | Datatype ID Funct_Params Compound
          | VOID ID Funct_Body Compound
          | VOID ID Funct_Params Compound
          ;

Datatype: INT
        | DOUBLE
	| FLOAT
        | BOOL
        | CHAR
        ;

Funct_Body: ID '(' ')'
          ;

Funct_Params: ID '(' Params ')'
            ;

Params: Param
      | Params ',' Param
      ;

Param: Datatype ID
     | Datatype Array
     | Final_Expr
     ;

Array: ID Array_Declaration
     ;

Array_Declaration: '[' INTEGER ']'
                 | Array_Declaration '[' INTEGER ']'
                 ;

Array_Content: '{' '}'
             | '{' Expressions '}'
             ;

Array_Expr: '[' Expression ']'
          |  Array_Expr '[' Expression ']'
          ;

Compound: '{' Compound_Body '}'
        | '{' '}'
        ;

Compound_Body: Compound_Content
             | Compound_Body Compound_Content
             ;

Compound_Content: Declaration
                | Stmt
                ;

Stmts: Stmt
     | Stmts Stmt
     ;

Stmt: Simple_Stmt
    | IF_ELSE
    | Switch_Stmt
    | Loop_Stmt
    | Terminal
    ;

Simple_Stmt: ID_or_Array '=' Expression ';'
	   | ID '(' Expressions_or_Not ')' ';'
           ;

Expressions_or_Not: Expressions
                  |  
                  ;

IF_ELSE: IF '(' Expression ')' Compound_or_Single
       | IF '(' Expression ')' Compound_or_Single ELSE Compound_or_Single
       ;

Compound_or_Single: Compound
                  | Declaration
                  | Simple_Stmt
                  | Terminal
                  | ';'
                  ;

Switch_Stmt: SWITCH '(' ID ')' '{' Switch_Compound '}'
           ;

Switch_Compound: Cases
               | Cases Default
               ;

Cases: Case_Content
     | Cases Case_Content
     ;

Case_Content: CASE INTEGER ':' Stmts
            | CASE CHR ':' Stmts
            | CASE INTEGER ':'
            | CASE CHR ':'
            ;

Default: DEFAULT ':' Stmts
       | DEFAULT ':'
       ;

Loop_Stmt: WHILE '(' Expression ')' Compound_or_Single
         | DO Compound WHILE '(' Expression ')' ';'
         | FOR For_Stmt Compound_or_Single
         ;

For_Stmt: '(' Expr_Stmt Expr_Stmt For_last ')'

For_last: Expression
        |
        ;

Expr_Stmt: Expression ';'
         | ID '=' Expression ';'
         | ';'   
         ;

Terminal: BREAK ';'
        | CONTINUE ';'
        | RETURN ';'
        | RETURN Expression ';'
        ;

Expression: Logic
          ;

Logic: Logic_Expr
     | Logic OR Logic_Expr
     ;

Logic_Expr: Relation
          | Logic_Expr AND Relation
          ;

Relation: Relation_Expr
        | NOT Relation_Expr
        ;

Relation_Expr: PLUS_SUB
             | Relation_Expr EQ PLUS_SUB
             | Relation_Expr GTE PLUS_SUB
             | Relation_Expr LTE PLUS_SUB
             | Relation_Expr GT PLUS_SUB
             | Relation_Expr LT PLUS_SUB
             | Relation_Expr NOTEQ PLUS_SUB
             ;

PLUS_SUB: MUL_DIV_MOD
        | PLUS_SUB PLUS MUL_DIV_MOD
        | PLUS_SUB SUB MUL_DIV_MOD
        ;

MUL_DIV_MOD: Unary_Expr
           | MUL_DIV_MOD MUL Unary_Expr
           | MUL_DIV_MOD DIV Unary_Expr
           | MUL_DIV_MOD MOD Unary_Expr
           ;

Unary_Expr: PO_or_SO
          | SUB PO_or_SO
          ;

PO_or_SO: Final_Expr
        | Final_Expr PO
        | Final_Expr SO
        ;

Final_Expr: INTEGER
          | DBLE
          | CHR
          | STR
          | ID_or_Array
          | TRUE
          | FALSE
          | '(' Expressions ')'
          | Funct_Body
          | ID '(' Expressions ')'
          ;

ID_or_Array: ID
           | ID Array_Expr
           ;

Expressions: Expression
           | Expressions ',' Expression
           ;

%%

int yyerror( const char* str) {
	fprintf( stderr, "*** Error at line %d: %s\n", lineCount, token );
	fprintf( stderr, "\n" );
	fprintf( stderr, "Unmatched token: %s\n", yytext );
	fprintf( stderr, "*** syntax error\n");
	exit(-1);
}

int main(void) {
  yyparse();
  
  if (Funct_Count == 0) yyerror("There should be at least one function");
  
  printf("No syntax error!\n");
  
  return 0;
}
