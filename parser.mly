%{ open Ast %}

/* Arithmetic operators  */
%token PLUS MINUS TIMES DIVIDE MODULO EXPON FLOOR INCREMENT DECREMENT
/* Assignment operators  */
%token ASSIGN PLUSASSIGN MINUSASSIGN TIMESASSIGN DIVIDEASSIGN MODULOASSIGN FLOORASSIGN EXPONASSIGN 
/* Logical    operators  */
%token LAND LOR LNOT
/* Comparison operators  */
%token EQ NEQ GT LT GEQ LEQ
/* Keywords logic        */
%token AND OR NOT
/* Keywords non-access   */
%token CONST
/* Keywords identity     */
%token IS ISNOT
/* Keywords membership   */
%token IN NOTIN
/* Keywords flow control */
%token WHEN WHILE IF ELSE BREAK CONTINUE DO FOR THEN DEFAULT
/* Keywords Object       */
%token CLASS CONSTRUCTOR NEW SUPER EXTENDS IMPLEMENTS DOT INTERFACE THROWS RAISES THIS
/* Keywords types        */
%token BOOL FLOAT CHAR STRING INT NULL
/* Keywords boolean lit  */
%token TRUE FALSE
/* Keywords imports      */
%token IMPORT AS
/* Keywords functions    */
%token RETURN VOID
/* Keywords exceptions   */
%token TRY CATCH FINALLY THROW RAISE
/* Delimiter characters  */
%token SEMICOLON COLON LCOMMENT RCOMMENT LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE COMMA SINGLEQUOTE DOUBLEQUOTE BACKTICK
/* Scoping DELIMITERS    */
%token NEWLINE INDENT DEDENT 
/* Terminal Tokens       */
%token <int> INTLIT
%token <float> FLOATLIT
%token <bool> BOOLLIT 
%token <string> ID
%token <string> STRINGLIT
%token <char> CHARLIT
%token EOF

%right ASSIGN PLUSASSIGN MINUSASSIGN TIMESASSIGN DIVIDEASSIGN MODULOASSIGN FLOORASSIGN EXPONASSIGN 
%left OR
%left AND
%nonassoc NOT
%left LOR
%left LAND
%nonassoc LNOT
%left EQ NEQ 
%left GT LT GEQ LEQ
%left PLUS MINUS 
%left TIMES DIVIDE MODULO FLOOR
%left EXPON
%nonassoc INCREMENT DECREMENT

%start program
%type <Ast.program> program

%%
program:
  statements EOF {$1}

statements:
  /* nothing */ { [] }
  | statement statements  { $1 :: $2 }

statement:
    compound_statement { $1 }
  | simple_statments   { $1 }

simple_statments:
   /*nothing*/ { [] }
  | simple_statement NEWLINE { $1 }
  | simple_statement SEMICOLON simple_statements {  $1 :: $3 }

simple_statement:
    assignment {}
  | return_statement {}
  | then_statement {}
  | import_statement {}
  | expression statement {}

compound_statement:
    function {}
  | class {}
  | if_statement {}
  | for_statement {}
  | when_statement {}
  | while_statement {}
  | try_statement {}

assignment:
    ID ASSIGN expr {}
  | ID PLUSASSIGN expr {}
  | ID MINUSASSIGN expr {}
  | ID TIMESASSIGN expr {}
  | ID DIVIDEASSIGN expr {}
  | ID MODULOASSIGN expr {}
  | ID FLOORASSIGN expr {}
  | ID EXPONASSIGN expr {}

import_statement:
    IMPORT ID {}
  | IMPORT ID AS ID {}

if_statement: 
    IF expr COLON block else_if_statment{}
  | IF expr COLON block else_clause {}

else_if_statment:
    ELSE IF expr COLON block else_if_statment {}
  | ELSE IF expr COLON block else_clause {}

else_clause:
    ELSE COLON block {}

for_statement:
    FOR type_decl expr IN type_decl expr COLON block {}
  | FOR args COLON block {}
 
when_statement:
    WHEN expr IS COLON NEWLINE INDENT case_block+ default_block DEDENT {}

while_statement:
    WHILE expr COLON block {}

try_statement:
    TRY COLON block finally_block {}
  | TRY COLON block catch_block+ finally_block  {}

then_statement:
  THEN simple_statement

case_block:
    ID COLON NEWLINE INDENT then_statement DEDENT {}

default_block:
  DEFAULT COLON NEWLINE INDENT then_statement DEDENT {}

catch_block:
    CATCH expr COLON block {}

finally_block:
    FINALLY COLON block {}

function:
  id_decl ID

formals_opt:
  /*nothing*/ { [] }
  | formals_list { $1 }

formals_list:
    id_decl { [$1] }
  | id_decl COMMA formals_list { $1::$3 }

id_decl:
  typ ID { ($1, $2) }

typ:
    INT    { Int    }
  | FLOAT  { Float  }
  | BOOL   { Bool   }
  | CHAR   { Char   }
  | STRING { String } 
  | VOID   { Void   }

block:
    NEWLINE INDENT statements DEDENT {}
  | simple_statments {}

expr:
    INTLIT     { IntLit($1)    }
  | FLOATLIT   { FloatLit($1)  }
  | BOOLLIT    { BoolLit($1)   }
  | CHARLIT    { CharLit($1)   }
  | STRINGLIT  { StringLit($1) }
  | ID         { Id($1)        }
  | expr PLUS   expr     { Binop($1, Add,   $3)     }
  | expr MINUS  expr     { Binop($1, Sub,   $3)     }
  | expr EQ     expr     { Binop($1, Eq,    $3)     }
  | expr NEQ    expr     { Binop($1, Neq,   $3)     }
  | expr LT     expr     { Binop($1, Lt,  $3)       }
  | expr AND    expr     { Binop($1, And,   $3)     }
  | expr OR     expr     { Binop($1, Or,    $3)     }
  | expr PLUSASSIGN expr {Assign($1, Binop($1, Add, $3)) }
  | expr MINUSASSIGN expr {Assign($1, Binop($1, Sub, $3)) }
  | expr TIMESASSIGN expr {Assign($1, Binop($1, Mult, $3)) }
  | expr DIVIDEASSIGN expr {Assign($1, Binop($1, Div, $3)) }
  | expr MODULOASSIGN expr {Assign($1, Binop($1, Mod, $3)) }
  | expr FLOORASSIGN expr {Assign($1, Binop($1, Floor, $3)) }
  | expr EXPONASSIGN expr {Assign($1, Binop($1, Exp, $3)) }

args:
  expr  { [$1] }
  | expr COMMA args { $1::$3 }