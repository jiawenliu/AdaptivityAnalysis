%{
open BeAst
%}


%token <int> INT
%token <string> VAR
%token EOF
%token FIX IF LAM
%token LET IN
%token MECH
%token TRUE FALSE
%token LG SIGN
%token OR AND XOR
%token PLUS SUB MUL DIV
%token LESS LEQ GREATER GEQ
%token CONS NIL
%token LPAREN
%token RPAREN
%token EQUALS
%token COLON
%token DOT


%start expr
%type <BeAst.expr> expr

%%
expr:
  | INT                                       		{ Const $1 }
  | VAR                                       		{ Var $1 }
  | TRUE                                          	{ True }
  | FALSE                                         	{ False }
  | LPAREN expr COLON expr  RPAREN                	{ Pair($2, $4) } 
  | IF LPAREN expr COLON expr COLON expr  RPAREN
                                                  	{ If($3, $5, $7) }
  | FIX expr LPAREN expr  RPAREN DOT expr
                                                  	{ Fix($2, $4, $7) }
  | LAM expr DOT expr                  				{ Fix($2, $2, $4) }
  | expr expr                          				{ App($1, $2) }
  | NIL                                           	{ Nil }
  | expr CONS expr                    				{ Cons($1, $3) }
  | MECH LPAREN expr RPAREN                			{ Mech($3) }
  | LET expr EQUALS expr IN expr   
                                                  	{ Let($2, $4, $6) }
