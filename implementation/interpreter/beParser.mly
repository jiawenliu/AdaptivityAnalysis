%{
open BeAst
%}


%token <int> INT
%token <float> FLOAT
%token <string> VAR
%token EOF
%token FIX IF LAM
%token LET IN
%token MECH
%token TRUE FALSE
%token CONS NIL
%token LPAREN
%token RPAREN

%token EQUAL
%token COLON
%token DOT

%token LG SIGN
%token OR AND XOR
%token PLUS MINUS MUL DIV
%token LESS LEQ GREATER GEQ




%start expr
%type <BeAst.expr> expr
%type <BeAst.bop> bop
%type <BeAst.uop> uop

%%
expr:
  | INT                                             { Const_i $1 }
  | FLOAT                                           { Const_f $1}
  | VAR                                             { Var $1 }
  | TRUE                                            { True }
  | FALSE                                           { False }
  | LPAREN expr COLON expr  RPAREN                  { Pair($2, $4) } 
  | IF LPAREN expr COLON expr COLON expr  RPAREN
                                                    { If($3, $5, $7) }
  | FIX expr LPAREN expr  RPAREN DOT expr
                                                    { Fix($2, $4, $7) }
  | LAM expr DOT expr                               { Fix((Var "anon"), $2, $4) }
  | expr expr                                       { App($1, $2) }
  | NIL                                             { Nil }
  | expr CONS expr                                  { Cons($1, $3) }
  | LPAREN expr RPAREN CONS LPAREN expr RPAREN      { Cons($2, $6) }
  | MECH LPAREN expr RPAREN                         { Mech($3) }
  | LET expr EQUAL expr IN expr   
                                                    { Let($2, $4, $6) }
  | uop LPAREN expr RPAREN                          { Uop($1, $3) }                                                  
  | expr bop expr                                   { Bop($2, $1, $3) }
  | LPAREN expr RPAREN                              { $2 }

bop:
    | PLUS          { Plus }
    | MINUS         { Minus }
    | MUL           { Mul }
    | DIV           { Div }
    | OR            { Or }
    | AND           { And }
    | XOR           { Xor }
    | EQUAL         { Equal }
    | LEQ           { Leq }
    | GEQ           { Geq }
    | LESS          { Less }
    | GREATER       { Greater }


uop:
    | SIGN          { Sign }
    | LG            { Lg }