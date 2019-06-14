%{
open Syntax
open IndexSyntax
%}

%token <int> INTV
%token <float> REALV
%token UNITV
%token <string> VAR

/* Tokens for keywords     */
%token EOF
%token FIX IF LAM
%token LET IN
%token MECH
%token TRUE FALSE
%token CONS NIL

/* Tokens for symbol     */
%token EQUAL
%token COLON
%token DOT
%token LPAREN
%token RPAREN
%token LBRACK
%token RBRACK
%token COMMA
%token LEFTSHIFT

/* Tokens for Operator     */
%token LOG SIGN
%token OR AND XOR
%token ADD SUB MUL DIV
%token LESS LEQ GREATER GEQ

/* Tokens for Types */
%token INT
%token BOOL
%token UNIT
%token REAL
%token TIMES
%token ARROW

/* tokens for Index terms */
%token INDEX
%token BIGLAM


/* tokens for modes */
%token CHECK
%token INFER




%start toplevel
%type < Syntax.expr * IndexSyntax.iterm * Syntax.ty * Syntax.mode > toplevel
%type < Syntax.expr > expr
%type < Syntax.bop > bop
%type < Syntax.uop > uop

%%

toplevel :
    expr LEFTSHIFT LBRACK Mode COMMA ITerm RBRACK COLON Type EOF
        { ($1, $6, $9, $4) }

ITerm:
  | INDEX INTV 
    { IConst $2}


expr:
  | INTV                                            { Prim (PrimInt $1) }
  | REALV                                           { Prim (PrimReal $1)}
  | UNITV                                           { Prim (PrimUnit)}
  | VAR                                             { Var {v_name = $1} }
  | TRUE                                            { True }
  | FALSE                                           { False }
  | LPAREN expr COMMA expr  RPAREN                  { Pair($2, $4) } 
  | IF LPAREN expr COMMA expr COMMA expr  RPAREN
                                                    { If($3, $5, $7) }
  | FIX VAR LPAREN expr  RPAREN DOT expr
                                                    { Fix( {v_name = $2}, $4, $7) }
  | LAM expr DOT expr                               { Fix( {v_name = "_"}, $2, $4) }
  | expr expr                                       { App($1, $2) }
  | NIL                                             { Nil }
  | expr CONS expr                                  { Cons($1, $3) }
  | LPAREN expr RPAREN CONS LPAREN expr RPAREN      { Cons($2, $6) }
  | MECH LPAREN expr RPAREN                         { Mech($3) }
  | LET VAR EQUAL expr IN expr   
                                                    { Let({v_name = $2}, $4, $6) }
  | uop LPAREN expr RPAREN                          { Uop($1, $3) }                                                  
  | expr bop expr                                   { Bop($2, $1, $3) }
  | LPAREN expr RPAREN                              { $2 }
  | BIGLAM DOT expr                                     { ILam $2 }

bop:
    | ADD           { Add }
    | SUB           { Sub }
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
    | LOG           { Log }



Mode:
   CHECK
    { Check }
  | INFER
    { Infer }

Type:

  | INT  
    { Ty_Prim Ty_PrimInt }
  | BOOL
    { Ty_Prim Ty_PrimBool }
  | REAL
    { Ty_Prim Ty_PrimReal }
  | UNIT
    { Ty_Prim Ty_PrimUnit }


