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
%token DBCOLON NIL

/* Tokens for symbol     */
%token EQUAL
%token COLON
%token SEMICOLON
%token DOT
%token LPAREN
%token RPAREN
%token LBRACK
%token RBRACK
%token COMMA
%token VDASH
%token PACK
%token UNPACK
%token UNIFORM
%token BERNOULLI

/* Tokens for Operator     */
%token LOG SIGN
%token OR AND XOR
%token ADD SUB MUL DIV DOT
%token LESS LEQ GREATER GEQ
%token SETMINUS

/* Tokens for Types */
%token INT
%token BOOL
%token UNIT
%token REAL
%token TIMES
%token ARROW
%token BOX
%token FORALL
%token EXISTS
%token LIST

/* tokens for Index terms */
%token INDEX
%token BIGLAM
%token MAX
%token MIN
%token ADAPT
%token DMAP
%token BOT
%token DEPTH
%token INFTY

/* tokens for modes */
%token CHECK
%token INFER




%start toplevel
%type < Syntax.expr * Syntax.ty > toplevel
%type < Syntax.expr > expr
%type < Syntax.bop > bop
%type < Syntax.uop > uop

%%

toplevel :
    expr VDASH Type EOF
        { ($1, $3) }

ITerm:
  | INDEX INTV 
    { IConst $2}
  | INDEX VAR
    { IVar { v_name = $2 }}
  | MAX LPAREN ITerm COMMA ITerm RPAREN
    { IMaximal($3, $5)}
  | ITerm SUB ITerm
    { ISub($1, $3) }
  | ITerm ADD ITerm
    { IAdd($1, $3) }

DTerm:
  | DEPTH INTV 
    { DConst $2}
  | DEPTH VAR
    { DVar { v_name = $2 }}
  | MAX LPAREN DTerm COMMA DTerm RPAREN
    { DMaximal($3, $5)}
  | DTerm SUB DTerm
    { DSub($1, $3) }
  | DTerm ADD DTerm
    { DAdd($1, $3) }
  | DEPTH INFTY
    { DInfty }

  | DEPTH BOT
    { DBot }


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
  | FIX VAR LPAREN VAR COLON Type RPAREN DOT expr
                                                    { Fix( {v_name = $2}, {v_name = $4}, $6, $9) }
  | LAM VAR COLON Type DOT expr                     { Fix( {v_name = "_"}, {v_name = $2}, $4, $6) }
  | NIL                                             { Nil }
  | expr DBCOLON expr                                  { Cons($1, $3) }
  | MECH LPAREN expr RPAREN                         { Mech($3) }
  | app                                             { $1 }
  | LET VAR COLON DTerm EQUAL expr IN expr   
                                                    { Let({v_name = $2}, $4, $6, $8) }
  | uop LPAREN expr RPAREN                          { Uop($1, $3) }                                                  
  | expr bop expr                                   { Bop($2, $1, $3) }
  | LPAREN expr RPAREN                              { $2 }
  | BIGLAM DOT expr                                 { ILam $3 }
  | expr LBRACK RBRACK                              { IApp $1 }
  | PACK expr                                       { Pack $2 }
  | UNPACK RPAREN expr COMMA VAR COMMA expr LPAREN  { Unpack($3, {v_name = $5}, $7) }
  | BERNOULLI expr                                  { Bernoulli $2 }
  | UNIFORM LPAREN expr COMMA expr RPAREN           { Uniform( $3, $5 ) }

/* Applications */
app:
  | app expr
     { App($1, $2) }
  |  expr 
     { $1 }


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
    | SETMINUS      { Setminus }
    | DOT           { Dot }


uop:
    | SIGN          { Sign }
    | LOG           { Log }


Sort:
  ADAPT
    { Adapt }



DMap:
  LBRACK BOT RBRACK
    { [] }

  | LBRACK Dmap RBRACK
    { $2 }

Dmap:
  | LPAREN VAR COLON INTV RPAREN
    { [ ({v_name = $2}, (DConst $4) ) ] }
  
  | LPAREN VAR COLON INTV RPAREN COMMA Dmap
    { ( {v_name = $2}, (DConst $4) ) :: $7 }


Type:

  | INT  
    { Ty_Prim Ty_PrimInt }
  | REAL
    { Ty_Prim Ty_PrimReal }
  | UNIT
    { Ty_Prim Ty_PrimUnit }

  | BOOL
    { Ty_Bool }

  | BOX Type
    { Ty_Box $2 }

  | Type TIMES Type
    { Ty_Prod($1, $3) }

  | Type ARROW Type
    { Ty_Arrow($1, DConst 0, [], IConst 0, $3) }

  | Type COMMA INTV ARROW LPAREN DMap SEMICOLON ITerm RPAREN Type
    { Ty_Arrow($1, DConst $3, $6, $8, $10) }

  | Type LIST
    { Ty_List $1 }

  | FORALL VAR DBCOLON COLON Sort DOT Type 
    { Ty_Forall({v_name = $2}, $5, [], IConst 0, $7) }

  | EXISTS VAR DBCOLON COLON Sort DOT Type 
    { Ty_Exists({v_name = $2}, $5, $7) }

  | INT LBRACK ITerm RBRACK
    { Ty_IntIndex($3)}


