%{
open Syntax
%}

%token <int> INT
%token <float> REAL
%token <string> VAR

/* Tokens for keywords     */
%token EOF
%token TRUE FALSE NOT
%token WHILE DO 
%token IF THEN ELSE
%token CHI
%token ALPHA

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
%token RBRACE
%token LBRACE

/* Tokens for Operator     */
%token LOG SIGN
%token OR AND XOR
%token ADD SUB MUL DIV 
%token LESS LEQ GREATER GEQ

/*Tokens for command*/
%token SKIP
%token QUERY
%token LEFTARROW

%start toplevel
%type < Syntax.lcommand > toplevel
%type < Syntax.lcommand > lcommand
%type < Syntax.expr > expr
%type < Syntax.query_expr > qexpr
%type < Syntax.a_expr > aexpr
%type < Syntax.b_expr > bexpr
%type < Syntax.aop > aop
%type < Syntax.bop > bop
%type < Syntax.cop > cop
%%

toplevel :
    lcommand  EOF
        { $1 }

lcommand : 
  | lcommand SEMICOLON lcommand
      { Seq ($1, $3) }
  | SKIP
     {Skip}   
  | LBRACK VAR LEFTARROW expr  RBRACK INT
     { Assign ({v_name = $2} , $4, Label($6) ) } 
  | LBRACK VAR LEFTARROW QUERY LPAREN qexpr RPAREN RBRACK INT
     { Query  ({v_name = $2} , $6, Label($9)) }
  | WHILE LBRACK  bexpr RBRACK INT  DO LBRACE lcommand  RBRACE
     { While  ($3 , $8, Label($5) ) }
  | IF LBRACK bexpr RBRACK INT THEN LBRACE lcommand RBRACE ELSE LBRACE lcommand RBRACE
     { If ( $3 , $8 , $12 , Label($5) )  }

 aop:
  | SUB                 { Sub }
  | ADD                 { Add }
  | MUL                 { Mul }
  | DIV                 { Div }  
  | XOR                 { Xor }

bop:
  | OR                  { Or }
  | AND                 { And } 

cop:
  | EQUAL               { Equal } 
  | LEQ                 { Leq } 
  | GEQ                 { Geq }
  | LESS                { LessThan } 
  | GREATER             { GreaterThan }  

expr:
  | aexpr               {  Eaexpr ($1) }
  | bexpr               {  Ebexpr ($1) }

aexpr : 
  | INT                                             { Aint ($1) }
  | VAR                                             { Avar {v_name = $1} }              
  | aop LPAREN aexpr  COMMA aexpr RPAREN            { Aaop ( $1, $3, $5 )  }

bexpr : 
  | TRUE                                            { BTrue }
  | FALSE                                           { BFalse }
  | NOT bexpr                                       { BNeg $2 }
  | bop LPAREN bexpr COMMA bexpr RPAREN              { Bbop ($1, $3, $5) }      
  | cop LPAREN aexpr COMMA aexpr RPAREN             { Bcop ($1, $3, $5 ) }

qexpr :
  | ALPHA                         {Qalpha } 
  | CHI  COLON aexpr  COLON    {Qchi ($3)}
  | aexpr                        {Qaexpr ($1)}
  | aop  LPAREN qexpr  COMMA qexpr RPAREN            { Qaop ( $1, $3, $5 )  }





// ITerm:
//   | INDEX INTV 
//     { IConst $2}
//   | INDEX VAR
//     { IVar { v_name = $2 }}
//   | MAX LPAREN ITerm COMMA ITerm RPAREN
//     { IMaximal($3, $5)}
//   | ITerm SUB ITerm
//     { ISub($1, $3) }
//   | ITerm ADD ITerm
//     { IAdd($1, $3) }

// DTerm:
//   | DEPTH INTV 
//     { DConst $2}
//   | DEPTH VAR
//     { DVar { v_name = $2 }}
//   | MAX LPAREN DTerm COMMA DTerm RPAREN
//     { DMaximal($3, $5)}
//   | DTerm SUB DTerm
//     { DSub($1, $3) }
//   | DTerm ADD DTerm
//     { DAdd($1, $3) }
//   | DEPTH INFTY
//     { DInfty }
//   | DEPTH BOT
//     { DBot }

// DMap:
//   LBRACK BOT RBRACK
//     { [] }

//   | LBRACK Dmap RBRACK
//     { $2 }

// Dmap:
//   | LPAREN VAR COLON INTV RPAREN
//     { [ ({v_name = $2}, (DConst $4) ) ] }
  
//   | LPAREN VAR COLON INTV RPAREN COMMA Dmap
//     { ( {v_name = $2}, (DConst $4) ) :: $7 }

//   | LPAREN VAR COLON BOT RPAREN
//     { [ ({v_name = $2}, DBot ) ] }
  
//   | LPAREN VAR COLON BOT RPAREN COMMA Dmap
//     { ( {v_name = $2}, DBot ) :: $7 }

//   | LPAREN VAR COLON VAR RPAREN
//     { [ ({v_name = $2}, (DVar {v_name = $4}) ) ] }
  
//   | LPAREN VAR COLON VAR RPAREN COMMA Dmap
//     { ( {v_name = $2},  (DVar {v_name = $4}) ) :: $7 }

//   | LPAREN VAR COLON DTerm RPAREN
//     { [ ({v_name = $2}, $4 ) ] }
  
//   | LPAREN VAR COLON DTerm RPAREN COMMA Dmap
//     { ( {v_name = $2},  $4 ) :: $7 }


// expr:
//   | INTV                                            { Prim (PrimInt $1) }
//   | REALV                                           { Prim (PrimReal $1)}
//   | UNITV                                           { Prim (PrimUnit)}
//   | VAR                                             { Var {v_name = $1} }
//   | TRUE                                            { True }
//   | FALSE                                           { False }
//   | LPAREN expr COMMA expr  RPAREN                  { Pair($2, $4) } 
//   | IF LPAREN expr COMMA expr COMMA expr  RPAREN
//                                                     { If($3, $5, $7) }
//   | FIX VAR LPAREN VAR COLON Type RPAREN DOT expr
//                                                     { Fix( {v_name = $2}, {v_name = $4}, $6, $9) }
//   | LAM LPAREN VAR COLON Type RPAREN DOT expr       
//                                                     { Fix( {v_name = "_"}, {v_name = $3}, $5, $8) }
//   | LAM VAR COLON Type DOT expr       
//                                                     { Fix( {v_name = "_"}, {v_name = $2}, $4, $6) }
//   | NIL                                             { Nil }
//   | expr DBCOLON expr                               { Cons($1, $3) }
//   | MECH LPAREN expr RPAREN                         { Mech($3) }
//   | LET VAR COLON DTerm EQUAL expr IN expr   
//                                                     { Let({v_name = $2}, $4, $6, $8) }
//   | app                                             { $1 }
//   | LBRACE expr COLON Type RBRACE  
//                                                     { Annotated($2, $4, [], IConst 0) }
//   | LBRACE expr COLON Type SEMICOLON DMap SEMICOLON ITerm RBRACE  
//                                                     { Annotated($2, $4, $6, $8) }
//   | uop LPAREN expr RPAREN                          { Uop($1, $3) }                                                  
//   | expr bop expr                                   { Bop($2, $1, $3) }
//   | LPAREN expr RPAREN                              { $2 }
//   | BIGLAM DOT ITerm DOT expr                         
//                                                     { ILam ($3, $5) }
  
//   | PACK expr                                       { Pack $2 }
//   | UNPACK RPAREN expr COMMA VAR COMMA expr LPAREN  { Unpack($3, {v_name = $5}, $7) }
//   | BERNOULLI expr                                  { Bernoulli $2 }
//   | UNIFORM LPAREN expr COMMA expr RPAREN           { Uniform( $3, $5 ) }


// /* Applications */
// app:
//   | app expr
//      { App($1, $2) }
//   |  expr 
//      { $1 }
//   | app LBRACK ITerm RBRACK
//      { IApp($3, $1) }



// bop:
//     | ADD           { Add }
//     | SUB           { Sub }
//     | MUL           { Mul }
//     | DIV           { Div }
//     | OR            { Or }
//     | AND           { And }
//     | XOR           { Xor }
//     | EQUAL         { Equal }
//     | LEQ           { Leq }
//     | GEQ           { Geq }
//     | LESS          { Less }
//     | GREATER       { Greater }
//     | SETMINUS      { Setminus }
//     | DOT           { Dot }


// uop:
//     | SIGN          { Sign }
//     | LOG           { Log }


// Sort:
//   ADAPT
//     { Adapt }




// Type:

//   | INT  
//     { Ty_Prim Ty_PrimInt }
//   | REAL
//     { Ty_Prim Ty_PrimReal }
//   | UNIT
//     { Ty_Prim Ty_PrimUnit }

//   | BOOL
//     { Ty_Bool }

//   | BOX Type
//     { Ty_Box $2 }

//   | TypePair
//     { $1 }

//   | Type ARROW Type
//     { Ty_Arrow($1, DConst 0, [], IConst 0, $3) }

//   | Type COMMA INTV ARROW LPAREN DMap SEMICOLON ITerm RPAREN Type
//     { Ty_Arrow($1, (DConst $3), $6, $8, $10) }

//   | Type COMMA BOT ARROW LPAREN DMap SEMICOLON ITerm RPAREN Type
//     { Ty_Arrow($1, DBot, $6, $8, $10) }

//   | Type LIST
//     { Ty_List $1 }

//   | FORALL VAR DBCOLON COLON Sort DOT Type 
//     { Ty_Forall({v_name = $2}, $5, [], IConst 0, $7) }

//   | EXISTS VAR DBCOLON COLON Sort DOT Type 
//     { Ty_Exists({v_name = $2}, $5, $7) }

//   | INT LBRACK ITerm RBRACK
//     { Ty_IntIndex($3)}

//   | LPAREN Type RPAREN
//     { $2 }


// TypePair :
// 	| Type TIMES Type
// 		{ Ty_Prod ($1, $3) }
//     | Type TIMES TypePair
//     	{ Ty_Prod ($1, $3) }






