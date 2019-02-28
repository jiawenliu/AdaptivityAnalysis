
%{
open Ast
%}

(* declares all the lexical *tokens* of the language *)

%token <int> INT
%token <string> ID
%token EOF
%token FIX IF LAM
%token LET
%token IN
%token MECH
%token TRUE FALSE
%token FOLDL FOLDR LG
%token OR AND XOR NEG
%token PLUS MUL DIV
%token LESS LEQ GREATER GEQ
%token CONS NIL
%token LPAREN
%token RPAREN
%token EQUALS
%token COLON

(* additional information about precedence and associativity. *)

%nonassoc IN
%left PLUS MUL DIV




%start <Ast.expr> prog

(* The following %% ends the declarations section of the grammar definition. *)

%%

(* Now begins the *rules* section of the grammar definition. *)
   
   
prog:
	| e = expr; EOF { e }
	;
	
(* the definition of experssion BNF *)

expr:
  | i = INT                                       { Const i }
  | x = ID                                        { Var x }
  | TRUE                                          { True }
  | FALSE                                         { False }
  | LPAREN e1 = expr COLON e2 = expr RPAREN       { Pair(e1, e2) } 
  | IF LPAREN e = expr COLON e1 = expr COLON e2 = expr RPAREN
                                                  { If(e, e1, e2) }
	| NIL                                           { Nil }
  | e1 = expr CONS e2 = expr                      { Cons(e1, e2) }
  | LET x = expr  EQUALS e1 = expr IN e2 = expr   { Let(x, e1, e2) }
	;
	
(* And that's the end of the grammar definition. *)
