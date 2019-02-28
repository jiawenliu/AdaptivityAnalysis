
%{
open Ast
%}

(* declares all the lexical *tokens* of the language *)

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
  | expr; PLUS; expr | expr; SUB; expr | expr; MUL; expr | expr; DIV; expr  
  | expr; OR; expr | expr; AND; expr 
  | LG; LPAREN; expr; RPAREN | SIGN; LPAREN; expr; RPAREN
  | expr; LESS; expr | expr; LEQ; expr
  | expr; GREATER; expr | expr; GEQ; expr
  | i = INT                                       { Const i }
  | x = VAR                                       { Var x }
  | TRUE                                          { True }
  | FALSE                                         { False }
  | LPAREN; e1 = expr; COLON; e2 = expr; RPAREN   { Pair(e1, e2) } 
  | IF; LPAREN; e = expr; COLON; e1 = expr; COLON; e2 = expr; RPAREN
                                                  { If(e, e1, e2) }
  | FIX; f = VAR; LPAREN; x = VAR; RPAREN; DOT; e = expr
                                                  { Fix(f, x, e) }
  | LAM; x = VAR; DOT; e = exper                  { Fix(_, x, e) }
  | e1 = expr; e2 = expr                          { App(e1, e2) }
  | NIL                                           { Nil }
  | e1 = expr; CONS; e2 = expr                    { Cons(e1, e2) }
  | MECH; LPAREN; e = expr; RPAREN                { Mech(e) }
  | LET; x = expr;  EQUALS; e1 = expr; IN; e2 = expr   
                                                  { Let(x, e1, e2) }
  | LPAREN; expr; RPAREN
  ;
	
(* And that's the end of the grammar definition. *)
