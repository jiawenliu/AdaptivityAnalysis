(******************************************************************************)
(*     Alt-Ergo: The SMT Solver For Software Verification                     *)
(*     Copyright (C) 2013-2014 --- OCamlPro                                   *)
(*     This file is distributed under the terms of the CeCILL-C licence       *)
(******************************************************************************)

(******************************************************************************)
(*     The Alt-Ergo theorem prover                                            *)
(*     Copyright (C) 2006-2013                                                *)
(*     CNRS - INRIA - Universite Paris Sud                                    *)
(*                                                                            *)
(*     Sylvain Conchon                                                        *)
(*     Evelyne Contejean                                                      *)
(*                                                                            *)
(*     Francois Bobot                                                         *)
(*     Mohamed Iguernelala                                                    *)
(*     Stephane Lescuyer                                                      *)
(*     Alain Mebsout                                                          *)
(*                                                                            *)
(*   This file is distributed under the terms of the CeCILL-C licence         *)
(******************************************************************************)

{ 
open Lexing
open Smtlib2_parse

let smtlib2_util_line = ref 1

let newline lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- 
      { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum; 
        pos_cnum=0 }

}

rule token = parse
| ['\t' ' ' ]+ 
    { token lexbuf }
| ';'  (_ # '\n')* 
     { token lexbuf }
| ['\n']+ as str 
     { newline lexbuf; 
       smtlib2_util_line := (!smtlib2_util_line + (String.length str));
       token lexbuf }
| "_"              { UNDERSCORE }
| "("              { LPAREN }
| ")"              { RPAREN }
| "as"             { AS }
| "let"            { LET }
| "forall"         { FORALL }
| "exists"         { EXISTS }
| "!"              { EXCLIMATIONPT }
| "set-logic"      { SETLOGIC }
| "set-option"     { SETOPTION }
| "set-info"       { SETINFO }
| "declare-sort"   { DECLARESORT }
| "define-sort"    { DEFINESORT }
| "declare-fun"    { DECLAREFUN }
| "declare-const"  { DECLARECONST }
| "define-fun"     { DEFINEFUN }
| "push"           { PUSH }
| "pop"            { POP }
| "assert"         { ASSERT }
| "check-sat"      { CHECKSAT }
| "get-assertions" { GETASSERT }
| "get-proof"      { GETPROOF }
| "get-unsat-core" { GETUNSATCORE }
| "get-value"      { GETVALUE }
| "get-assignment" { GETASSIGN }
| "get-option"     { GETOPTION }
| "get-info"       { GETINFO }
| "exit"           { EXIT }
|  '#' 'x' ['0'-'9' 'A'-'F' 'a'-'f']+  as str 
    { HEXADECIMAL(str) }
|  '#' 'b' ['0'-'1']+  as str 
    { BINARY(str) }
|  '|' ([ '!'-'~' ' ' '\n' '\t' '\r'] # ['\\' '|'])* '|'  as str
    { ASCIIWOR(str) }
|  ':' ['a'-'z' 'A'-'Z' '0'-'9' '+' '-' '/' '*' '=' '%' '?' '!' '.' '$' '_' '~' '&' '^' '<' '>' '@']+  as str
    { KEYWORD(str) }
|  ['a'-'z' 'A'-'Z' '+' '-' '/' '*' '=''%' '?' '!' '.' '$' '_' '~' '&' '^' '<' '>' '@'] ['a'-'z' 'A'-'Z' '0'-'9' '+' '-' '/' '*' '=''%' '?' '!' '.' '$' '_' '~' '&' '^' '<' '>' '@']*  as str 
    { SYMBOL(str) }
|  '"' (([ '!'-'~' ' ' '\n' '\t' '\r' ] # ['\\' '"']) | ('\\' ['!'-'~' ' ' '\n' '\t' '\r'] ))* '"' as str 
    { STRINGLIT(str) }
|  ( '0' | ['1'-'9'] ['0'-'9']* )  as str 
    { NUMERAL(str) }
|  ( '0' | ['1'-'9'] ['0'-'9']* ) '.' ['0'-'9']+ as str
    { DECIMAL(str) }
| eof 
    { EOF }
| _ 
    {failwith(
      (Lexing.lexeme lexbuf) ^
	": lexing error on line "^(string_of_int !smtlib2_util_line))}{}
