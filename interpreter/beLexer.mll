{
open BeParser
open Lexing
exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }
    
}

let white = [' ' '\t']+
let digit = ['0'-'9']
let int = '-'? digit+
let letter = ['a'-'z' 'A'-'Z']
let id = letter+ (digit|letter)*

(* The final section of the lexer definition defines how to parse a character
   stream into a token stream.  Each of the rules below has the form 
     | regexp { action }
   If the lexer sees the regular expression [regexp], it produces the token 
   specified by the [action].  We won't go into details on how the actions
   work.  *)

rule token = 
  parse
  | white               { token lexbuf }
  | int                 { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | eof                 { EOF }
  | "fix"               { FIX }
  | "if"                { IF }
  | "lambda"            { LAM }
  | "let"               { LET }
  | "in"                { IN }
  | "mech"              { MECH }
  | "true"              { TRUE }
  | "false"             { FALSE }
  | "lg"                { LG }
  | "sign"              { SIGN }
  | "||"                { OR }
  | "&&"                { AND }
  | "^"                 { XOR }
  | "-"                 { SUB }
  | "+"                 { PLUS }
  | "*"                 { MUL }
  | "/"                 { DIV }
  | "<"                 { LESS }
  | "<="                { LEQ }
  | ">"                 { GREATER }
  | ">="                { GEQ }
  | "::"                { CONS }
  | "[]"                { NIL }
  | "("                 { LPAREN }
  | ")"                 { RPAREN }
  | "="                 { EQUALS }
  | ","                 { COLON }
  | "."                 { DOT }
  | id                  { VAR (Lexing.lexeme lexbuf) }
