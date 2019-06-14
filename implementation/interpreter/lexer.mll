{
open Parser
open Lexing
exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }
    
}

let white = [' ' '\t' '\n']+
let digit = ['0'-'9']
let int = '-'? digit+
let float = '-'? digit+ '.' digit+
let letter = ['a'-'z' 'A'-'Z']
let id = letter+ (digit|letter)*

(* The final section of the lexer definition defines how to parse a character
   stream into a token stream.  Each of the rules below has the form 
     | regexp { action }
   If the lexer sees the regular expression [regexp], it produces the token 
   specified by the [action].  We won't go into details on how the actions
   work.  *)

rule main = 
  parse
  | white               { main lexbuf }
  | int                 { INTV (int_of_string (Lexing.lexeme lexbuf)) }
  | float               { REALV (float_of_string (Lexing.lexeme lexbuf))}
  | eof                 { EOF }

  | "int"               { INT }
  | "bool"              { BOOL }
  | "real"              { REAL }
  | "unit"              { UNIT }

  | "nil"               { NIL }

  | "index"             { INDEX }

  | "fix"               { FIX }
  | "if"                { IF }
  | "lambda"            { LAM }
  | "let"               { LET }
  | "in"                { IN }
  | "mech"              { MECH }
  | "true"              { TRUE }
  | "false"             { FALSE }

  | "Lambda"            { BIGLAM }

  | "log"               { LOG }
  | "sign"              { SIGN }

  | "<<"                { LEFTSHIFT }
  | "||"                { OR }
  | "&&"                { AND }
  | "^"                 { XOR }
  | "-"                 { SUB }
  | "+"                 { ADD }
  | "*"                 { MUL }
  | "/"                 { DIV }
  | "<"                 { LESS }
  | "<="                { LEQ }
  | ">"                 { GREATER }
  | ">="                { GEQ }
  | "::"                { CONS }
  | "[]"                { NIL }
  | "()"                { UNITV }
  | "["                 { LBRACK }
  | "]"                 { RBRACK }
  | "("                 { LPAREN }
  | ")"                 { RPAREN }
  | "="                 { EQUAL }
  | ":"                 { COLON }
  | ","                 { COMMA }
  | "."                 { DOT }

  | "check"             { CHECK }
  | "infer"             { INFER }


  | id                  { VAR (Lexing.lexeme lexbuf) }
