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
let underscore = ['_']
let apostrophe = ['\'']
let int = '-'? digit+
let float = '-'? digit+ '.' digit+
let letter = ['a'-'z' 'A'-'Z']
let id = letter+ (digit|letter|underscore|apostrophe)*

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
  | "dot"               { DOT }

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
  | "pack"              { PACK }
  | "unpack"            { UNPACK }
  | "bernoulli"         { BERNOULLI }
  | "uniform"           { UNIFORM }
  | "dmap"              { DMAP }

  | "Lambda"            { BIGLAM }

  | "log"               { LOG }
  | "sign"              { SIGN }

  | "->"                { ARROW }
  | "|>"                { VDASH }
  | "||"                { OR }
  | "&&"                { AND }
  | "^"                 { XOR }
  | "-"                 { SUB }
  | "+"                 { ADD }
  | "*"                 { MUL }
  | "/"                 { DIV }
  | "\\"                { SETMINUS }
  | "<"                 { LESS }
  | "<="                { LEQ }
  | ">"                 { GREATER }
  | ">="                { GEQ }
  | "::"                { DBCOLON }
  | "()"                { UNITV }
  | "["                 { LBRACK }
  | "]"                 { RBRACK }
  | "("                 { LPAREN }
  | ")"                 { RPAREN }
  | "="                 { EQUAL }
  | ":"                 { COLON }
  | ","                 { COMMA }
  | "."                 { DOT }
  | ";"                 { SEMICOLON }

  | "X"                 { TIMES }
  | "box"               { BOX }
  | "list"              { LIST }
  | "forall"            { FORALL }
  | "exists"            { EXISTS }
  | "adapt"             { ADAPT }

  | "check"             { CHECK }
  | "infer"             { INFER }
  | "max"               { MAX }
  | "min"               { MIN }


  | id                  { VAR (Lexing.lexeme lexbuf) }
