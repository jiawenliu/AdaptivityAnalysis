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


rule main = 
  parse
  | white               { main lexbuf }
  | int                 { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | float               { REAL (float_of_string (Lexing.lexeme lexbuf))}
  | eof                 {  EOF }
  | "true"              { TRUE }
  | "false"             { FALSE }
  | "not"               { NOT }
  | "while"             { WHILE }
  | "do"                { Parser.DO }
  | "if"                { IF }
  | "then"              { THEN }
  | "else"              { ELSE }
   | "chi"               { CHI }
   | "alpha"             { ALPHA }
   | "="                 { EQUAL }
  | ":"                 { COLON }
  | "."                 { DOT }
  | ";"                 { SEMICOLON }
   | "["                 { LBRACK }
  | "]"                 { RBRACK }
  | "("                 { LPAREN }
  | ")"                 { RPAREN }
  | "{"                 { LBRACE }
  | "}"                 { RBRACE }
    | ","                 { COMMA }

  | "log"               { LOG }
  | "sign"              { SIGN } 
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
  
  
  
 
  | "skip"              { SKIP }
  | "query"             { Parser.QUERY }
   | "<-"                { LEFTARROW }
  | id                  { VAR (Lexing.lexeme lexbuf) }



        (* rule token = parse
            [' ' '\t']     { token lexbuf }     (* skip blanks *)
          | ['\n' ]        { EOL }
          | ['0'-'9']+ as lxm { INT(int_of_string lxm) }
          | '+'            { PLUS }
          | '-'            { MINUS }
          | '*'            { TIMES }
          | '/'            { DIV }
          | '('            { LPAREN }
          | ')'            { RPAREN }
          | eof            { raise Eof } *)
