type token =
  | INT of (int)
  | REAL of (float)
  | VAR of (string)
  | EOF
  | TRUE
  | FALSE
  | NOT
  | WHILE
  | DO
  | IF
  | THEN
  | ELSE
  | CHI
  | ALPHA
  | EQUAL
  | COLON
  | SEMICOLON
  | DOT
  | LPAREN
  | RPAREN
  | LBRACK
  | RBRACK
  | COMMA
  | RBRACE
  | LBRACE
  | LOG
  | SIGN
  | OR
  | AND
  | XOR
  | ADD
  | SUB
  | MUL
  | DIV
  | LESS
  | LEQ
  | GREATER
  | GEQ
  | SKIP
  | QUERY
  | LEFTARROW

val toplevel :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf ->  Syntax.lcommand 
