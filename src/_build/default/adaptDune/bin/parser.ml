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

open Parsing;;
let _ = parse_error;;
# 2 "adaptDune/bin/parser.mly"
open Syntax
# 49 "adaptDune/bin/parser.ml"
let yytransl_const = [|
    0 (* EOF *);
  260 (* TRUE *);
  261 (* FALSE *);
  262 (* NOT *);
  263 (* WHILE *);
  264 (* DO *);
  265 (* IF *);
  266 (* THEN *);
  267 (* ELSE *);
  268 (* CHI *);
  269 (* ALPHA *);
  270 (* EQUAL *);
  271 (* COLON *);
  272 (* SEMICOLON *);
  273 (* DOT *);
  274 (* LPAREN *);
  275 (* RPAREN *);
  276 (* LBRACK *);
  277 (* RBRACK *);
  278 (* COMMA *);
  279 (* RBRACE *);
  280 (* LBRACE *);
  281 (* LOG *);
  282 (* SIGN *);
  283 (* OR *);
  284 (* AND *);
  285 (* XOR *);
  286 (* ADD *);
  287 (* SUB *);
  288 (* MUL *);
  289 (* DIV *);
  290 (* LESS *);
  291 (* LEQ *);
  292 (* GREATER *);
  293 (* GEQ *);
  294 (* SKIP *);
  295 (* QUERY *);
  296 (* LEFTARROW *);
    0|]

let yytransl_block = [|
  257 (* INT *);
  258 (* REAL *);
  259 (* VAR *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\002\000\002\000\002\000\007\000\
\007\000\007\000\007\000\007\000\008\000\008\000\009\000\009\000\
\009\000\009\000\009\000\003\000\003\000\005\000\005\000\005\000\
\006\000\006\000\006\000\006\000\006\000\004\000\004\000\004\000\
\004\000\000\000"

let yylen = "\002\000\
\002\000\003\000\001\000\006\000\009\000\009\000\013\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\006\000\
\001\000\001\000\002\000\006\000\006\000\001\000\004\000\001\000\
\006\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\003\000\034\000\000\000\
\000\000\000\000\000\000\001\000\000\000\025\000\026\000\000\000\
\015\000\013\000\014\000\018\000\016\000\019\000\017\000\000\000\
\000\000\000\000\000\000\000\000\000\000\027\000\000\000\000\000\
\000\000\000\000\022\000\023\000\012\000\009\000\008\000\010\000\
\011\000\000\000\000\000\020\000\021\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\030\000\000\000\032\000\000\000\004\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\028\000\029\000\000\000\000\000\000\000\000\000\000\000\
\000\000\006\000\000\000\031\000\005\000\000\000\024\000\000\000\
\000\000\000\000\033\000\000\000\007\000"

let yydgoto = "\002\000\
\007\000\008\000\043\000\060\000\061\000\024\000\046\000\025\000\
\026\000"

let yysindex = "\006\000\
\074\255\000\000\249\254\254\254\016\255\000\000\000\000\002\000\
\068\255\068\255\238\254\000\000\074\255\000\000\000\000\068\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\005\255\
\009\255\010\255\008\255\032\255\014\255\000\000\031\255\068\255\
\011\255\044\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\033\255\035\255\000\000\000\000\036\255\045\255\048\255\
\053\255\047\255\077\255\057\255\011\255\052\255\068\255\011\255\
\055\255\069\255\000\000\066\255\000\000\070\255\000\000\064\255\
\074\255\072\255\073\255\074\255\011\255\076\255\077\255\011\255\
\248\254\000\000\000\000\250\254\078\255\086\255\079\255\064\255\
\080\255\000\000\087\255\000\000\000\000\077\255\000\000\089\255\
\081\255\074\255\000\000\000\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\001\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\243\255\000\000\190\255\234\255\249\255\209\255\000\000\
\000\000"

let yytablesize = 280
let yytable = "\029\000\
\002\000\012\000\027\000\062\000\079\000\044\000\001\000\013\000\
\030\000\013\000\049\000\035\000\009\000\036\000\082\000\013\000\
\083\000\010\000\011\000\089\000\045\000\028\000\093\000\062\000\
\048\000\031\000\032\000\033\000\034\000\013\000\064\000\047\000\
\035\000\067\000\036\000\014\000\015\000\016\000\062\000\037\000\
\038\000\039\000\040\000\041\000\050\000\017\000\077\000\066\000\
\080\000\081\000\051\000\073\000\054\000\053\000\076\000\052\000\
\057\000\063\000\018\000\019\000\037\000\038\000\039\000\040\000\
\041\000\020\000\021\000\022\000\023\000\055\000\042\000\014\000\
\015\000\016\000\056\000\065\000\092\000\035\000\068\000\036\000\
\003\000\017\000\004\000\069\000\070\000\072\000\085\000\071\000\
\058\000\059\000\074\000\075\000\084\000\005\000\018\000\019\000\
\078\000\088\000\087\000\091\000\086\000\020\000\021\000\022\000\
\023\000\037\000\038\000\039\000\040\000\041\000\000\000\006\000\
\090\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\013\000\000\000\000\000\000\000\000\000\000\000\002\000"

let yycheck = "\013\000\
\000\000\000\000\010\000\051\000\071\000\028\000\001\000\016\001\
\016\000\016\001\033\000\001\001\020\001\003\001\023\001\016\001\
\023\001\020\001\003\001\086\000\028\000\040\001\023\001\071\000\
\032\000\021\001\018\001\018\001\021\001\016\001\053\000\001\001\
\001\001\056\000\003\001\004\001\005\001\006\001\086\000\029\001\
\030\001\031\001\032\001\033\001\001\001\014\001\069\000\055\000\
\071\000\072\000\018\001\065\000\008\001\018\001\068\000\021\001\
\010\001\001\001\027\001\028\001\029\001\030\001\031\001\032\001\
\033\001\034\001\035\001\036\001\037\001\022\001\039\001\004\001\
\005\001\006\001\022\001\024\001\090\000\001\001\024\001\003\001\
\007\001\014\001\009\001\015\001\019\001\022\001\001\001\018\001\
\012\001\013\001\019\001\019\001\015\001\020\001\027\001\028\001\
\021\001\011\001\019\001\019\001\022\001\034\001\035\001\036\001\
\037\001\029\001\030\001\031\001\032\001\033\001\255\255\038\001\
\024\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\016\001\255\255\255\255\255\255\255\255\255\255\023\001"

let yynames_const = "\
  EOF\000\
  TRUE\000\
  FALSE\000\
  NOT\000\
  WHILE\000\
  DO\000\
  IF\000\
  THEN\000\
  ELSE\000\
  CHI\000\
  ALPHA\000\
  EQUAL\000\
  COLON\000\
  SEMICOLON\000\
  DOT\000\
  LPAREN\000\
  RPAREN\000\
  LBRACK\000\
  RBRACK\000\
  COMMA\000\
  RBRACE\000\
  LBRACE\000\
  LOG\000\
  SIGN\000\
  OR\000\
  AND\000\
  XOR\000\
  ADD\000\
  SUB\000\
  MUL\000\
  DIV\000\
  LESS\000\
  LEQ\000\
  GREATER\000\
  GEQ\000\
  SKIP\000\
  QUERY\000\
  LEFTARROW\000\
  "

let yynames_block = "\
  INT\000\
  REAL\000\
  VAR\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 :  Syntax.lcommand ) in
    Obj.repr(
# 55 "adaptDune/bin/parser.mly"
        ( _1 )
# 290 "adaptDune/bin/parser.ml"
               :  Syntax.lcommand ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 :  Syntax.lcommand ) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 :  Syntax.lcommand ) in
    Obj.repr(
# 59 "adaptDune/bin/parser.mly"
      ( Seq (_1, _3) )
# 298 "adaptDune/bin/parser.ml"
               :  Syntax.lcommand ))
; (fun __caml_parser_env ->
    Obj.repr(
# 61 "adaptDune/bin/parser.mly"
     (Skip)
# 304 "adaptDune/bin/parser.ml"
               :  Syntax.lcommand ))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 :  Syntax.expr ) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 63 "adaptDune/bin/parser.mly"
     ( Assign ({v_name = _2} , _4, Label(_6) ) )
# 313 "adaptDune/bin/parser.ml"
               :  Syntax.lcommand ))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 3 :  Syntax.query_expr ) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 65 "adaptDune/bin/parser.mly"
     ( Query  ({v_name = _2} , _6, Label(_9)) )
# 322 "adaptDune/bin/parser.ml"
               :  Syntax.lcommand ))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 :  Syntax.b_expr ) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : int) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 :  Syntax.lcommand ) in
    Obj.repr(
# 67 "adaptDune/bin/parser.mly"
     ( While  (_3 , _8, Label(_5) ) )
# 331 "adaptDune/bin/parser.ml"
               :  Syntax.lcommand ))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 10 :  Syntax.b_expr ) in
    let _5 = (Parsing.peek_val __caml_parser_env 8 : int) in
    let _8 = (Parsing.peek_val __caml_parser_env 5 :  Syntax.lcommand ) in
    let _12 = (Parsing.peek_val __caml_parser_env 1 :  Syntax.lcommand ) in
    Obj.repr(
# 69 "adaptDune/bin/parser.mly"
     ( If ( _3 , _8 , _12 , Label(_5) )  )
# 341 "adaptDune/bin/parser.ml"
               :  Syntax.lcommand ))
; (fun __caml_parser_env ->
    Obj.repr(
# 72 "adaptDune/bin/parser.mly"
                        ( Sub )
# 347 "adaptDune/bin/parser.ml"
               :  Syntax.aop ))
; (fun __caml_parser_env ->
    Obj.repr(
# 73 "adaptDune/bin/parser.mly"
                        ( Add )
# 353 "adaptDune/bin/parser.ml"
               :  Syntax.aop ))
; (fun __caml_parser_env ->
    Obj.repr(
# 74 "adaptDune/bin/parser.mly"
                        ( Mul )
# 359 "adaptDune/bin/parser.ml"
               :  Syntax.aop ))
; (fun __caml_parser_env ->
    Obj.repr(
# 75 "adaptDune/bin/parser.mly"
                        ( Div )
# 365 "adaptDune/bin/parser.ml"
               :  Syntax.aop ))
; (fun __caml_parser_env ->
    Obj.repr(
# 76 "adaptDune/bin/parser.mly"
                        ( Xor )
# 371 "adaptDune/bin/parser.ml"
               :  Syntax.aop ))
; (fun __caml_parser_env ->
    Obj.repr(
# 79 "adaptDune/bin/parser.mly"
                        ( Or )
# 377 "adaptDune/bin/parser.ml"
               :  Syntax.bop ))
; (fun __caml_parser_env ->
    Obj.repr(
# 80 "adaptDune/bin/parser.mly"
                        ( And )
# 383 "adaptDune/bin/parser.ml"
               :  Syntax.bop ))
; (fun __caml_parser_env ->
    Obj.repr(
# 83 "adaptDune/bin/parser.mly"
                        ( Equal )
# 389 "adaptDune/bin/parser.ml"
               :  Syntax.cop ))
; (fun __caml_parser_env ->
    Obj.repr(
# 84 "adaptDune/bin/parser.mly"
                        ( Leq )
# 395 "adaptDune/bin/parser.ml"
               :  Syntax.cop ))
; (fun __caml_parser_env ->
    Obj.repr(
# 85 "adaptDune/bin/parser.mly"
                        ( Geq )
# 401 "adaptDune/bin/parser.ml"
               :  Syntax.cop ))
; (fun __caml_parser_env ->
    Obj.repr(
# 86 "adaptDune/bin/parser.mly"
                        ( LessThan )
# 407 "adaptDune/bin/parser.ml"
               :  Syntax.cop ))
; (fun __caml_parser_env ->
    Obj.repr(
# 87 "adaptDune/bin/parser.mly"
                        ( GreaterThan )
# 413 "adaptDune/bin/parser.ml"
               :  Syntax.cop ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 :  Syntax.a_expr ) in
    Obj.repr(
# 90 "adaptDune/bin/parser.mly"
                        (  Eaexpr (_1) )
# 420 "adaptDune/bin/parser.ml"
               :  Syntax.expr ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 :  Syntax.b_expr ) in
    Obj.repr(
# 91 "adaptDune/bin/parser.mly"
                        (  Ebexpr (_1) )
# 427 "adaptDune/bin/parser.ml"
               :  Syntax.expr ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 94 "adaptDune/bin/parser.mly"
                                                    ( Aint (_1) )
# 434 "adaptDune/bin/parser.ml"
               :  Syntax.a_expr ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 95 "adaptDune/bin/parser.mly"
                                                    ( Avar {v_name = _1} )
# 441 "adaptDune/bin/parser.ml"
               :  Syntax.a_expr ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 :  Syntax.aop ) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 :  Syntax.a_expr ) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 :  Syntax.a_expr ) in
    Obj.repr(
# 96 "adaptDune/bin/parser.mly"
                                                    ( Aaop ( _1, _3, _5 )  )
# 450 "adaptDune/bin/parser.ml"
               :  Syntax.a_expr ))
; (fun __caml_parser_env ->
    Obj.repr(
# 99 "adaptDune/bin/parser.mly"
                                                    ( BTrue )
# 456 "adaptDune/bin/parser.ml"
               :  Syntax.b_expr ))
; (fun __caml_parser_env ->
    Obj.repr(
# 100 "adaptDune/bin/parser.mly"
                                                    ( BFalse )
# 462 "adaptDune/bin/parser.ml"
               :  Syntax.b_expr ))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 :  Syntax.b_expr ) in
    Obj.repr(
# 101 "adaptDune/bin/parser.mly"
                                                    ( BNeg _2 )
# 469 "adaptDune/bin/parser.ml"
               :  Syntax.b_expr ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 :  Syntax.bop ) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 :  Syntax.b_expr ) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 :  Syntax.b_expr ) in
    Obj.repr(
# 102 "adaptDune/bin/parser.mly"
                                                     ( Bbop (_1, _3, _5) )
# 478 "adaptDune/bin/parser.ml"
               :  Syntax.b_expr ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 :  Syntax.cop ) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 :  Syntax.a_expr ) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 :  Syntax.a_expr ) in
    Obj.repr(
# 103 "adaptDune/bin/parser.mly"
                                                    ( Bcop (_1, _3, _5 ) )
# 487 "adaptDune/bin/parser.ml"
               :  Syntax.b_expr ))
; (fun __caml_parser_env ->
    Obj.repr(
# 106 "adaptDune/bin/parser.mly"
                                  (Qalpha )
# 493 "adaptDune/bin/parser.ml"
               :  Syntax.query_expr ))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 :  Syntax.a_expr ) in
    Obj.repr(
# 107 "adaptDune/bin/parser.mly"
                               (Qchi (_3))
# 500 "adaptDune/bin/parser.ml"
               :  Syntax.query_expr ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 :  Syntax.a_expr ) in
    Obj.repr(
# 108 "adaptDune/bin/parser.mly"
                                 (Qaexpr (_1))
# 507 "adaptDune/bin/parser.ml"
               :  Syntax.query_expr ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 :  Syntax.aop ) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 :  Syntax.query_expr ) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 :  Syntax.query_expr ) in
    Obj.repr(
# 109 "adaptDune/bin/parser.mly"
                                                     ( Qaop ( _1, _3, _5 )  )
# 516 "adaptDune/bin/parser.ml"
               :  Syntax.query_expr ))
(* Entry toplevel *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let toplevel (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf :  Syntax.lcommand )
