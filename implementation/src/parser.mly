%{
open Syntax
open IndexSyntax
open Constr
open Support.FileInfo

let parser_error   fi = Support.Error.error_msg   Support.Options.Parser fi
let parser_warning fi = Support.Error.message   1 Support.Options.Parser fi
let parser_info    fi = Support.Error.message   2 Support.Options.Parser fi

let dummy_ty  = Ty_Prim Ty_PrimUnit


(* look for a variable in the context *)
let existing_var fi id ctx =
  match Ctx.lookup_var id ctx with
      None            -> parser_error fi "Identifier %s is unbound" id
    | Some (var, _) -> var

(* look for an index variable in the context *)
let existing_ivar fi id ctx =
  match Ctx.lookup_ivar id ctx with
      None            -> parser_error fi "Index variable %s is unbound" id
    | Some (var, s)  -> (var, s)

(* look for an existential variable in the context *)
let existing_evar fi id ctx =
  match Ctx.lookup_evar id ctx with
      None            -> parser_error fi "Index variable %s is unbound" id
    | Some (var, s)  -> (var, s)

let extend_var id ctx =
  Ctx.extend_var id dummy_ty ctx

let extend_i_var id s ctx =
  Ctx.extend_i_var id s ctx

let extend_l_var id ctx =
  Ctx.extend_l_var id ctx

(* Create a new binder *)
(* TODO: set the proper b_size !!! *)
let nb_prim  n = {v_name = n; v_type = Var}
let nb_var   n = {v_name = n; v_type = Var}
let nb_ivar n = {v_name = n; v_type = IVar}
let nb_lvar n = {v_name = n; v_type = LVar}


(* From a list of arguments to a universally quantified unary type *)
let rec qf_to_forall_type qf u_ty = match qf with
    []               -> u_ty
  | (_, n, t, s) :: qfl -> 
    Ty_Forall(nb_lvar n, t, s, qf_to_forall_type qfl u_ty)

(* From a list of arguments to an existentially quantified unary type *)
let rec qf_to_exists_type qf u_ty = match qf with
    []               -> u_ty
  | (_, n, t, s) :: qfl -> Ty_Exists(nb_ivar n, t, s, qf_to_exists_type qfl u_ty)
                                        

(* from (v:string, list int) list -> predicate *)
(* let rec predicate_trans ivs = match ivs with
      [] -> []
      | (v, o, l) :: tl -> let n_lvar = {v_name = v; v_type = LVar;} in 
                        (n_lvar,  IArray (o, l) ) :: (predicate_trans tl)
*)

%}

/* ---------------------------------------------------------------------- */
/* Preliminaries */

/* Keyword tokens */
%token <Support.FileInfo.info> AT
%token <Support.FileInfo.info> ADD
%token <Support.FileInfo.info> AMP
%token <Support.FileInfo.info> AND
%token <Support.FileInfo.info> ARROW
%token <Support.FileInfo.info> LARROW
%token <Support.FileInfo.info> COLON
%token <Support.FileInfo.info> CONS
%token <Support.FileInfo.info> COMMA
%token <Support.FileInfo.info> DASH
%token <Support.FileInfo.info> LBRACE
%token <Support.FileInfo.info> SEMI
%token <Support.FileInfo.info> RBRACE
%token <Support.FileInfo.info> EQUAL
%token <Support.FileInfo.info> HAT
%token <Support.FileInfo.info> DBLARROW
%token <Support.FileInfo.info> SUB
%token <Support.FileInfo.info> MUL
%token <Support.FileInfo.info> TIMES
%token <Support.FileInfo.info> DIV
%token <Support.FileInfo.info> LPAREN
%token <Support.FileInfo.info> RPAREN
/* Comparison */
%token <Support.FileInfo.info> LEQ
%token <Support.FileInfo.info> GEQ
%token <Support.FileInfo.info> LESS
%token <Support.FileInfo.info> GREATER


%token <Support.FileInfo.info> LBRACK
%token <Support.FileInfo.info> RBRACK
%token <Support.FileInfo.info> PIPE
%token <Support.FileInfo.info> OR
%token <Support.FileInfo.info> TRUE
%token <Support.FileInfo.info> FALSE
%token <Support.FileInfo.info> INF
%token <Support.FileInfo.info> UNIT
%token <Support.FileInfo.info> UNITR
%token <Support.FileInfo.info> INL
%token <Support.FileInfo.info> INR
%token <Support.FileInfo.info> FUN
%token <Support.FileInfo.info> UNIONCASE
%token <Support.FileInfo.info> LISTCASE
%token <Support.FileInfo.info> OF
%token <Support.FileInfo.info> AS
%token <Support.FileInfo.info> DIFF
%token <Support.FileInfo.info> MAX
%token <Support.FileInfo.info> MIN
%token <Support.FileInfo.info> SND
%token <Support.FileInfo.info> FST
%token <Support.FileInfo.info> NIL
%token <Support.FileInfo.info> MU
%token <Support.FileInfo.info> LET
%token <Support.FileInfo.info> CLET
%token <Support.FileInfo.info> FIX
%token <Support.FileInfo.info> PRIMITIVE
%token <Support.FileInfo.info> BIGLAMBDA
%token <Support.FileInfo.info> LAMBDA
%token <Support.FileInfo.info> IF
%token <Support.FileInfo.info> THEN
%token <Support.FileInfo.info> ELSE
%token <Support.FileInfo.info> PRINT
%token EOF
%token <Support.FileInfo.info> BOOL
%token <Support.FileInfo.info> BOOLR
%token <Support.FileInfo.info> NUM
%token <Support.FileInfo.info> STRING
%token <Support.FileInfo.info> SIZE
%token <Support.FileInfo.info> SENS
%token <Support.FileInfo.info> TYPE
%token <Support.FileInfo.info> PACK
%token <Support.FileInfo.info> WITH
%token <Support.FileInfo.info> IN
%token <Support.FileInfo.info> COST
%token <Support.FileInfo.info> SIZE
%token <Support.FileInfo.info> UNPACK
%token <Support.FileInfo.info> FORALL
%token <Support.FileInfo.info> EXISTS
%token <Support.FileInfo.info> LIST
%token <Support.FileInfo.info> DBLCOLON
%token <Support.FileInfo.info> NAT
%token <Support.FileInfo.info> INT
%token <Support.FileInfo.info> INTR
%token <Support.FileInfo.info> DOT
%token <Support.FileInfo.info> ZERO
%token <Support.FileInfo.info> SUCC
%token <Support.FileInfo.info> UNREL
%token <Support.FileInfo.info> CONTRA
%token <Support.FileInfo.info> FLOOR
%token <Support.FileInfo.info> CEIL
%token <Support.FileInfo.info> LOG
%token <Support.FileInfo.info> MINPOWLIN
%token <Support.FileInfo.info> MINPOWCONSTANT
%token <Support.FileInfo.info> SUM
%token <Support.FileInfo.info> BOX
%token <Support.FileInfo.info> BERNOULLI
%token <Support.FileInfo.info> UNIFORM

/* Operations */
%token <Support.FileInfo.info> SIGN

/* Identifier and constant value tokens */
%token <string Support.FileInfo.withinfo> ID
%token <int    Support.FileInfo.withinfo> INTV
%token <float  Support.FileInfo.withinfo> FLOATV

/*monadic tokens*/
%token <Support.FileInfo.info> ALLOC
%token <Support.FileInfo.info> READ
%token <Support.FileInfo.info> UPDATE
%token <Support.FileInfo.info> RETURN
%token <Support.FileInfo.info> LETM
%token <Support.FileInfo.info> UINT
%token <Support.FileInfo.info> BINT
%token <Support.FileInfo.info> ARRAY
%token <Support.FileInfo.info> LOC
%token <Support.FileInfo.info> CELIM
%token <Support.FileInfo.info> IO
%token <Support.FileInfo.info> LT

%token <Support.FileInfo.info> ARR
%token <Support.FileInfo.info> UNION
%token <Support.FileInfo.info> INTERSECT
%token <Support.FileInfo.info> SETDIFF
%token <Support.FileInfo.info> CBETAIN
%token <Support.FileInfo.info> SPLIT
%token <Support.FileInfo.info> WITH
%token <Support.FileInfo.info> FIXEXT
%token <Support.FileInfo.info> IE
%token <Support.FileInfo.info> CBETAEQ
%token <Support.FileInfo.info> SWITCH
%token <Support.FileInfo.info> BETAMIN
%token <Support.FileInfo.info> INTMAX
%token <Support.FileInfo.info> INTMIN
%token <Support.FileInfo.info> CNOT 


/* ---------------------------------------------------------------------- */
/* RelCost grammar                                                           */
/* ---------------------------------------------------------------------- */

%start u_toplevel
%type < Syntax.expr * IndexSyntax.iterm * Syntax.ty * Syntax.mode > u_toplevel
%%

/* ---------------------------------------------------------------------- */
/* Main body of the parser definition     m                                */
/* ---------------------------------------------------------------------- */

u_toplevel :
    Expr LEQ LBRACK Mode COMMA ITerm RBRACK COLON Type EOF
        { let ctx = Ctx.set_exec_mode $4 (Ctx.empty_context) in
          ($1 ctx, $6 ctx, $9 ctx, $4) }



Term :
    LET ID EQUAL Term IN Term
        {
          fun ctx ->
          let ctx' = extend_var $2.v ctx in
          Let( (nb_var $2.v), $4 ctx, $6 ctx')
        }
        

    | FIX ID LPAREN ID RPAREN DOT Term
      {
        fun ctx ->
        let ctx_f = extend_var $2.v ctx   in
        let ctx_x = extend_var $4.v ctx_f in
        Fix( nb_var $2.v, nb_var $4.v, $7 ctx_x)
      }
    | LAMBDA ID DOT Term
      {
        fun ctx ->
        let ctx_x = extend_var $2.v ctx   in
        Fix(nb_var "_", nb_var $2.v, $4 ctx_x)
      }

    | BIGLAMBDA DOT Term
      {
        fun ctx -> let e = $3 ctx in ILam( e)
      }
    |  IF Expr THEN Term ELSE Term 
      { fun ctx -> If($2 ctx, $4 ctx, $6 ctx)
      }

    | PACK Term
      { fun ctx -> Pack ($2 ctx) }

    | UNPACK Term AS ID IN Term
      { fun ctx ->
        let ctx' = extend_var $4.v ctx in
        Unpack($2 ctx, nb_var $4.v, $6 ctx')
      }
    | UNIONCASE Expr OF INL LPAREN ID RPAREN DBLARROW Term PIPE INR LPAREN ID RPAREN DBLARROW Term 
      { fun ctx ->
        let ctx_l = extend_var $6.v  ctx in
        let ctx_r = extend_var $13.v ctx in
        Case($2 ctx, nb_var $6.v, $9 ctx_l, nb_var $13.v, $16 ctx_r) }



/* Applications */
App:
   App Expr
  { fun ctx ->
      let e1 = $1 ctx in
      let e2 = $2 ctx in
      App (e1, e2) 
  }

  |  Expr 
     { $1 }
  | App LBRACK RBRACK { fun ctx -> let e = $1 ctx in IApp( e) } 


/* Syntactic sugar for n-ary tuples */
PairSeq:
    Term COMMA Term 
    { fun ctx -> Pair($1 ctx, $3 ctx) }
  | Term COMMA PairSeq 
    { fun ctx -> Pair($1 ctx, $3 ctx)  }


Expr:
    TRUE
     { fun _cx -> Prim (PrimBool true) }
  | FALSE
     { fun _cx -> Prim (PrimBool false) }
  | INTV
     { fun _cx -> Prim (PrimInt $1.v) }

  | FLOATV
     { fun _cx -> Prim (PrimReal $1.v) }

  | NIL
     { fun _cx -> Nil }

  | LPAREN RPAREN
     { fun _cx -> Prim (PrimUnit) }

  | ID
     { fun ctx -> Var(existing_var $1.i $1.v ctx) }

  | LPAREN Term RPAREN
     { $2 }

  | FST Term
     { fun ctx -> Fst ($2 ctx) }

  | SND Term   
     { fun ctx -> Snd ($2 ctx) }    

  | LPAREN PairSeq RPAREN 
     { $2 }

  | Term DBLCOLON Term
    { fun ctx -> Cons($1 ctx, $3 ctx) }

  | uop LPAREN Expr RPAREN
    { fun ctx -> Uop($1 ctx, $3 ctx) }                                                  

  | Expr bop Expr
    { fun ctx -> Bop($2 ctx, $1 ctx, $3 ctx) }

  | BERNOULLI LPAREN FLOATV RPAREN
    { fun ctx -> Bernoulli (PrimReal $3.v)}

  | UNIFORM LPAREN FLOATV COMMA FLOATV RPAREN
    { fun ctx -> Uniform(PrimReal $3.v, PrimReal $5.v)}



/* Operations */

bop:
    | ADD           { fun ctx -> Add }
    | SUB           { fun ctx -> Sub }
    | MUL           { fun ctx -> Mul }
    | DIV           { fun ctx -> Div }
    | OR            { fun ctx -> Or }
    | AND           { fun ctx -> And }
    | EQUAL         { fun ctx -> Equal }
    | LEQ           { fun ctx -> Leq }
    | GEQ           { fun ctx -> Geq }
    | LESS          { fun ctx -> Less }
    | GREATER       { fun ctx -> Greater }


uop:
    | SIGN          { fun ctx -> Sign }
    | LOG           { fun ctx -> Log }

 
/* Sorts */
Sort :
    SIZE
      { fun ctx -> Adap }


Mode:
   MAX
    { MaxEx }
  | MIN
    { MinEx }


  /* Types */
Type:
    Type LBRACK Mode COMMA ITerm RBRACK ARROW Type
    { fun ctx -> 
      Ty_Arr($1 ctx, $3, $5 ctx, $8 ctx) }
  | Type ARROW Type
    { fun ctx -> 
      Ty_Arr($1 ctx, MaxEx, (IConst 0), $3 ctx) }

  | Type ARROW ARROW Type
    { fun ctx -> Ty_Arr($1 ctx, MinEx, (IConst 0), $4 ctx) }

  | LIST LBRACK ITerm RBRACK Type
    { fun ctx -> Ty_List($3 ctx, $5 ctx) }

  | Type
    { $1 }


Type:
    LPAREN Type RPAREN
    { $2 }
  | BOOL
    { fun _cx -> Ty_Prim Ty_PrimBool }

  | INT
    { fun _cx ->  Ty_Prim Ty_PrimInt }
  | UNIT
    { fun _cx ->  Ty_Prim Ty_PrimUnit }
  | UTPairSeq
    { fun ctx -> $1 ctx }
  

UTPairSeq:
    Type TIMES Type
    { fun ctx -> Ty_Prod($1 ctx, $3 ctx) }
  | Type TIMES UTPairSeq
    { fun ctx -> Ty_Prod($1 ctx, $3 ctx) }




/* Index terms */
 ITerm:
   ITerm ADD ITerm
    { fun ctx -> IAdd($1 ctx, $3 ctx) }

  | ITerm SUB ITerm
    { fun ctx -> IMinus($1 ctx, $3 ctx) }

  | LPAREN ITerm RPAREN
    { fun ctx -> $2 ctx }
  | ID
    { fun ctx -> let n_ivar = {v_name = $1.v; v_type = IVar;} in
                             IVar n_ivar                 
    }

  | INTMAX LPAREN ITerm COMMA ITerm RPAREN
      { fun ctx -> IMaximal($3 ctx, $5 ctx) }

/* Constraints */
Constr:
  | ITerm EQUAL ITerm       { fun ctx ->CEq($1 ctx,$3 ctx) }
  | ITerm LEQ ITerm         { fun ctx -> CLeq($1 ctx,$3 ctx) }
  | ITerm LT ITerm         { fun ctx -> CLt($1 ctx,$3 ctx) }
  | Constr AND Constr       { fun ctx -> CAnd($1 ctx,$3 ctx) }
  | LPAREN Constr RPAREN    { fun ctx -> $2 ctx }

  | CNOT Constr {fun ctx -> CNot ($2 ctx) }
