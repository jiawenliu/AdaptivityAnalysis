%{
open Syntax
open IndexSyntax
open Constr
open Support.FileInfo

let parser_error   fi = Support.Error.error_msg   Support.Options.Parser fi
let parser_warning fi = Support.Error.message   1 Support.Options.Parser fi
let parser_info    fi = Support.Error.message   2 Support.Options.Parser fi

let dummy_ty  = Ty_Prim UPrimUnit

let rec int_to_speano n = if n = 0 then IZero else ISucc (int_to_speano (n-1))

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
let nb_prim  n = {v_name = n; v_type = BiVar}
let nb_var   n = {v_name = n; v_type = BiVar}
let nb_ivar n = {v_name = n; v_type = BiIVar}
let nb_lvar n = {v_name = n; v_type = BiLVar}


(* From a list of arguments to a universally quantified unary type *)
let rec qf_to_forall_type qf u_ty = match qf with
    []               -> u_ty
  | (_, n, mu, t, s) :: qfl -> 
    if mu=Loc then Ty_Forall(nb_lvar n, mu, t, s, qf_to_forall_type qfl u_ty)
    else Ty_Forall(nb_ivar n, mu, t, s, qf_to_forall_type qfl u_ty)

(* From a list of arguments to an existentially quantified unary type *)
let rec qf_to_exists_type qf u_ty = match qf with
    []               -> u_ty
  | (_, n, s) :: qfl -> Ty_Exists(nb_ivar n, s, qf_to_exists_type qfl u_ty)
                                        

(* From a list of arguments to a universally quantified binary type *)
let rec qf_to_forall_btype qf bi_ty = match qf with
    []               -> bi_ty
  | (_, n, t, s) :: qfl -> BTyForall(nb_ivar n, t, s, qf_to_forall_btype qfl bi_ty)

(* From a list of arguments to an existentially quantified binary type *)
let rec qf_to_exists_btype qf bi_ty = match qf with
    []               -> bi_ty
  | (_, n, s) :: qfl -> BTyExists(nb_ivar n, s, qf_to_exists_btype qfl bi_ty)
                                        
(* from (v:string, list int) list -> predicate *)
let rec predicate_trans ivs = match ivs with
      [] -> []
      | (v, o, l) :: tl -> let n_lvar = {v_name = v; v_type = BiLVar;} in 
                        (n_lvar,  IArray (o, l) ) :: (predicate_trans tl)


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
%token <Support.FileInfo.info> LEQ
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
%token <Support.FileInfo.info> EOF
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
    Term LEQ LBRACK Mode COMMA ITerm RBRACK COLON Type EOF
        { let ctx = Ctx.set_exec_mode $4 (Ctx.empty_context) in
          ($1 ctx, $6 ctx, $9 ctx, $4) }



Term :
    LET ID EQUAL Term IN Term
        {
          fun ctx ->
          let ctx' = extend_var $2.v ctx in
          Let($2.i, (nb_var $2.v), $4 ctx, $6 ctx')
        }
        

    | CELIM Term 
        { fun ctx -> let e = $2 ctx in CExpr(expInfo e, e) }

    | FIX ID LPAREN ID RPAREN DOT Term
      {
        fun ctx ->
        let ctx_f = extend_var $2.v ctx   in
        let ctx_x = extend_var $4.v ctx_f in
        Fix($2.i, nb_var $2.v, nb_var $4.v, $7 ctx_x)
      }
    | LAMBDA ID DOT Term
      {
        fun ctx ->
        let ctx_x = extend_var $2.v ctx   in
        Fix($2.i, nb_var "_", nb_var $2.v, $4 ctx_x)
      }

    | BIGLAMBDA DOT Term
      {
        fun ctx -> let e = $3 ctx in ILam(expInfo e, e)
      }
    |  IF Expr THEN Term ELSE Term 
      { fun ctx -> IfThen($1, $2 ctx, $4 ctx, $6 ctx)
      }

    | PACK Term
      { fun ctx -> Pack ($1, $2 ctx) }

    | UNPACK Term AS ID IN Term
      { fun ctx ->
        let ctx' = extend_var $4.v ctx in
        Unpack($1,$2 ctx, nb_var $4.v, $6 ctx')
      }
    | UNIONCASE Expr OF INL LPAREN ID RPAREN DBLARROW Term PIPE INR LPAREN ID RPAREN DBLARROW Term 
      { fun ctx ->
        let ctx_l = extend_var $6.v  ctx in
        let ctx_r = extend_var $13.v ctx in
        Case($1, $2 ctx, nb_var $6.v, $9 ctx_l, nb_var $13.v, $16 ctx_r) }

    | LISTCASE Expr OF NIL DBLARROW Term PIPE ID DBLCOLON ID DBLARROW Term 
      { fun ctx ->
        let ctx_h  = extend_var $8.v ctx in
        let ctx_tl = extend_var $10.v ctx_h in
        CaseL($1, $2 ctx, $6 ctx, nb_var $8.v, nb_var $10.v, $12 ctx_tl) }


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
    { fun ctx -> Pair($2, $1 ctx, $3 ctx) }
  | Term COMMA PairSeq 
    { fun ctx -> Pair($2, $1 ctx, $3 ctx)  }


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



  | uop LPAREN expr RPAREN
    { fun ctx -> Uop($1 ctx, $3 ctx) }                                                  

  | expr bop expr
    { fun ctx -> Bop($2 ctx, $1 ctx, $3 ctx) }

  | BERNOULLI FLOATV
    { fun ctx -> Bernoulli (PrimReal $2.v)}

  | UNIFORM LPAREN FLOATV COMMA FLOATV RPAREN
    { fun ctx -> Uniform(PrimReal $3.v, PrimReal $5.v)}



/* Operations */

bop:
    | ADD           { Add }
    | SUB           { Sub }
    | MUL           { Mul }
    | DIV           { Div }
    | OR            { Or }
    | AND           { And }
    | EQUAL         { Equal }
    | LEQ           { Leq }
    | GEQ           { Geq }
    | LESS          { Less }
    | GREATER       { Greater }


uop:
    | SIGN          { Sign }
    | LOG           { Log }

 
/* Sorts */
Sort :
    SIZE
      { Adap }


Mode:
   MAX
    { MaxEx }
  | MIN
    { MinEx }


  /* Unary Types */
Type:
    Type LBRACK Mode COMMA ITerm RBRACK ARROW Type
    { fun ctx -> 
      Ty_Arr($1 ctx, $3, $5 ctx, $8 ctx) }
  | Type ARROW Type
    { fun ctx -> 
      Ty_Arr($1 ctx, MaxEx, IZero, $3 ctx) }

  | Type ARROW ARROW Type
    { fun ctx -> Ty_Arr($1 ctx, MinEx, IZero, $4 ctx) }

  | LIST LBRACK ITerm RBRACK type
    { fun ctx -> Ty_List($3 ctx, $5 ctx) }

  | Quantifiedtypes
    { $1 }

  | Constrainedtypes
    { $1 }

  | ConstrainedImptypes
    { $1 }

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
  | UINT LBRACK ITerm RBRACK
    { fun ctx -> UInt ($3 ctx) }
  | ARRAY LPAREN ID RPAREN LBRACK ITerm RBRACK type
     { fun ctx -> let n_lvar = {v_name =  $3.v ; v_type = BiLVar;} in 
          UArray ( n_lvar, $6 ctx, $8 ctx)  }
  

UTPairSeq:
    Type TIMES Type
    { fun ctx -> Ty_Prod($1 ctx, $3 ctx) }
  | Type TIMES UTPairSeq
    { fun ctx -> Ty_Prod($1 ctx, $3 ctx) }

FSortUAnn :
    ID LBRACK Mode COMMA ITerm RBRACK DBLCOLON Sort
      { fun ctx -> if ($8 = Loc) then ([($1.i, $1.v, $8, $3, $5 ctx)], extend_l_var $1.v ctx) 
      else  ([($1.i, $1.v, $8, $3, $5 ctx)], extend_i_var $1.v $8 ctx) }
       

ESortUAnn :
    ID DBLCOLON Sort
      { fun ctx -> ([($1.i, $1.v, $3)], extend_i_var $1.v $3 ctx) }
     
FQuantifierUList :
    FSortUAnn
      { $1 }
  | FSortUAnn SEMI FQuantifierUList
      { fun ctx -> let (iv, ctx')  = $1 ctx  in
                   let (qf, ctx_qf) = $3 ctx' in
                   (iv @ qf, ctx_qf)
      }

EQuantifierUList :
    ESortUAnn
      { $1 }
  | ESortUAnn SEMI EQuantifierUList
      { fun ctx -> let (iv, ctx')  = $1 ctx  in
                   let (qf, ctx_qf) = $3 ctx' in
                   (iv @ qf, ctx_qf)
      }

Quantifiedtypes :
   FORALL FQuantifierUList DOT type
          { fun ctx -> let (qf, ctx') =  $2 ctx in
                        qf_to_forall_type qf ($4 ctx')  }
  | EXISTS EQuantifierUList DOT type
    { fun ctx ->  let (qf, ctx') =  $2 ctx in qf_to_exists_type qf ($4 ctx') }


Constrainedtypes :
    LBRACE Constr RBRACE type
    { fun ctx -> Ty_Cs ($2 ctx, $4 ctx) }      

ConstrainedImptypes :
    LBRACE Constr RBRACE DBLARROW type
    { fun ctx -> Ty_CsImp ($2 ctx, $5 ctx) }     


/*  Predicates */

Predicates:
    LBRACE PContents RBRACE
    { $2
     }

PContents:
      PCt
      {$1}
    | PCt COMMA PContents
      { fun ctx -> let (iv, ctx') = $1 ctx in
                   let  (ivs, ctx'') = $3 ctx' in
                    (iv @ ivs, ctx'') }

PCts:
    ID ARROW  ITerm LBRACK LCts RBRACK 
    { fun ctx -> ([($1.v, $3 ctx, $5 ctx)], extend_l_var $1.v ctx) }
    | ID ARROW ITerm 
    { fun ctx -> ([($1.v, $3 ctx, [] )], extend_l_var $1.v ctx) }
    

PCt:
    ID ARROW ARRs
    { fun ctx -> 
    let n_lvar = {v_name = $1.v; v_type = BiLVar;} in 
    ([(n_lvar , IBeta ($3 ctx) )], extend_l_var $1.v ctx) }
   
    


ARRs:
     IO 
    {  fun ctx -> BIO}
    | IE
    { fun ctx -> BIE }
    | ID
    { fun ctx -> let n_lvar = {v_name = $1.v; v_type = BiIVar;} in BVar n_lvar }
    | LBRACK ITerm COMMA ITerm RBRACK
    { fun ctx ->  BRange ( $2 ctx, $4 ctx ) }
    | LBRACK ID RBRACK
    { fun ctx -> let n_ivar = {v_name = $2.v; v_type = BiIVar;} in BPos (IVar n_ivar )   }
    | LBRACK ITerm RBRACK
    { fun ctx -> BPos ($2 ctx)   }
    | ARRs UNION ARRs
     { fun ctx -> BUnion  ( ($1 ctx), ($3 ctx) )  }
    | ARRs INTERSECT ARRs 
      { fun ctx -> BIntersect  ( ($1 ctx), ($3 ctx) )  }
    | ARRs SETDIFF ARRs
      { fun ctx -> BSetDiff ( ($1 ctx), ($3 ctx) )  }
   

LCts:
    LCt
    {$1}
  | LCt SEMI LCts
    { fun ctx -> let h = $1 ctx in 
                let tl = $3 ctx in
                  (h @ tl) }

LCt:
    INTV 
    { fun ctx ->  [$1.v] }

FSortBAnn :
     ID LBRACK DIFF COMMA ITerm RBRACK DBLCOLON Sort
      { fun ctx -> ([($1.i, $1.v, $8, $5 ctx)], extend_i_var $1.v $8 ctx) }
   | ID 
      { fun ctx -> ([($1.i, $1.v, Size, IZero)], extend_i_var $1.v Size ctx) }

ESortBAnn :
    ID DBLCOLON Sort
      { fun ctx -> ([($1.i, $1.v, $3)], extend_i_var $1.v $3 ctx) }
     
FQuantifierBList :
    FSortBAnn
      { $1 }
  | FSortBAnn SEMI FQuantifierBList
      { fun ctx -> let (iv, ctx')  = $1 ctx  in
                   let (qf, ctx_qf) = $3 ctx' in
                   (iv @ qf, ctx_qf)
      }

EQuantifierBList :
    ESortBAnn
      { $1 }
  | ESortBAnn SEMI EQuantifierBList
      { fun ctx -> let (iv, ctx')  = $1 ctx  in
                   let (qf, ctx_qf) = $3 ctx' in
                   (iv @ qf, ctx_qf)
      }

QuantifiedBTypes :
   FORALL FQuantifierBList DOT BType
          { fun ctx -> let (qf, ctx') =  $2 ctx in
                        qf_to_forall_btype qf ($4 ctx')  }
  | EXISTS EQuantifierBList DOT BType
    { fun ctx ->  let (qf, ctx') =  $2 ctx in qf_to_exists_btype qf ($4 ctx') }

UnrelatedTypes :
   UNREL LPAREN type COMMA type RPAREN 
    { fun ctx -> BTyUnrel ($3 ctx, $5 ctx) }
  | UNREL type
    { fun ctx -> BTyUnrel ($2 ctx, $2 ctx) }

ConstrainedBTypes :
    LBRACE Constr RBRACE BType
    { fun ctx -> BTyCs ($2 ctx, $4 ctx) }      

ConstrainedImpBTypes :
    LBRACE Constr RBRACE DBLARROW BType
    { fun ctx -> BTyCsImp ($2 ctx, $5 ctx) }     


/* Index terms */
 ITerm:
   ITerm ADD ITerm
    { fun ctx -> IAdd($1 ctx, $3 ctx) }

  | ITerm SUB ITerm
    { fun ctx -> IMinus($1 ctx, $3 ctx) }

  | LPAREN ITerm RPAREN
    { fun ctx -> $2 ctx }
  | ID
    { fun ctx -> let n_ivar = {v_name = $1.v; v_type = BiIVar;} in
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
