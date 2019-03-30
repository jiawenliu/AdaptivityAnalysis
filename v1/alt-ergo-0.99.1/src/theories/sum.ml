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

open Options
open Format
open Sig
open Exception  
module Sy = Symbols
module T  = Term
module A  = Literal
module L  = List
module Hs = Hstring
module Ex = Explanation

type 'a abstract = Cons of Hs.t * Ty.t |  Alien of 'a

module type ALIEN = sig
  include Sig.X
  val embed : r abstract -> r
  val extract : r -> (r abstract) option
end

module Shostak (X : ALIEN) = struct

  type t = X.r abstract
  type r = X.r

  let name = "Sum"
    
  let is_mine_symb = function 
    | Sy.Name(_, Sy.Constructor) -> true 
    | _ -> false
      
  let fully_interpreted sb = true

  let type_info = function
    | Cons (_, ty) -> ty
    | Alien x -> X.type_info x
      
  let color _ = assert false
    

  (*BISECT-IGNORE-BEGIN*)
  module Debug = struct
      
    let print fmt = function
      | Cons (hs,ty) -> fprintf fmt "%s" (Hs.view hs)
      | Alien x -> fprintf fmt "%a" X.print x

    let solve_bis a b = 
      if debug_sum () then fprintf fmt "[Sum] we solve %a = %a@."  
        X.print a X.print b

    let solve_bis_result res =
      if debug_sum () then 
        match res with
          | [p,v] -> fprintf fmt "\twe get: %a |-> %a@." X.print p X.print v
          | []    -> fprintf fmt "\tthe equation is trivial@."
          | _ -> assert false
            
    let solve_bis_unsolvable () = 
      if debug_sum () then fprintf fmt "\tthe equation is unsolvable@."

  end
  (*BISECT-IGNORE-END*)

  let print = Debug.print

  let embed r =
    match X.extract r with
      | Some c -> c
      | None -> Alien r 

  let is_mine = function
    | Alien r -> r
    | Cons(hs,ty) as c -> X.embed c
      
  let compare_mine c1 c2 = 
    match c1 , c2 with
      | Cons (h1,ty1) , Cons (h2,ty2)  -> 
        let n = Hs.compare h1 h2 in
        if n <> 0 then n else Ty.compare ty1 ty2
      | Alien r1, Alien r2 -> X.compare r1 r2
      | Alien _ , Cons _   -> 1
      | Cons _  , Alien _  -> -1
        
  let compare x y = compare_mine (embed x) (embed y)

  let hash = function
    | Cons (h, ty) -> Hstring.hash h + 19 * Ty.hash ty
    | Alien r -> X.hash r

  let leaves _ = []

  let subst p v c = 
    let cr = is_mine c in
    if X.equal p cr then v
    else 
      match c with
        | Cons(hs,t) -> cr
        | Alien r    -> X.subst p v r
          
  let make t = match T.view t with
    | {T.f=Sy.Name(hs, Sy.Constructor); xs=[];ty=ty} ->
      is_mine (Cons(hs,ty)), []
    | _ -> assert false
      
  let solve a b = 
    match embed a, embed b with
      | Cons(c1,_) , Cons(c2,_) when Hs.equal c1 c2 -> []
      | Cons(c1,_) , Cons(c2,_) -> raise Unsolvable
      | Cons _     , Alien r2   -> [r2,a]
      | Alien r1   , Cons _     -> [r1,b]
      | Alien _    , Alien _    -> 
        if X.compare a b > 0 then [a,b] else [b,a]
          
  let solve_bis a b =
    Debug.solve_bis a b;
    try
      let res = solve a b in
      Debug.solve_bis_result res;
      res
    with Unsolvable -> 
      Debug.solve_bis_unsolvable ();
      raise Unsolvable

  let abstract_selectors v acc = is_mine v, acc

  let term_extract _ = None, false
    
  let solve r1 r2 pb = 
    {pb with sbt = List.rev_append (solve_bis r1 r2) pb.sbt}

  let solve r1 r2 pb =
    if profiling() then
      try 
	Options.exec_timer_start Timers.TSum;
	let res = solve r1 r2 pb in
	Options.exec_timer_pause Timers.TSum;
	res
      with e -> 
	Options.exec_timer_pause Timers.TSum;
	raise e
    else solve r1 r2 pb

end

module Relation (X : ALIEN) (Uf : Uf.S) = struct
  type r = X.r
  type uf = Uf.t

  module Sh = Shostak(X)
  open Sh

  exception Not_Cons

  module Ex = Explanation

  module MX = Map.Make(struct type t = X.r include X end)
  module HSS = Set.Make (struct type t=Hs.t let compare = Hs.compare end)
    
  module LR = Literal.Make(struct type t = X.r include X end)

  type t = { mx : (HSS.t * Ex.t) MX.t; classes : Term.Set.t list }

  let empty classes = { mx = MX.empty; classes = classes }

  (*BISECT-IGNORE-BEGIN*)
  module Debug = struct

    let assume bol r1 r2 =
      if debug_sum () then
        fprintf fmt "[Sum.Rel] we assume %a %s %a@." 
          X.print r1 (if bol then "=" else "<>") X.print r2

    let print_env env =
      if debug_sum () then begin
        fprintf fmt "--SUM env ---------------------------------@.";
        MX.iter
          (fun r (hss, ex) ->
            fprintf fmt "%a ::= " X.print r;
            begin
              match HSS.elements hss with
                  []      -> ()
                | hs :: l ->
                  fprintf fmt " %s" (Hs.view hs);
                  L.iter (fun hs -> fprintf fmt " | %s" (Hs.view hs)) l
            end;
            fprintf fmt " : %a@." Ex.print ex;
            
          ) env.mx;
        fprintf fmt "-------------------------------------------@.";
      end

    let case_split r r' =
      if debug_sum () then
	fprintf fmt "[case-split] %a = %a@." X.print r X.print r'
          
    let no_case_split () =
      if debug_sum () then fprintf fmt "[case-split] sum: nothing@."

    let add r =
      if debug_sum () then fprintf fmt "Sum.Rel.add: %a@." X.print r

  end
  (*BISECT-IGNORE-END*)

  let values_of r = match X.type_info r with
    | Ty.Tsum (_,l) -> 
      Some (List.fold_left (fun st hs -> HSS.add hs st) HSS.empty l)
    | _ -> None

  let add_diseq hss sm1 sm2 dep env eqs = 
    match sm1, sm2 with
      | Alien r , Cons(h,ty) 
      | Cons (h,ty), Alien r  ->
        let enum, ex = try MX.find r env.mx with Not_found -> hss, Ex.empty in
        let enum = HSS.remove h enum in
        let ex = Ex.union ex dep in
        if HSS.is_empty enum then raise (Inconsistent (ex, env.classes))
        else 
          let env = { env with mx = MX.add r (enum, ex) env.mx } in
          if HSS.cardinal enum = 1 then
            let h' = HSS.choose enum in
            env, (LSem (LR.mkv_eq r (is_mine (Cons(h',ty)))), ex)::eqs
          else env, eqs
            
      | Alien r1, Alien r2 -> 
        let enum1,ex1= try MX.find r1 env.mx with Not_found -> hss,Ex.empty in
        let enum2,ex2= try MX.find r2 env.mx with Not_found -> hss,Ex.empty in

        let eqs = 
          if HSS.cardinal enum1 = 1 then
            let ex = Ex.union dep ex1 in
            let h' = HSS.choose enum1 in
            let ty = X.type_info r1 in
            (LSem (LR.mkv_eq r1 (is_mine (Cons(h',ty)))), ex)::eqs
          else eqs
        in
        let eqs = 
          if HSS.cardinal enum2 = 1 then
            let ex = Ex.union dep ex2 in
            let h' = HSS.choose enum2 in
            let ty = X.type_info r2 in
            (LSem (LR.mkv_eq r2 (is_mine (Cons(h',ty)))), ex)::eqs
          else eqs
        in
        env, eqs

      |  _ -> env, eqs

  let add_eq hss sm1 sm2 dep env eqs = 
    match sm1, sm2 with
      | Alien r, Cons(h,ty) | Cons (h,ty), Alien r  ->
        let enum, ex = try MX.find r env.mx with Not_found -> hss, Ex.empty in
        let ex = Ex.union ex dep in
        if not (HSS.mem h enum) then raise (Inconsistent (ex, env.classes));
	{env with mx = MX.add r (HSS.singleton h, ex) env.mx} , eqs
          
      | Alien r1, Alien r2   -> 
        let enum1,ex1 = 
          try MX.find r1 env.mx with Not_found -> hss, Ex.empty in
        let enum2,ex2 = 
          try MX.find r2 env.mx with Not_found -> hss, Ex.empty in
        let ex = Ex.union dep (Ex.union ex1 ex2) in
        let diff = HSS.inter enum1 enum2 in 
        if HSS.is_empty diff then raise (Inconsistent (ex, env.classes));
        let mx = MX.add r1 (diff, ex) env.mx in
        let env = {env with mx = MX.add r2 (diff, ex) mx } in
        if HSS.cardinal diff = 1 then
          let h' = HSS.choose diff in
          let ty = X.type_info r1 in
          env, (LSem (LR.mkv_eq r1 (is_mine (Cons(h',ty)))), ex)::eqs
        else env, eqs

      |  _ -> env, eqs

  let assume env uf la =
    let classes = Uf.cl_extract uf in
    let env = { env with classes = classes } in
    let aux bol r1 r2 dep env eqs = function
      | None     -> env, eqs
      | Some hss -> 
        Debug.assume bol r1 r2;
        if bol then 
          add_eq hss (embed r1) (embed r2) dep env eqs
        else
          add_diseq hss (embed r1) (embed r2) dep env eqs
    in
    Debug.print_env env;
    let env, eqs = 
      List.fold_left
        (fun (env,eqs) -> function
          | A.Eq(r1,r2), _, ex -> 
	    aux true  r1 r2 ex env eqs (values_of r1)
	      
          | A.Distinct(false, [r1;r2]), _, ex -> 
	    aux false r1 r2 ex env eqs (values_of r1)
	      
          | _ -> env, eqs
	    
        ) (env,[]) la
    in
    env, { assume = eqs; remove = [] }

  (* XXXXXX : TODO -> ajouter les explications dans les choix du
     case split *)

  let case_split env = 
    let acc = MX.fold
      (fun r (hss, ex) acc ->
        let sz = HSS.cardinal hss in
        if sz = 1 then acc
        else match acc with
	  | Some (n,r,hs) when n <= sz -> acc
	  | _ -> Some (sz, r, HSS.choose hss)
      ) env.mx None 
    in
    match acc with 
      | Some (n,r,hs) -> 
	let r' = is_mine (Cons(hs,X.type_info r)) in
        Debug.case_split r r';
	[LR.mkv_eq r r', Ex.empty, Numbers.Q.of_int n]
      | None -> 
        Debug.no_case_split ();
	[]
          

  let query env uf a_ex =
    try ignore(assume env uf [a_ex]); Sig.No
    with Inconsistent (expl, classes) -> Sig.Yes (expl, classes)

  let add env r =
    Debug.add r;
    match embed r, values_of r with
      | Alien r, Some hss -> 
        if MX.mem r env.mx then env else 
          { env with mx = MX.add r (hss, Ex.empty) env.mx }
      | _ -> env


  let assume env uf la =
    if profiling() then
      try 
	Options.exec_timer_start Timers.TSum;
	let res =assume env uf la in
	Options.exec_timer_pause Timers.TSum;
	res
      with e -> 
	Options.exec_timer_pause Timers.TSum;
	raise e
    else assume env uf la

  let query env uf la =
    if profiling() then
      try 
	Options.exec_timer_start Timers.TSum;
	let res = query env uf la  in
	Options.exec_timer_pause Timers.TSum;
	res
      with e -> 
	Options.exec_timer_pause Timers.TSum;
	raise e
    else query env uf la 


  let instantiate env _ _ _ _ = env, []

  let print_model _ _ _ = ()

  let new_terms env = Term.Set.empty

end
