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

open Format
open Options

module Z = Numbers.Z
module Q = Numbers.Q

module Ex = Explanation

type borne = 
  | Strict of (Q.t * Ex.t) 
  | Large of (Q.t * Ex.t) 
  | Pinfty | Minfty

let compare_bornes b1 b2 =
  match b1, b2 with
    | Minfty, Minfty | Pinfty, Pinfty -> 0
    | Minfty, _ | _, Pinfty -> -1
    | Pinfty, _ | _, Minfty -> 1
    | Strict (v1, _), Strict (v2, _) | Large (v1, _), Large (v2, _) 
    | Strict (v1, _), Large (v2, _) | Large (v1, _), Strict (v2, _) -> 
      Q.compare v1 v2

let compare_bu_bl b1 b2 =
  match b1, b2 with
    | (Minfty | Pinfty), _ | _,(Minfty | Pinfty)
    | Large _, Large _ -> 
      compare_bornes b1 b2
    | Strict _, Strict _ ->
      let c = compare_bornes b1 b2 in
      if c = 0 then -1 else c
    | Strict (v1, _), Large (v2, _) | Large (v1, _), Strict (v2, _) ->
      let c = Q.compare v1 v2 in
      if c = 0 then -1 else c
        
let compare_bl_bu b1 b2 =
  match b1, b2 with
    | (Minfty | Pinfty), _ | _,(Minfty | Pinfty)
    | Large _, Large _ -> 
      compare_bornes b1 b2
    | Strict _, Strict _ ->
      let c = compare_bornes b1 b2 in
      if c = 0 then 1 else c
    | Strict (v1, _), Large (v2, _) | Large (v1, _), Strict (v2, _) ->
      let c = Q.compare v1 v2 in
      if c = 0 then 1 else c

let compare_bl_bl b1 b2 = 
  match b1, b2 with 
    | (Minfty | Pinfty), _ | _,(Minfty | Pinfty) 
    | Strict _, Strict _ | Large _, Large _ -> 
      compare_bornes b1 b2 
    | Strict (v1, _), Large (v2, _) ->
      let c = Q.compare v1 v2 in
      if c = 0 then 1 else c
    | Large (v1, _), Strict (v2, _) ->
      let c = Q.compare v1 v2 in
      if c = 0 then -1 else c

let compare_bu_bu b1 b2 =
  match b1, b2 with
    | (Minfty | Pinfty), _ | _,(Minfty | Pinfty) 
    | Strict _, Strict _ | Large _, Large _ -> 
      compare_bornes b1 b2 
    | Strict (v1, _), Large (v2, _) ->
      let c = Q.compare v1 v2 in
      if c = 0 then -1 else c
    | Large (v1, _), Strict (v2, _) ->
      let c = Q.compare v1 v2 in
      if c = 0 then 1 else c

type t = { 
  ints : (borne * borne) list;
  is_int : bool;
  expl: Ex.t
}

exception EmptyInterval of Ex.t
exception NotConsistent of Ex.t

(*BISECT-IGNORE-BEGIN*)
module Debug = struct
  let print_borne pretty fmt = function
    | Minfty -> fprintf fmt "%s" (if pretty then "-∞" else "-inf")
    | Pinfty -> fprintf fmt "%s" (if pretty then "+∞" else "+inf")
    | Strict (v, e) | Large (v, e) ->
      if verbose () || proof () then 
        fprintf fmt "%s %a" (Q.string_of v) Ex.print e
      else fprintf fmt "%s" (Q.string_of v)
        
  let print_interval pretty fmt (b1,b2) =
    let c1, c2 = match b1, b2 with
      | Large _, Large _ -> '[', ']'
      | Large _, _ -> '[', '['
      | _, Large _ -> ']', ']'
      | _, _ -> ']', '['
    in 	    
    fprintf fmt "%c%a;%a%c" 
      c1 (print_borne pretty) b1 (print_borne pretty) b2 c2

  let rec print_list pretty fmt = function
    | [] -> ()
    | [t] -> print_interval pretty fmt t
    | t::l -> fprintf fmt "%a%s%a" 
      (print_interval pretty) t 
      (if pretty then " ∪ " else " ") (print_list pretty) l

  let pretty_print fmt {ints = ints; is_int = b; expl = e } = 
    print_list true fmt ints;
    if verbose () || proof () then fprintf fmt " %a" Ex.print e
      
  let print fmt {ints = ints; is_int = b; expl = e } = 
    print_list false fmt ints;
    if verbose () || proof () then fprintf fmt " %a" Ex.print e

end
(*BISECT-IGNORE-END*)

let print = Debug.print
let pretty_print = Debug.pretty_print

let undefined ty = {
  ints = [Minfty, Pinfty];
  is_int =  ty  = Ty.Tint;
  expl = Ex.empty
}

let point b ty e = {
  ints = [Large (b, e), Large (b, e)]; 
  is_int = ty  = Ty.Tint;
  expl = Ex.empty
}

let explain_borne = function
  | Large (_, e) | Strict (_, e) -> e
  | _ -> Ex.empty

let add_expl_to_borne b e =
  match b with
    | Large (n, e') -> Large (n, Ex.union e e')
    | Strict (n, e') -> Strict (n, Ex.union e e')
    | Pinfty | Minfty -> b

let borne_of k e n = if k then Large (n, e) else Strict (n, e)

let is_point { ints = l; expl = e } =
  match l with
    | [Large (v1, e1) , Large (v2, e2)] when Q.equal v1 v2 ->
      Some (v1, Ex.union e2 (Ex.union e1 e))
    | _ -> None

let add_expl_zero i expl =
  let res = List.map (fun x -> 
    match x with
      | (Large (c1, e1) , Large (c2, e2)) when Q.sign c1 = 0 && Q.sign c2 = 0->
        (Large (Q.zero, Ex.union e1 expl),
         Large (Q.zero, Ex.union e2 expl))
      | _ -> x) i.ints in
  { i with ints = res }

let check_one_interval b1 b2 is_int =
  match b1, b2 with
    | Pinfty, _ | _, Minfty  -> raise (EmptyInterval Ex.empty)
    | (Strict (v1, e1) | Large (v1,e1)), 
      (Strict (v2, e2) | Large (v2, e2)) ->
      let c = Q.compare v1 v2 in 
      if c > 0 then raise 
	(EmptyInterval (Ex.union e2 e1));
      if c = 0 then begin
	match b1, b2 with
	  | Large _, Large _ when not is_int || Q.is_integer v1 ->
	    ()
	  | _ -> raise (EmptyInterval (Ex.union e2 e1))
      end
    | _ -> ()

let min_borne b1 b2 = 
  match b1, b2 with
    | Minfty , _ | _ , Minfty -> Minfty
    | b , Pinfty | Pinfty, b -> b
    | (Strict (v1, _) | Large (v1, _)) , (Strict (v2, _) | Large (v2, _)) ->
      let c = Q.compare v1 v2 in
      if c < 0 then b1
      else if c > 0 then b2
      else match b1, b2 with 
	| (Strict _ as b) , _ | _, (Strict _ as b) -> b
	| _, _ -> b1
          
let max_borne b1 b2 = 
  match b1, b2 with
    | Pinfty , _ | _ , Pinfty -> Pinfty
    | b , Minfty | Minfty, b -> b
    | (Strict (v1, _) | Large (v1, _)) , (Strict (v2, _) | Large (v2, _)) -> 
      let c = Q.compare v1 v2 in
      if c > 0 then b1
      else if c < 0 then b2
      else match b1, b2 with 
	| (Strict _ as b) , _ | _, (Strict _ as b) -> b
	| _, _ -> b1
	  
let pos_borne b1 =
  compare_bornes b1 (borne_of true Ex.empty Q.zero) >= 0
let pos_borne_strict b1 = 
  compare_bornes b1 (borne_of true Ex.empty Q.zero) > 0
let neg_borne b1 = 
  compare_bornes b1 (borne_of true Ex.empty Q.zero) <= 0
let neg_borne_strict b1 = 
  compare_bornes b1 (borne_of true Ex.empty Q.zero) < 0
let zero_borne b1 = 
  compare_bornes b1 (borne_of true Ex.empty Q.zero) = 0

exception Found of Sig.answer

let doesnt_contain_0 {ints=l} =
  try
    let max = List.fold_left
      (fun old_u (l, u) -> 
	if neg_borne l && pos_borne u then raise (Found Sig.No);
	if neg_borne_strict old_u && pos_borne_strict l then 
	  raise (Found 
		   (Sig.Yes 
		      (Ex.union 
			 (explain_borne old_u) (explain_borne l), [])));
	u) Minfty l in
    if neg_borne_strict max then Sig.Yes (explain_borne max, [])
    else Sig.No
  with Found ans -> ans

let is_strict_smaller i1 i2 =
  match i1, i2 with
    | _, [] -> false
    | [], _ -> true
    | _ ->
      try
	List.iter2 (fun (l1, u1) (l2, u2) ->
	  if compare_bl_bl l1 l2 > 0 || compare_bu_bu u1 u2 < 0
	  then raise Exit
	) i1 i2;
	false
      with 
	| Exit -> true
	| Invalid_argument _ -> true

let is_strict_smaller {ints=i1} {ints=i2} = 
  is_strict_smaller i1 i2


let rec union_bornes l =
  match l with
    | [] | [_] -> l
    | (l1, u1)::((l2, u2)::r as r2) ->
      if compare_bu_bl u1 l2 < 0 then
	(l1, u1)::(union_bornes r2)
      else if compare_bu_bu u1 u2 > 0 then
	union_bornes ((l1, u1)::r)
      else
	union_bornes ((l1, u2)::r)

let union ({ints = l} as uints) =
  let l = List.sort (fun (l1, _) (l2, _) -> compare_bornes l1 l2) l in
  { uints with ints = union_bornes l }

let add_borne b1 b2 =
  match b1,b2 with
    | Minfty, Pinfty | Pinfty, Minfty -> assert false
    | Minfty, _ | _, Minfty -> Minfty
    | Pinfty, _ | _, Pinfty -> Pinfty
    | Large (v1, e1), Large (v2, e2) -> 
      Large (Q.add v1 v2, Ex.union e1 e2)
    | (Large (v1, e1) | Strict (v1, e1)), (Large (v2, e2) | Strict (v2, e2)) ->
      Strict (Q.add v1 v2, Ex.union e1 e2)

let add_interval l (b1,b2) =
  List.fold_right
    (fun (b1', b2') l ->
      let l1 = ((add_borne b1 b1'),(add_borne b2 b2'))::l in
      union_bornes (l1)
    ) l []

let add {ints = l1; is_int = is_int; expl = e1} {ints = l2; expl = e2}=
  let l = 
    List.fold_left
      (fun l bs -> let i = add_interval l1 bs in i@l) [] l2 
  in
  union { ints = l ; is_int = is_int; expl = Ex.union e1 e2 }

let minus_borne = function
  | Minfty -> Pinfty
  | Pinfty -> Minfty
  | Large (v, e) -> Large (Q.minus v, e)
  | Strict (v, e) -> Strict (Q.minus v, e)

let scale_borne n b =
  assert (Q.sign n >= 0);
  if Q.sign n = 0 then 
    match b with
      | Pinfty | Minfty -> Large (Q.zero, Ex.empty)
      | Large (_, e) | Strict (_, e) ->  Large (Q.zero, e)
  else match b with
    | Pinfty | Minfty -> b
    | Large (v, e) -> Large (Q.mult n v, e)
    | Strict (v, e) -> Strict (Q.mult n v, e)

let scale_interval n (b1,b2) =
  if Q.sign n < 0 then
    (minus_borne (scale_borne (Q.minus n) b2),
     minus_borne (scale_borne (Q.minus n) b1))
  else (scale_borne n b1, scale_borne n b2)


let scale n uints =
  Options.tool_req 4 "TR-Arith-Axiomes scale";
  let l = List.map (scale_interval n) uints.ints in
  union { uints with ints = l; expl = uints.expl }
    
let mult_borne b1 b2 =
  match b1,b2 with
    | Minfty, Pinfty | Pinfty, Minfty -> assert false
    | Minfty, b | b, Minfty ->
      if compare_bornes b (borne_of true Ex.empty Q.zero) = 0 
      then b
      else if pos_borne b then Minfty
      else Pinfty
    | Pinfty, b | b, Pinfty ->
      if compare_bornes b (borne_of true Ex.empty Q.zero) = 0 
      then b
      else if pos_borne b then Pinfty
      else Minfty
    | Strict (v1, e1), Strict (v2, e2) | Strict (v1, e1), Large (v2, e2)
    | Large (v1, e1), Strict (v2, e2) -> 
      Strict (Q.mult v1 v2, Ex.union e1 e2)
    | Large (v1, e1), Large (v2, e2) -> 
      Large (Q.mult v1 v2, Ex.union e1 e2)

let mult_borne_inf b1 b2 =
  match b1,b2 with
    | Minfty, Pinfty | Pinfty, Minfty -> Minfty
    | _, _ -> mult_borne b1 b2

let mult_borne_sup b1 b2 =
  match b1,b2 with
    | Minfty, Pinfty | Pinfty, Minfty -> Pinfty
    | _, _ -> mult_borne b1 b2

type interval_class = 
  | P of Ex.t 
  | M of Ex.t 
  | N of Ex.t 
  | Z

let class_of (l,u) =
  if zero_borne l && zero_borne u then Z
  else if pos_borne l && pos_borne u then P (explain_borne l)
  else if neg_borne l && neg_borne u then N (explain_borne u)
  else M (Ex.union (explain_borne l) (explain_borne u))

let mult_bornes (a,b) (c,d) =
  (* see util/intervals_mult.png *)
  match class_of (a,b), class_of (c,d) with
    | P e1, P e2 -> 
      mult_borne_inf a c, mult_borne_sup b d, Ex.union e1 e2
    | P e1, M e2 -> 
      mult_borne_inf b c, mult_borne_sup b d, Ex.union e1 e2
    | P e1, N e2 -> 
      mult_borne_inf b c, mult_borne_sup a d, Ex.union e1 e2
    | M e1, P e2 -> 
      mult_borne_inf a d, mult_borne_sup b d, Ex.union e1 e2
    | M e1, M e2 -> 
      min_borne (mult_borne_inf a d) (mult_borne_inf b c),
      max_borne (mult_borne_sup a c) (mult_borne_sup b d), 
      Ex.union e1 e2
    | M e1, N e2 ->
      mult_borne_inf b c, mult_borne_sup a c, Ex.union e1 e2
    | N e1, P e2 ->
      mult_borne_inf a d, mult_borne_sup b c, Ex.union e1 e2
    | N e1, M e2 ->
      mult_borne_inf a d, mult_borne_sup a c, Ex.union e1 e2
    | N e1, N e2 ->
      mult_borne_inf b d, mult_borne_sup a c, Ex.union e1 e2
    | Z, (P _ | M _ | N _ | Z) -> (a, b, Ex.empty)
    | (P _ | M _ | N _ ), Z -> (c, d, Ex.empty)
      
let rec power_borne_inf p b =
  match p with
    | 1 -> b
    | p -> mult_borne_inf b (power_borne_inf (p-1) b)

let rec power_borne_sup p b =
  match p with
    | 1 -> b
    | p -> mult_borne_sup b (power_borne_sup (p-1) b)

let max_merge b1 b2 =
  let ex = Ex.union (explain_borne b1) (explain_borne b2) in
  let max = max_borne b1 b2 in
  match max with
    | Minfty | Pinfty -> max
    | Large (v, e) -> Large (v, ex)
    | Strict (v, e) -> Strict (v, ex)

let power_bornes p (b1,b2) =
  if neg_borne b1 && pos_borne b2 then
    match p with
      | 0 -> assert false
      | p when p mod 2 = 0 ->
	(* max_merge to have explanations !!! *)
	let m = max_merge (power_borne_sup p b1) (power_borne_sup p b2) in
	(Large (Q.zero, Ex.empty), m)
      | _ -> (power_borne_inf p b1, power_borne_sup p b2)
  else if pos_borne b1 && pos_borne b2 then
    (power_borne_inf p b1, power_borne_sup p b2)
  else if neg_borne b1 && neg_borne b2 then
    match p with
      | 0 -> assert false
      | p when p mod 2 = 0 -> (power_borne_inf p b2, power_borne_sup p b1)
      | _ -> (power_borne_inf p b1, power_borne_sup p b2)
  else assert false
    
let int_of_borne_inf b =
  match b with
    | Minfty | Pinfty -> b
    | Large (v, e) -> Large (Q.ceiling v, e)
    | Strict (v, e) ->
      let v' = Q.ceiling v in
      if Q.compare v' v > 0 then Large (v', e) else Large (Q.add v Q.one, e) 

let int_of_borne_sup b =
  match b with
    | Minfty | Pinfty -> b
    | Large (v, e) -> Large (Q.floor v, e)
    | Strict (v, e) ->
      let v' = Q.floor v in
      if Q.compare v' v < 0 then Large (v', e) else Large (Q.sub v Q.one, e) 

let int_div_of_borne_inf b =
  match b with
    | Minfty | Pinfty -> b
    | Large (v, e) -> Large (Q.floor v, e)
    | Strict (v, e) ->
      let v' = Q.floor v in
      if Q.compare v' v > 0 then Large (v', e) else Large (Q.add v Q.one, e) 

let int_div_of_borne_sup b =
  match b with
    | Minfty | Pinfty -> b
    | Large (v, e) -> Large (Q.floor v, e)
    | Strict (v, e) ->
      let v' = Q.floor v in
      if Q.compare v' v < 0 then Large (v', e) else Large (Q.sub v Q.one, e)

let int_bornes l u = 
  int_of_borne_inf l, int_of_borne_sup u

let int_div_bornes l u = 
  int_div_of_borne_inf l, int_div_of_borne_sup u


let intersect ({ints=l1; expl=e1; is_int=is_int} as uints1)
    {ints=l2; expl=e2} =
  let rec step (l1,l2) acc expl =
    match l1, l2 with
      | (lo1,up1)::r1, (lo2,up2)::r2 ->
	let (lo1,up1), (lo2,up2) = 
	  if is_int then (int_bornes lo1 up1), (int_bornes lo2 up2)
	  else (lo1,up1), (lo2,up2) in
	let cll = compare_bl_bl lo1 lo2 in
	let cuu = compare_bu_bu up1 up2 in
	let clu = compare_bl_bu lo1 up2 in
	let cul = compare_bu_bl up1 lo2 in
	if cul < 0 then
	  let nexpl  = Ex.union (explain_borne up1) (explain_borne lo2) in
	  match r1 with
	    | [] -> step (r1, l2) acc (Ex.union nexpl expl)
	    | (lor1,upr1)::rr1 ->
	      let lor1 = add_expl_to_borne lor1 nexpl in
	      let r1 = (lor1,upr1)::rr1 in
	      step (r1, l2) acc expl
	else if clu > 0 then 
	  let nexpl  = Ex.union (explain_borne up2) (explain_borne lo1) in
	  match r2 with
	    | [] -> step (l1, r2) acc (Ex.union nexpl expl)
	    | (lor2,upr2)::rr2 ->
	      let lor2 = add_expl_to_borne lor2 nexpl in
	      let r2 = (lor2,upr2)::rr2 in
	      step (l1, r2) acc expl
	(* incorrect *)
	(* else if cll = 0 && cuu = 0 then  *)
	(*   step (r1, r2) ((lo1,up1)::acc) expl *)
	else if cll <= 0 && cuu >= 0 then 
	  step (l1, r2) ((lo2,up2)::acc) expl
	else if cll >= 0 && cuu <= 0 then 
	  step (r1, l2) ((lo1,up1)::acc) expl
	else if cll <= 0 && cuu <= 0 && cul >= 0 then 
	  step (r1, l2) ((lo2,up1)::acc) expl
	else if cll >= 0 && cuu >= 0 && clu <= 0 then 
	  step (l1, r2) ((lo1,up2)::acc) expl
	else assert false
            | [], _ | _, [] ->  List.rev acc, expl
  in
  let l, expl = step (l1,l2) [] (Ex.union e1 e2) in
  if l = [] then raise (NotConsistent expl)
  else { uints1 with ints = l; expl = expl }


let new_borne_sup expl b ~is_le uints =
  intersect 
    { ints = [Minfty, (borne_of is_le expl b)];
      is_int = uints.is_int;
      expl = Ex.empty } uints

let new_borne_inf expl b ~is_le uints =
  intersect 
    { ints = [(borne_of is_le expl b), Pinfty];
      is_int = uints.is_int;
      expl = Ex.empty } uints

let complement ({ints=l; expl=e} as uints) =
  let rec step l prev acc =
    match l with
      | (b1,b2)::r ->
	let bu = match b1 with
	  | Strict v -> Large v
	  | Large v -> Strict v
	  | _ -> b1 in
	let bl = match b2 with
	  | Strict v -> Large v
	  | Large v -> Strict v
	  | _ -> b2 in
	if bu = Minfty then step r bl acc
	else step r bl ((prev, bu)::acc)
      | [] -> 
	if prev = Pinfty then List.rev acc
	else List.rev ((prev, Pinfty)::acc)
  in
  { uints with ints = step l Minfty [] }
    

let exclude uints1 uints2 =
  intersect (complement uints1) uints2 

let mult u1 u2 =
  Options.tool_req 4 "TR-Arith-Axiomes mult";
  let resl, expl = 
    List.fold_left
      (fun (l', expl) b1 ->
	List.fold_left 
	  (fun (l, ex) b2 ->
	    let bl, bu, ex' = mult_bornes b1 b2 in
	    (bl, bu)::l, Ex.union ex ex') (l', expl) u2.ints)
      ([], Ex.empty) u1.ints
  in
  union { ints=resl; is_int = u1.is_int;
	  expl = Ex.union expl 
      (Ex.union u1.expl u2.expl) }

let power n u =
  Options.tool_req 4 "TR-Arith-Axiomes power";
  let l = List.map (power_bornes n) u.ints in
  union { u with ints = l }

let root_num a n = 
  if Q.sign a < 0 then assert false
  else if Q.sign a = 0 then Q.zero
  else
    let v = Q.float_of a in
    let w = if v < min_float then min_float
      else if v > max_float then max_float
      else v
    in
    if n = 2 then Q.of_float (sqrt w)
    else Q.of_float (w ** (1./. (float n)))

let root_default_num a n =
  let s = root_num a n in
  let d = Q.sub a (Q.power s n) in
  if Q.sign d >= 0 then s else Q.div a (Q.power s (n - 1))

let root_exces_num a n =
  let s = root_num a n in
  let d = Q.sub a (Q.power s n) in
  if Q.sign d <= 0 then s else Q.div a (Q.power s (n - 1))

let root_default_borne is_int x n =
  match x with
    | Pinfty -> Pinfty
    | Minfty -> Minfty
    | Large (v, e) | Strict (v, e) ->
      let s = if Q.sign v >= 0 then root_default_num v n
	else (Q.minus (root_exces_num (Q.minus v) n)) in
      if is_int then
	let cs = Q.ceiling s in
	let cs2 = Q.power cs n in
	if Q.compare v cs2 <= 0 then Large (cs, e)
	else Large (Q.add cs Q.one, e)
      else Large (s, e)

let root_exces_borne is_int x n =
  match x with
    | Pinfty -> Pinfty
    | Minfty -> Minfty
    | Large (v, e) | Strict (v, e) ->
      let s = if Q.sign v >= 0 then root_exces_num v n
	else (Q.minus (root_default_num (Q.minus v) n)) in
      if is_int then
	let cs = Q.floor s in
	let cs2 = Q.power cs n in
	if Q.compare v cs2 >= 0 then Large (cs, e)
	else Large (Q.sub cs Q.one, e)
      else Large (s, e)

let sqrt_interval is_int (b1,b2) =
  let l1, u1 = (minus_borne (root_exces_borne is_int b2 2),
		minus_borne (root_default_borne is_int b1 2)) in
  let l2, u2 = (root_default_borne is_int b1 2,
		root_exces_borne is_int b2 2) in
  if compare_bornes l1 u1 > 0 then
    if compare_bornes l2 u2 > 0 then []
    else [l2,u2]
  else if compare_bornes l2 u2 > 0 then [l1, u1]
  else  union_bornes [(l1,u1); (l2, u2)]

let root_interval is_int (b1,b2) n =
  let u,l = (root_default_borne is_int b1 n, root_exces_borne is_int b2 n) in
  if compare_bornes u l > 0 then [] else [u,l]

let sqrt {ints = l; is_int = is_int; expl = e } =
  Options.tool_req 4 "TR-Arith-Axiomes sqrt";
  let l =
    List.fold_left
      (fun l' bs ->
	(sqrt_interval is_int bs)@l'
      ) [] l in
  union { ints = l; is_int = is_int; expl = e }

let rec root n ({ints = l; is_int = is_int; expl = e} as u) =
  Options.tool_req 4"TR-Arith-Axiomes root";
  if n mod 2 = 0 then root (n/2) (sqrt u)
  else
    let l =
      List.fold_left
	(fun l' bs ->
	  (root_interval is_int bs n)@l'
	) [] l in
    union { ints = l; is_int = is_int; expl = e }
      
let finite_size {ints = l; is_int = is_int} =
  if (not is_int) then None
  else
    try
      let n =
	List.fold_left
	  (fun n (b1,b2) ->
	    match b1, b2 with
	      | Minfty, _ | _, Pinfty -> raise Exit
	      | Large (v1, _) , Large (v2, _) -> 
                Q.add n (Q.add (Q.sub v2 v1) Q.one)
	      | _, _ -> assert false
	  ) Q.zero l in
      Some n
    with Exit -> None
      
let borne_inf = function
  | {ints = (Large (v, ex), _)::_} -> v, ex
  | _ -> invalid_arg "Intervals.borne_inf : No finite lower bound"



let inv_borne_inf b is_int ~other =
  match b with
    | Pinfty -> assert false
    | Minfty ->
      if is_int then Large (Q.zero,  explain_borne other) 
      else Strict (Q.zero, explain_borne other)
    | Strict (c, e) | Large (c, e) when Q.sign c = 0 -> Pinfty
    | Strict (v, e) -> Strict (Q.div Q.one v, e)
    | Large (v, e) -> Large (Q.div Q.one v, e)

let inv_borne_sup b is_int ~other =
  match b with
    | Minfty -> assert false
    | Pinfty ->
      if is_int then Large (Q.zero, explain_borne other)
      else Strict (Q.zero, explain_borne other)
    | Strict (c, e) | Large (c, e) when Q.sign c = 0 -> Minfty
    | Strict (v, e) -> Strict (Q.div Q.one v, e)
    | Large (v, e) -> Large (Q.div Q.one v, e)

let inv_bornes (l, u) is_int =
  inv_borne_sup u is_int ~other:l, inv_borne_inf l is_int ~other:u


let inv ({ints=l; is_int=is_int} as u) =
  try
    let l' = List.fold_left 
      (fun acc (l,u) ->
	if (pos_borne_strict l && pos_borne_strict u) 
	  || (neg_borne_strict l && neg_borne_strict u) then 
	  (inv_bornes (l, u) is_int) :: acc
	else raise Exit
      ) [] l in
    union { u with ints=l' }
  with Exit -> { u with ints = [Minfty, Pinfty]  }

let div i1 i2 =
  Options.tool_req 4 "TR-Arith-Axiomes div";
  let inv_i2 = inv i2 in
  if inv_i2.ints = [Minfty, Pinfty] then inv_i2
  else
    let i1 = match doesnt_contain_0 i2 with
      | Sig.Yes (ex, _) -> add_expl_zero i1 ex
      | Sig.No -> i1
    in
    let ({ints=l; is_int=is_int} as i) = mult i1 inv_i2 in
    let l = 
      if is_int then 
	List.map (fun (l,u) -> int_div_bornes l u) l
      else l in
    { i with ints = l }
