    open IndexSyntax
    open Fresh_var
    open Constr
    open Ctx
    open Syntax
    open Support.FileInfo
    open Core

module Map = Map.Make(var_info)


let dmap_cs ctx d1 d2 =
    if Map.cardinal d1 = Map.cardinal d2
      then     
          match d1,d2 with
            | (id1, depth1)::tl, (id2, depth2)::tl2
                  -> CAnd( CLeq(iterm_simpl depth1, iterm_simpl depth2), depth_cs ctx tl1 tl2)
            | [],[] -> CTrue
            | _ -> fail
      else
        fail



let rec dmap_cs_const ctx dmp i =
    let rec helper dps =
      match dps with
        | [] -> empty_constr
        | (id, depth)::tl -> CDAnd( CDLeq(iterm_simpl depth, i), helper tl )
      in
        helper (Map.bindings dmp)


let empty_dmap = Map.empty

let rec depth_cs d1 d2 =
  CDLeq(d1, d2)

let rec get_depth d v = 
  Map.find d v

let rec merge_dmaps d1 d2 =
  Map.union (fun key dp1 dp2 -> Some (max_depths dp1 dp2)) d1 d2

let rec max_dmaps d1 d2 =
  Map.union (fun key dp1 dp2 -> Some (max_depths dp1 dp2)) d1 d2

let rec bot_dmap ctx =
  let rec helper var_ctx dm =
    match var_ctx with
      | [] -> dm
      | (v, ty) :: tl -> 
      let dm' = Map.add v DBot dm in
        helper tl dm'
      in
        helper ctx.var_ctx Map.empty



let rec set_dmap ds v d = 
  Map.update v 
  (fun y -> 
    match y with 
      | Some y -> Some d
      | None   -> None
  ) 
  ds


let rec sum_adap_dmap z d =
  ()

let rec sum_cons_dmap c d =
  ()

let rec sum_adap_depth q d =
  ()


let rec to_dmap dps = 
  
  let rec helper dps dm =
    match dps with
      | [] -> dm
      | (v, d) :: tl -> 
        let dm' = Map.add v d dm in
          helper tl dm'
  in
    helper dps Map.empty
