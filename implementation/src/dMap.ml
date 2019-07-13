    open IndexSyntax
    open Fresh_var
    open Constr
    open Ctx
    open Syntax
    open Support.FileInfo
    open Core

module Map = Map.Make(var_info)

let empty_dmap = Map.empty

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



let rec dmap_cs_const ctx d1 i =
  match d1 with
    | [] -> CTrue
    | (id, depth)::tl -> CAnd( CLeq(iterm_simpl depth, i), depth_cs_const ctx tl i)


let rec depth_cs d1 d2 =
  CDLeq(d1, d2)

let rec get_depth d v = 
  ()

let rec merge_dmaps d1 d2 =

  ()

let rec max_dmaps d1 d2 =
  ()

let rec bot_dmap =
  ()

let rec empty_dmap =
  ()

let rec set_dmap d i = 
  ()


let rec sum_adap_dmap z d =
  ()

let rec sum_cons_dmap c d =
  ()

let rec sum_adap_depth q d =
  ()

let rec to_dmap dps = 
  ()
