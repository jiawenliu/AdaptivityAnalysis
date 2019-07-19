    open IndexSyntax
    open Fresh_var
    open Constr
    open Ctx
    open Syntax
    open Support.FileInfo
    open Core
    open Map

module Map = Map.Make(String)


let dfail = CFalse

let dmap_cs d1 d2 =
  let rec helper d1 d2 =
          match d1,d2 with
            | (id1, depth1)::tl1, (id2, depth2)::tl2
                  -> CAnd( CDLeq(depth1, depth2), (helper tl1 tl2) )
            | [],[] -> CTrue
            | _     -> dfail
    in
        helper (Map.bindings d1) (Map.bindings d2)    

let rec dmap_cs_const ctx dmp i =
    let rec helper dps =
      match dps with
        | [] -> empty_constr
        | (id, depth)::tl -> CAnd( CDLeq(depth, i), helper tl )
      in
        helper (Map.to_alist dmp)


let depth_eq q1 q2 = 
  CDEq(q1, q2)


let dmap_eq d1 d2 =
  let rec helper d1 d2 =
          match d1,d2 with
            | (id1, depth1)::tl1, (id2, depth2)::tl2
                  -> CAnd( CDEq(depth1, depth2), (helper tl1 tl2) )
            | [],[] -> CTrue
            | _     -> dfail
    in
        helper (Map.to_alist d1) (Map.to_alist d2)    



let empty_dmap = Map.empty

let rec depth_cs d1 d2 =
  CDLeq(d1, d2)

let rec get_depth d v = 
  Map.find d v

let rec merge_dmaps d1 d2 =
  Map.merge d1 d2 (fun k (dp1, dp2) -> Some (max_depths dp1 dp2)) 

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



let rec set_dmap dps v d = 
  Map.update v 
  (fun y -> 
    match y with 
      | Some y -> Some d
      | None   -> None
  ) 
  dps


(* Add an adaptivity value into each binding in the depth map and return a new depth map *)  
let rec sum_adap_dmap z dps =
  Map.fold (
    fun key d dps -> 
    let dps' = Map.add key (sum_adap_depth z d) dps
    in dps'
      ) dps Map.empty


(* Add a depth value to each binding in the depth map and return a new depth map *)  
let rec sum_depth_dmap q dps =
  Map.fold (fun key d dps -> 
    let dps' = Map.add key (add_depths d q) dps
    in dps') dps Map.empty


(* Add the adaptivity into Depth and return new Depth *)  
let rec sum_adap_depth z d = 
  match z with
    | IConst z            -> add_depths d (DConst z)
    | IVar z              -> add_depths d (DVar z)
    | IAdd(z1, z2)        -> DAdd((sum_adap_depth z1 d), (sum_adap_depth z2 (DConst 0)))
    | ISub(z1, z2)        -> add_depths d 
                             DSub( (sum_adap_depth z1 (DConst 0) ), (sum_adap_depth z2 (DConst 0) ))
    | IMaximal(z1, z2)    -> DMaximal ((sum_adap_depth z1 d), (sum_adap_depth z2 d))

  

(* Add the value of depth into adaptivity and return new Adaptivity *)  
let rec add_adap_depth z d =
  match d with
    | DBot              -> z
    | DInfy             -> z
    | DConst d          -> add_adapts z (IConst d)
    | DVar d            -> add_adapts z (IVar d)
    | DSub(d1,d2)       -> add_adapts z
                           ISub((add_adap_depth (IConst 0) d1), (add_adap_depth (IConst 0) d2))
    | DAdd(d1, d2)      -> IAdd ((add_adap_depth z d1), (add_adap_depth (IConst 0) d2))
    | DMaximal(d1,d2)   -> IMaximal ((add_adap_depth z d1), (add_adap_depth z d2))


(* Convert a List of Depth Binding from type annotation into Map *)  
let rec to_dmap dps = 
  
  let rec helper dps dm =
    match dps with
      | [] -> dm
      | (v, d) :: tl -> 
        let dm' = Map.add v d dm in
          helper tl dm'
  in
    helper dps Map.empty
