    open IndexSyntax
    open Fresh_var
    open Constr
    open Ctx
    open Syntax
    open Support.FileInfo
    
    

module StdMap = Map.Make(String)

let empty_dmap = StdMap.empty

(* The Depth Map of variables in programs *)
type dmap = IndexSyntax.dterm StdMap.t

let dfail = CFalse

let dmap_cs d1 d2 =
  let rec helper d1 d2 =
          match d1,d2 with
            | (id1, depth1)::tl1, (id2, depth2)::tl2
                  -> 
                  if depth1 = DBot && depth2 = DBot
                  then
                    helper tl1 tl2
                    else
                      CAnd( CDLeq(depth1, depth2), (helper tl1 tl2) )
            | [],[] -> CTrue
            | _     -> dfail
    in
        helper (StdMap.bindings d1) (StdMap.bindings d2)    

let rec dmap_cs_const ctx dmp i =
    let rec helper dps =
      match dps with
        | [] -> empty_constr
        | (id, depth)::tl -> CAnd( CDLeq(depth, i), helper tl )
      in
        helper (StdMap.bindings dmp)

let dmap_length dmp =
  StdMap.cardinal dmp

let depth_eq q1 q2 = 
  CDEq(q1, q2)


let dmap_rm dmp k =
  StdMap.remove k dmp

let dmap_eq d1 d2 =
  let rec helper d1 d2 =
          match d1,d2 with
            | (id1, depth1)::tl1, (id2, depth2)::tl2
                  -> CAnd( CDEq(depth1, depth2), (helper tl1 tl2) )
            | [],[] -> CTrue
            | _     -> dfail
    in
        helper (StdMap.bindings d1) (StdMap.bindings d2)    


let rec adap_eq z1 z2 =
  CEq(iterm_simpl z1, iterm_simpl z2)



let rec depth_cs d1 d2 =
  CDLeq(d1, d2)

let rec get_depth d v = 
  StdMap.find_opt v d


let rec set_dmap dps v d = 
  StdMap.update v
  (fun y -> 
    match y with 
      | Some y -> Some d
      | None   -> None
  ) 
  dps



let rec merge_dmaps d1 d2 =
  StdMap.union (fun key dp1 dp2 -> Some (max_depths dp1 dp2)) d1 d2

let rec max_dmaps d1 d2 =
  StdMap.union (fun key dp1 dp2 -> Some (max_depths dp1 dp2)) d1 d2

let rec bot_dmap ctx =
  let rec helper var_ctx dm =
    match var_ctx with
      | [] -> dm
      | (v, ty) :: tl -> 
      let dm' = StdMap.add v.v_name DBot dm in
        helper tl dm'
      in
        helper ctx.var_ctx StdMap.empty




(* Add the adaptivity into Depth and return new Depth *)  
let rec sum_adap_depth z d = 
  match z with
    | IConst z            -> add_depths d (DConst z)
    | IVar z              -> add_depths d (DVar z)
    | IAdd(z1, z2)        -> DAdd((sum_adap_depth z1 d), (sum_adap_depth z2 (DConst 0)) )
    | ISub(z1, z2)        -> add_depths d 
                             (DSub( (sum_adap_depth z1 (DConst 0) ), (sum_adap_depth z2 (DConst 0) ) ))
    | IMaximal(z1, z2)    -> DMaximal ((sum_adap_depth z1 d), (sum_adap_depth z2 d))

  

(* Add the value of depth into adaptivity and return new Adaptivity *)  
let rec add_adap_depth z d =
  match d with
    | DBot              -> z
    | DInfty            -> z
    | DConst d          -> add_adapts z (IConst d)
    | DVar d            -> add_adapts z (IVar d)
    | DSub(d1,d2)       -> add_adapts z
                           (ISub((add_adap_depth (IConst 0) d1), (add_adap_depth (IConst 0) d2)))
    | DAdd(d1, d2)      -> IAdd ((add_adap_depth z d1), (add_adap_depth (IConst 0) d2))
    | DMaximal(d1,d2)   -> IMaximal ((add_adap_depth z d1), (add_adap_depth z d2))




(* Add an adaptivity value into each binding in the depth map and return a new depth map *)  
let rec sum_adap_dmap z dps =
  StdMap.fold (
    fun key d dps -> 
    let dps' = StdMap.add key (sum_adap_depth z d) dps
    in dps'
      ) dps StdMap.empty


(* Add a depth value to each binding in the depth map and return a new depth map *)  
let rec sum_depth_dmap q dps =
  StdMap.fold (fun key d dps -> 
    let dps' = StdMap.add key (add_depths d q) dps
    in dps') dps StdMap.empty


(* Convert a List of Depth Binding from type annotation into Map *)  
let rec to_dmap dps = 
  
  let rec helper dps dm =
    match dps with
      | [] -> dm
      | (v, d) :: tl -> 
        let dm' = StdMap.add v.v_name d dm in
          helper tl dm'
  in
    helper dps StdMap.empty


let rec to_dlist dmp =
	StdMap.bindings dmp
