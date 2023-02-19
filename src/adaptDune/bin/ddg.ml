
open Syntax
open Core
   
let vars2varstring (vars: var_info list) : string list =
  List.map ~f: (fun var -> var.v_name) vars


let live_vars (vars: var_info list) (rd_in : Df.sigma) : lvar list =
  let vars_string = vars2varstring vars in
  let alive (v, _) = List.mem vars_string v ~equal: String.equal in  
  let sigma =  List.filter ~f:alive  rd_in in
  List.map ~f:(fun (v, i) -> LabelVar(v,i) ) sigma

let rec ddg program  (rd_in:Df.rd_results) : (lvar * lvar) list = 
 match program with
 | Skip _  -> []
 | Assign ( x , e , l) ->  
   let label_int = print_label l in
   let lvar_x  = LabelVar (x.v_name, label_int ) in
   let rdin_l = Int.Map.find_exn rd_in label_int in
   let vars = Cfg.vars e in 
   let live_vars = live_vars vars rdin_l in
   List.map  ~f:(fun lvar_y -> ( lvar_y, lvar_x )  )  live_vars
 | Query ( x ,  q , l ) -> let label_int = print_label l in
   let lvar_x  = LabelVar (x.v_name, label_int ) in
   let rdin_l = Int.Map.find_exn rd_in label_int in
   let vars = Cfg.qvars q in 
   let live_vars = live_vars vars rdin_l in
   List.map  ~f:(fun lvar_y -> ( lvar_y, lvar_x )  )  live_vars
 | While ( _ , lc , _ ) ->  
    (ddg lc rd_in) 
 | Seq ( lc_1,  lc_2 ) ->  (ddg lc_1 rd_in) @ (ddg lc_2 rd_in)
 | If ( _ , lc_1 , lc_2 , _ ) -> 
  (ddg lc_1 rd_in) @ (ddg lc_2 rd_in)
