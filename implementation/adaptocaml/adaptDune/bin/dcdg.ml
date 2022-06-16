
open Syntax
open Core

let vars2varstring (vars: var_info list) : string list =
  List.map ~f: (fun var -> var.v_name) vars


let live_vars (vars: var_info list) (rd_in : Df.sigma) : lvar list =
  let vars_string = vars2varstring vars in
  let alive (v, _) = List.mem vars_string v ~equal: String.equal in  
  let sigma =  List.filter ~f:alive  rd_in in
  List.map ~f:(fun (v, i) -> LabelVar(v,i) ) sigma

let assigned_lvars program : lvar list = 
    let rec assigned = function
    | Skip  -> []
    | Assign ( var , _ , l ) -> [ LabelVar( var.v_name, print_label l)  ]
    | Query ( var ,  _ , l ) -> [ LabelVar( var.v_name, print_label l)  ]
    | While ( _ , lc , _ ) ->   assigned lc
    | Seq ( lc_1,  lc_2 ) -> (assigned lc_1) @ (assigned lc_2) 
    | If ( _ , lc_1 , lc_2 , _ ) -> (assigned lc_1) @ (assigned lc_2) 
   in 
      let result = assigned program in
       result

   let combine_lvars (a:lvar list) (b:lvar list) : (lvar * lvar) list =
      List.fold_left ~init:[] a  
      ~f:(fun result lvar_a -> 
        let a_blist = List.map ~f:(fun lvar_b-> (lvar_a,lvar_b)) b in 
        List.append a_blist result  )    

let rec dcdg program (cfg:Cfg.t) (rd_in:Df.rd_results) : (lvar * lvar) list = 
 match program with
 |  Skip  -> []
 | Assign ( x , e , l) ->  
   let label_int = print_label l in
   let lvar_x  = LabelVar (x.v_name, label_int ) in
   let rdin_l = Int.Map.find_exn rd_in label_int in
   let vars = Cfg.vars e in 
   let live_vars = live_vars vars rdin_l in
   List.map  ~f:(fun lvar_y -> ( lvar_x, lvar_y )  )  live_vars
 | Query ( x ,  q , l ) -> let label_int = print_label l in
   let lvar_x  = LabelVar (x.v_name, label_int ) in
   let rdin_l = Int.Map.find_exn rd_in label_int in
   let vars = Cfg.qvars q in 
   let live_vars = live_vars vars rdin_l in
   List.map  ~f:(fun lvar_y -> ( lvar_x, lvar_y )  )  live_vars
 | While ( b , lc , l ) ->  
    let label_int = print_label l in
    let rdin_l = Int.Map.find_exn rd_in label_int in
    let lvars_y = assigned_lvars lc in
    let vars = Cfg.vars_b b in
    let lvars_x = live_vars vars rdin_l in
     (combine_lvars lvars_x lvars_y) @
    (dcdg lc cfg rd_in) 
 | Seq ( lc_1,  lc_2 ) ->  (dcdg lc_1 cfg rd_in) @ (dcdg lc_2 cfg rd_in)
 | If ( b , lc_1 , lc_2 , l ) -> 
  let label_int = print_label l in
  let rdin_l = Int.Map.find_exn rd_in label_int in
  let lvars_y_1 = assigned_lvars lc_1 in
  let lvars_y_2 = assigned_lvars lc_2 in
  let vars = Cfg.vars_b b in
  let lvars_x = live_vars vars rdin_l in
   (combine_lvars lvars_x lvars_y_1) @  (combine_lvars lvars_x lvars_y_2) @
  (dcdg lc_1 cfg rd_in) @ (dcdg lc_2 cfg rd_in)
