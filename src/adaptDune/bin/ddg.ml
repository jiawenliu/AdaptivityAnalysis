
open Syntax
open Core
   

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
 | While ( b , lc , l ) ->  
    (dcdg lc rd_in) 
 | Seq ( lc_1,  lc_2 ) ->  (dcdg lc_1 rd_in) @ (dcdg lc_2 rd_in)
 | If ( b , lc_1 , lc_2 , l ) -> 
  (dcdg lc_1 rd_in) @ (dcdg lc_2 rd_in)
