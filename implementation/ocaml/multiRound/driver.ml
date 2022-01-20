open Printf 
open Mechs
open MultiRound
open Support

let n = float_of_int (!population_N)

(* let db = [[1.0; 1.0; 1.0]; [-1.0; -1.0; -1.0]; [1.0; 1.0; 1.0]; [-1.0; -1.0; -1.0]] 
*)


(* let population = ref Support.gen_dataset (!population_N)  *)

(* ****************************************************************************** *)
(* ****************************TwoRound with Gauss Mech*************************** *)
let rec experiments_mr r errors  =
  if r < !trails then
    let db = Support.create_db !cols !rows in
    let dom = (domain db) in
    (* let _ = record_db db stdout in *)
      (* let _ = record_db !Support.population stdout in *)
      
      let n = float_of_int (!population_N) in 
      (* let _ = write (n*.n) stdout in *)
      let c = n  in
        let sc = Support.mr_initial n in
          let scc = Support.mr_initial c in
      let results = MultiRound.multiRound () !rounds 0.0 sc scc [] n c db dom in
          (* let error = (List.map (fun (a, b) -> abs_float(a -. b)**2.0) results) in *)
          let error = (List.map (fun (a, b) -> abs_float(a)**2.0) results) in

          (* let _ = write_list error stdout in *)
          experiments_mr (r+1) (Support.zip (+.) error errors)
      else errors

(* ****************************************************************************** *)
(* *************************Experiment driver with colnum************************ *)
(* ****************************************************************************** *)

let experimet_for_fixed_size oc row =
let _ = print_endline ("row number: " ^ string_of_float(row)) in

let result = experiments_mr 0 (Support.mr_initial !rounds) in
  let result = (List.map (fun r -> r /. (float_of_int !trails)) result) in 
  (* let result = (List.fold_left (fun acc a -> acc + a) 0.0 result) / (!rounds) *)
  let _ = write_list result stdout in
        write_list result oc         

 (* 	  let _ = write result stdout in
        write result oc 
 *)

(* ****************************************************************************** *)
(* Experiment driver with colnum range from row to !rows *)
(* ****************************************************************************** *)

let rec experiments_for_sizes row oc =
if row <= !rows
  then 
    let _ = experimet_for_fixed_size oc (row)
    in 
      let row = row +. 10.0
      in
        experiments_for_sizes row oc
  else
    close_out oc




let main  = 
    let _ = Random.init(int_of_float(Unix.time ()) ) in 
    let (ofile) = parseArgs() in
    let oc = open_out ofile in
    experiments_for_sizes (!rows) oc

              
