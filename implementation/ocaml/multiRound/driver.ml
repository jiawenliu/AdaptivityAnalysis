open Printf 
open Mechs
open MultiRound
open Support

let n = 10

let db = [[1.0; 1.0; 1.0]; [-1.0; -1.0; -1.0]; [1.0; 1.0; 1.0]; [-1.0; -1.0; -1.0]]

  let write res oc =
    fprintf oc "%f\n" res

let rec write_list res oc = 
  match res with
    | x::xs -> write x oc; write_list xs oc
    | [] -> ()

(* ****************************************************************************** *)
(* ****************************TwoRound with Gauss Mech*************************** *)
let rec experiments_mr r error k  =
  if r < !rounds then
    let db = Support.create_db k !rows in
      let c = n in
        let sc = Support.mr_initial n in
          let scc = Support.mr_initial n in
      let x = MultiRound.multiRound () k 0.0 sc scc [] n c dataset in
           
          experiments_mr (r+1) (error +. (abs_float(x) ** 2.0))  k
      else sqrt(error /. (float_of_int !rounds ))

(* ****************************************************************************** *)
(* *************************Experiment driver with colnum************************ *)
(* ****************************************************************************** *)

let experimet_for_one_colnum oc colnum =
let _ = print_endline ("column number: " ^ string_of_float(colnum)) in

let result = experiments_tr_split 0 0.0 colnum in
  let _ = print_endline (string_of_float(result)) in
        write result oc 


(* if (!mech_name = "split") then
      let result = experiments_tr_split 0 0.0 colnum in
	let _ = print_endline (string_of_float(result)) in
        write result oc 
else
  if (!mech_name = "non") then
      let result = experiments_tr_none 0 0.0 colnum in
	let _ = print_endline (string_of_float(result)) in
        write result oc
  else 
      let result = experiments_tr 0 0.0 colnum in
	let _ = print_endline (string_of_float(result)) in
        write result oc
 *)
(* ****************************************************************************** *)
(* Experiment driver with colnum range from colst to col *)
(* ****************************************************************************** *)

let rec experiments_for_colnums colnum oc =
if colnum < !cols
  then 
    let _ = experimet_for_one_colnum oc (colnum)
    in 
      let coln = colnum +. 1.0
      in
        experiments_for_colnums coln oc
  else
    close_out oc




let main  = 
    let _ = Random.init(int_of_float(Unix.time ()) ) in 
    let (ofile) = parseArgs() in
    let oc = open_out ofile in
    experiments_for_colnums (!colst) oc

              
