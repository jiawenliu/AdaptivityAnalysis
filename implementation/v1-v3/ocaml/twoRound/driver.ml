open TwoRound
open Printf 
open Mechs
open TwoRoundSplit
open TwoRoundNone
open Support

let db = [[1.0; 1.0; 1.0]; [-1.0; -1.0; -1.0]; [1.0; 1.0; 1.0]; [-1.0; -1.0; -1.0]]

  let write res oc =
    fprintf oc "%f\n" res

let rec write_list res oc = 
  match res with
    | x::xs -> write x oc; write_list xs oc
    | [] -> ()

(* ****************************************************************************** *)
(* ****************************TwoRound with Gauss Mech*************************** *)
  let rec experiments_tr r error k =
     if r < !rounds then
     let db = create_db k !rows 
 in
(*  let _ = record_db db stdout in
 *)        
 let x = TwoRound.twoRound (k -. 1.0) db in
        experiments_tr (r+1) (error +. (abs_float(x) ** 2.0)) k
      else sqrt(error /. (float_of_int !rounds ))

(* ****************************************************************************** *)
(* ****************************TwoRound with Split Mech*************************** *)
  let rec experiments_tr_split r error k =
     if r < !rounds then
       let (db1, db2) =  (create_db k (!rows/. 2.0), create_db k (!rows/. 2.0))
      in
(*        let _ = record_db db1 stdout in
        let _ = record_db db2 stdout in
 *)
           let x = TwoRoundSplit.twoRound (k -. 1.0) db1 db2 in
          experiments_tr_split (r+1) (error +. (abs_float(x) ** 2.0)) k
    else sqrt(error /. (float_of_int !rounds ))

(* ****************************************************************************** *)
(* *************************TwoRound without Noise************************ *)
  let rec experiments_tr_none r error k =
     if r < !rounds then
    let db = create_db k !rows
      in 
(*  let _ = record_db db stdout in 
 *)
 (*  	let db = db in  
 *) 	
 let x = TwoRoundNone.twoRound (k -. 1.0) db in
        experiments_tr_none (r+1) (error +. (abs_float(x)**2.0) ) k
      else sqrt(error /. (float_of_int !rounds ))

(* ****************************************************************************** *)
(* *************************Experiment driver with colnum************************ *)
(* ****************************************************************************** *)

let experimet_for_one_colnum oc colnum =
let _ = print_endline ("column number: " ^ string_of_float(colnum)) in
if (!mech_name = "split") then
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
    let _ = Random.init( int_of_float(Unix.time ()) ) in 
    let (ofile) = parseArgs() in
    let oc = open_out ofile in
    experiments_for_colnums (!colst) oc

              
