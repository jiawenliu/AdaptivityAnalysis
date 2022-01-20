open List
open Distribution
open Printf
open Support 


let thresholdout_mech (q:query) db =
  let holdout_db = create_db (float_of_int (List.length (hd db))) (float_of_int(List.length db)) in
    let threshold = 1.0 in
      let noise_rate = 1.0 in
        let budget = (List.length db) in
              let rec threshold_mech db holdout_db gamma =
                match (db, holdout_db) with
                  |([],[]) -> []
                  |(rw::t, holdout_rw::holdout_t) -> let eta = (sample_exp (4.0 *. noise_rate)) in
                    if ((q rw) -. (q holdout_rw)) > (threshold +. gamma +. eta)
                    then
                      ((q holdout_rw) +. (sample_exp noise_rate))::(threshold_mech t holdout_t (sample_exp 2.0 *. noise_rate))
                    else
                      (q rw)::(threshold_mech t holdout_t gamma)
                  | _ -> printf "This shouldn't happen\n";[]
                  in
                    let sm = List.fold_left 
                      (fun sum rw ->  
                          sum +. rw
                      ) 0.0 (threshold_mech db holdout_db (sample_exp (2.0 *. noise_rate)))
                    in 
                      (sm /.  float_of_int (List.length (hd db)))


(*let rec read_rows ic i j acc =
        if i < j then
         read_line ic (i+1) j (float_of_string (input_line ic)) :: acc
        else
         List.rev acc *)
       

let sum_q (db: float list list) = 
    List.fold_left ( fun a r -> a +.  (hd r)  ) 0.0 db  

let gauss_mech (q:query) db =  
  let result =
    let mean = 
      	let sm =  List.fold_left 
          ( fun sum rw ->  
              sum +. (q rw)
          ) 0.0 db  
        in  
        sm /.  float_of_int (List.length db)

    in
(*       let _ = print_endline(string_of_int(List.length db) ^ (string_of_int(List.length (List.nth db 0)))) in
 *)        
 		(* let sigma = ( sqrt( (sqrt (float_of_int (List.length (List.nth db 0))) )   /. (float_of_int (List.length db)) )) *)
 		(*2.0 *. log(1.25 /. delta) *. 2.0 *. 2.0 /. epsilon*)
 		let sigma = ( sqrt( (sqrt (!rounds) )   /. (float_of_int (List.length db)) ))
      	in
        (* let _ = print_endline (string_of_float sigma)
         in *) 
        mean +. (sample_normal_boxmuller3 0.0 sigma) 
  in 
  if result > 1.0 then 1.0 
  else  if result < 0.0 then 0.0 else result 

let nonoise_mech (q:query) db =  
    let mean = 
      let sm =  List.fold_left 
          ( fun sum rw -> 

              sum +.  (q rw)
          ) 0.0 db  
        in 
        (* let _ =  fprintf stdout "%f " sm in  *)
        sm /.  float_of_int (List.length db)

      in
      (* let _ =  fprintf stdout "%f " mean in *)
    mean 


let mech = nonoise_mech


