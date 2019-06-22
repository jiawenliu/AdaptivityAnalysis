open TwoRound
open MultiRound
open Printf 

  let write res oc =
    fprintf oc "%f\n" res

  let rec experiments oc i db =
     if i < !rounds then
        let x = two_round db !cols thresholdout_mech in
        let y =  two_round db !cols nonoise_mech in
        write (x-.y) oc ; experiments oc (i+1) db
      else close_out oc

  let main  = 
   let (ifile,ofile) = parseArgs() in
   let dataset = 
    if (!cdb) then 
     let ic = open_out ifile in
     let db = creat_db !rows !cols in 
      record_db db ic ; cdb:= true ; db 
   else
        let ic = open_in ifile in 
        let db = read_db ic !rows !cols in close_in ic; db 
      in  
    let oc = open_out ofile in 
     experiments oc 0 dataset
