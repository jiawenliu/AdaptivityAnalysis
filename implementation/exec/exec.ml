open TwoRound
open MultiRound
open Printf 
open HeadFile



  let write res oc =
    fprintf oc "%f\n" res

let rec write_list res oc = 
  match res with
    | x::xs -> write x oc; write_list xs oc
    | [] -> ()

  let rec experiments_tr r oc dataset =
     if r < !rounds then
        let x = TwoRound.twoRound 6.0 dataset in
        write (x) oc ; experiments_tr (r+1) oc dataset
      else close_out oc

  let rec experiments_mr r n k oc dataset  =
     if r < !rounds then
      let c = n in
        let sc = mr_initial n in
          let scc = mr_initial n in
        let x = MultiRound.multiRound () k 0.0 sc scc [] n c dataset in
        write_list (x) oc ; 
        experiments_mr (r+1) n k oc dataset
      else ()

  let main  = 
    let (ifile, ofile) = parseArgs() in

(*    let dataset = 
    if (!cdb) then 
     let ic = open_out ifile in
     let db = creat_db !rows !cols in 
      record_db db ic ; cdb:= true ; db 
   else
        let ic = open_in ifile in 
        let db = read_db ic !rows !cols in close_in ic; db 
      in  
*)    let oc = open_out ofile in 
        let ic = open_in ifile in
          let dataset = read_db ic !rows !cols in 
            let _ = experiments_mr 0 8.0 10.0 oc dataset in
              close_out oc;
              close_in ic
