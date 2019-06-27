open TwoRound
open MultiRound
open Printf 
open HeadFile

  let write res oc =
    fprintf oc "%f\n" res

  let rec experiments oc i =
     if i < !rounds then
        let x = TwoRound.twoRound 3.0 in
        write (x) oc ; experiments oc (i+1)
      else close_out oc

  let main  = 
    let (ifile,ofile) = parseArgs() in
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
     experiments oc 0
