open Syntax
open IndexSyntax
open Support.Options
open Format
open Print
open CheckEngine


module T = Tycheck_sigs
(*module WS = WhySolver*)
(*module E = Exist_elim*)
module SE = Support.Error

let dp = Support.FileInfo.dummyinfo

(*let main_error = SE.error_msg General*)

(*let smt_error = SE.error_msg SMT*)

let main_warning fi = SE.message 1 General fi
let main_info    fi = SE.message 2 General fi
let main_debug   fi = SE.message 1 General fi


let infile = ref (None : string option)
let outfile = ref (None : string option)



let argDefs = [
    "-i", Arg.String (fun s -> infile := Some s  ), "specify the input file name" ; 
    "-o", Arg.String (fun s -> outfile := Some s ), "specify the output file name" 
]

let parseArgs () =  
        Arg.parse argDefs 
        (fun s -> 
                match !infile  with 
                      | Some (_) -> printf "%s" "specify "  
                      | None  -> infile := Some (s) ) " " ;
             match !infile, !outfile  with
                   | Some inf, Some outf -> (inf, outf)
                   | Some inf, None  -> printf "specify your output file name by -o outfilename"; ("", "")
                   | None, Some outf -> printf "specify your input file name by -i infilename"; ("", "")
                   | _ -> printf "specify your input file name and output file name "; ("", "")



(* Parsing string *)

let parse_prog file =
    let ic = open_in file 
    in
      let lb = (Lexing.from_channel ic) in 
        Parser.toplevel Lexer.main lb



let type_check file t= 
 let (prgm, ty) = parse_prog file in
    (* Print the results of the parsing phase *)
    main_debug dp "Parsed program:@\n@[%a@]@.\n\nParsed type:@\n@[%a@]@." 
         Print.pp_expression prgm
         Print.pp_type ty;
    let ctx = Ctx.empty_context in
    let (cs, dmp, z) =  (CheckEngine.check_type ctx prgm ty) in

    (*main_info dp "Typechecking engine: %fs\n" ((Unix.gettimeofday () -. t) -. !WhySolver.smt_time);*)
    main_debug dp "Resulting constraint:@\n@[%a@]@." Print.pp_cs cs
    
(*    let tcons= Unix.gettimeofday ()  in
   try
     E.elim ctx (Constr.constr_simpl cs)
            (fun cs' ->
             let elim_cs = Constr.constr_simpl cs' in
             main_info dp "Existential elimination time: %fs\n" (Unix.gettimeofday () -. tcons);
              main_debug dp "Eliminated constraint:@\n@[%a@]@." Print.pp_cs elim_cs; 
             if ((WS.send_smt_u) elim_cs) then 
               (main_info dp "Total execution time: %fs\n" (Unix.gettimeofday () -. t);
          raise Success) else ())
   with
     Success -> main_info dp "Successfully typechecked!\n"
*)
let main = 
  let t = Unix.gettimeofday ()  in
    let (infile, outfile)  = parseArgs () in 
        type_check infile t
