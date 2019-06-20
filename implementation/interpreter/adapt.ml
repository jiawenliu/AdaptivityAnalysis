open Syntax
open IndexSyntax
open Support.Options
open Format
open Print
open TyCheck

let inprog = ref (None : string option)
let infile = ref (None : string option)
let isfile = ref false

module T = Tycheck_sigs
module WS = WhySolver
module E = Exist_elim
module SE = Support.Error

let dp = Support.FileInfo.dummyinfo

let main_error = SE.error_msg General

let smt_error = SE.error_msg SMT

let main_warning fi = SE.message 1 General fi
let main_info    fi = SE.message 2 General fi
let main_debug   fi = SE.message 4 General fi


let argDefs = [
    "-prog", Arg.String (fun s -> inprog := Some s  ), "specify the input program string, -ip string" ; 
      "-file", Arg.String (fun s -> infile := Some s; isfile := true ), "specify the input file name, -if string" 
]

let parseArgs () =  
        Arg.parse argDefs 
        (fun s -> 
                match !inprog  with 
                      | Some (_) -> printf "%s" "specify just the programs"  
                      | None  -> inprog := Some (s) ) " " ;
             match !inprog  with
                   | Some i -> (i)
                   | _ -> 
                   (
                    match !infile with
                          | Some i -> (i)
                          | _ -> printf "%s" "specify  your input file -if or intput program string -ip"; ""
                    )



(* Parsing string *)

let parse_prog prog =
  if (!isfile) 
  then
    let fh = open_in prog 
    in
      let lb = (Lexing.from_channel fh) in 
        Parser.toplevel Lexer.main lb
  else 
    let lb = Lexing.from_string prog
    in
      Parser.toplevel Lexer.main lb


let type_check infile  t= 
let (prgm, ty) = parse_prog !infile in
    (* Print the results of the parsing phase *)
    main_debug dp "Parsed program:@\n@[%a@]@.\nParsed type:@\n@[%a@]@." 
         Print.pp_expr prgm Print.pp_type ty;
    let ctx = Ctx.set_exec_mode mu (Ctx.empty_context) in
    let cs =  (TyCheck.check_type ctx prgm ty) in
    
    main_info dp "Typechecking engine: %fs\n" ((Unix.gettimeofday () -. t) -. !WhySolver.smt_time);
    main_debug dp "Resulting constraint:@\n@[%a@]@." Print.pp_cs cs;
    
    let tcons= Unix.gettimeofday ()  in
   try 
     E.elim ctx (Constr.constr_simpl cs)
            (fun cs' ->
             let elim_cs = Constr.constr_simpl cs' in
             main_info dp "Existential elimination time: %fs\n" (Unix.gettimeofday () -. tcons);
            (*  main_debug dp "Eliminated constraint:@\n@[%a@]@." Print.pp_cs elim_cs; *)
             if ((WS.send_smt_u) elim_cs) then 
               (main_info dp "Total execution time: %fs\n" (Unix.gettimeofday () -. t);
          raise Success) else ())
   with
     Success -> main_info dp "Successfully typechecked!\n"

let main = 
  let t= Unix.gettimeofday ()  in
    let infile = parseArgs () in 
        type_check_un infile t
