open Syntax ;;
open Print ;;
open Read ;;
open Eval ;;
open Simpl ;;
open Vars ;;
open Sign ;;





let read_polish (filename:string) : program =  
  
    makeprogram (read_and_extract_all filename) 
;;

let print_polish (p:program) : unit =  
    
    print_block p 0;;
;;

let  eval_polish (p:program) : unit =  
    
 let res = eval_block p NameTable.empty in ()
;;


let simpl_polish (p: program) : program =
    
  instrList_into_bloc (simpl_block p ) 0
;;

let vars_polish (p:program) : unit = 
    get_vars p
;;


let sign_polish (p:program) : unit = let res = sign_block p VarTable.empty in print_sign res;;

let usage () =
  print_string "Polish : analyse statique d'un mini-langage\n";
  print_string "usage: à documenter (TODO)\n"

let main () =
  match Sys.argv with
  | [|_;"-reprint";file|] -> print_polish (read_polish file)
  | [|_;"-eval";file|] -> eval_polish (read_polish file)
  | [|_;"-simpl";file|] -> print_polish (simpl_polish(read_polish file))
  | [|_;"-vars";file|]  -> vars_polish (read_polish file)
  | [|_;"-sign";file|]  -> sign_polish (read_polish file)
  | _ -> usage ()

(* lancement de ce main *)
let () = main ()

