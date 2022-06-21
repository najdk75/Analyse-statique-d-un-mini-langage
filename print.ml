open Syntax;;


let indentation = "  ";;

(* on affiche i indentation, dépendant du niveau de profondeur dans lequel on se situe*)
let rec print_indent (i:int) =
  match i with 
  |0 -> ()
  |_ -> print_string indentation; print_indent (i-1);;


(** fonction qui prend en paramètre un opérateur de comparaison et le convertit en string  *)
let comp_to_string (s: comp) : string =
  match s with 
  |Eq ->  "=" 
  |Ne ->  "<>"
  |Lt ->  "<"
  |Le ->  "<="
  |Gt ->  ">"
  |Ge ->  ">=" ;;
  
           
 
(** fonction qui prend en paramètre un opérateur arithmétique et le convertit en string *)
let operand_to_string (s: op) : string =
  match s with 
  |Add ->  "+"
  |Sub ->  "-"
  |Mul ->  "*"
  |Div ->  "/"
  |Mod ->  "%"
  

(** fonction qui prend une opération et la convertit en string *)

let rec op_to_string (opr:op) (exp1:expr) (exp2:expr) : string = 
  (operand_to_string opr) ^ " " ^ (exp_to_string exp1) ^ " " ^ (exp_to_string exp2)
            
(** fonction qui prend en paramètre une expression arithmétique la convertit en string  *)

and exp_to_string ( e: expr) : string = 
  match e with 
  |Num (i)->  string_of_int i
  |Var (name) -> name
  |Op (opr,exp1,exp2) -> op_to_string opr exp1 exp2;;
  




(** fonction qui prend en paramètre une condition la convertit en string  *)
let cond_to_string (c : cond) : string = 
  
  let (exp1,cmp,exp2) = c in 

  (exp_to_string exp1) ^ " " ^ (comp_to_string cmp ) ^ " " ^ (exp_to_string exp2);;


(** les deux fonctions qui prennent en paramètre affichent une instruction et un block  *)
let rec print_block (b: block) (indent:int)  : unit = 

  match b with 
  | [] -> ()
  
    |(_,inst)::t ->  print_instr inst indent ; print_block t indent ;
      
    
(*On affiche l'indentation nécessaire en début de ligne, puis par fonctions auxiliaires on convertit l'instruction en string et on l'affiche proprement*)
and print_instr (s:instr) (indent:int) : unit = 
  print_indent indent;
  match s with 
  |Read(v) ->  Printf.printf ("READ %s\n") v;
  |Print(exp)-> Printf.printf ("PRINT %s\n") (exp_to_string exp);
      
      
  |Set(n,exp)-> Printf.printf ("%s := %s\n") n (exp_to_string exp);
  
  |If (cnd,blc1,blc2) -> Printf.printf ("IF %s\n") (cond_to_string cnd);
                         print_block blc1 (indent+1);
                         begin match blc2 with
                         |[] -> ()
                         |_  -> print_indent (indent);Printf.printf ("ELSE\n"); print_block blc2 (indent+1);
                         end
          
  | While(cnd,blc)-> Printf.printf ("WHILE %s\n") (cond_to_string cnd); print_block blc (indent+1);;



