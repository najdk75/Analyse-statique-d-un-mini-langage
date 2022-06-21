module NameTable = Map.Make(String);;
open NameTable;;
open Syntax ;;

(***********************************************************************)
(* L'environnement qu'on a choisi est une map : String -> int option. À chaque lecture de blocks, chaque variable sera stocké dans cet environnement.
On associera le nom de cette variable de type String à une valeur de type int Option.
*)


(*Cette fonction sert à retourner un int après l'évaluation arithmétique de l'expression donnée en prenant en compte l'environnement
  Renverra une erreur si : 
                            - Une variable de l'expression n'appartient pas à l'environnement 
                            - Une division par 0 ou un modulo par 0.
*)

let rec eval_expr (expr:expr) (env:int option NameTable.t):int  = 
  match expr with 
  |Num(x) -> x (* Si c'est un nombre alors on renvoit ce nombre *)
  |Var(s) -> begin try Option.get(find s env) with Not_found -> Printf.printf ("La variable %s n'a pas été initialisée pour être affichée.\n") s; exit(1) end 
  (* On cherche dans l'environnement la valeur associée à la variable s*)
  |Op(op,expr1,expr2) -> eval_op op expr1 expr2 env (*Sinon on évalue l'expression récursivement*)

and eval_op (op:op) (expr1 :expr) (expr2:expr) (env):int = (*Gère les opérations arithmétiques entre deux expressions dans le cas où expr est une opération*)
  match op with
  |Add -> (eval_expr expr1 env) + (eval_expr expr2 env) 
  |Sub -> (eval_expr expr1 env) - (eval_expr expr2 env)
  |Mul -> (eval_expr expr1 env) * (eval_expr expr2 env)
  |Div ->  begin try (eval_expr expr1 env) / (eval_expr expr2 env) with Division_by_zero -> Printf.printf ("Division par zéro interdite.\n"); exit(1) end
  |Mod -> match (eval_expr expr1 env) with 
    |0 -> Printf.printf ("Module zéro interdit.\n");exit(1);
    | _-> (eval_expr expr1 env) mod (eval_expr expr2 env);;

(*Évalue l'expression donnée et l'ajoute à l'environnement, écrase la valeur de la clef n si déjà existante *)
let eval_set (n:name) (exp:expr) (env:int option NameTable.t) = add n (Some(eval_expr exp env)) env;; 

(* Affiche le nom de la variable à l'utilisateur et lui demande d'écrire une valeur, termine sur une erreur si la valeur écrite n'est pas un int
sinon l'ajoute à l'environnement *)


let rec eval_read (n:name) (env:int option NameTable.t) (i:int) = 
  Printf.printf "%s ?\n" n;

  let user_choice = read_int_opt() in
  match user_choice with
  |Some e -> add n user_choice env
  |None   -> if i == 0 then exit(1) else Printf.printf "Écrivez un nombre svp. %d tentative(s) restante(s) ?\n" i; eval_read n env (i-1);;

(*Calcule la valeur de l expression en paramètre,l'ajoute à l'environnement, affiche cette valeur et retourne l'environnement pour respecter 
   l'implémentation de eval_instr. *)

let eval_print (exp:expr) (env:int option NameTable.t): int option NameTable.t = 
  let expr_res = eval_expr exp env in 
  Printf.printf "%d \n" (expr_res); 
  env;;

let eval_cond cmp expr1 expr2 (env:int option NameTable.t) = 
  let expr1_value = eval_expr expr1 env in (* eval_expr retourne un int*)
  let expr2_value = eval_expr expr2 env in 
  match cmp with
  | Eq ->  expr1_value == expr2_value 
  | Ne ->  expr1_value <> expr2_value 
  | Lt ->  expr1_value < expr2_value 
  | Le ->  expr1_value <= expr2_value 
  | Gt ->  expr1_value > expr2_value 
  | Ge ->  expr1_value >= expr2_value 


let rec eval_block (blc:block) (env:int option NameTable.t) = 
  let rec aux blc env =
    match blc with
    |[]                  -> env
    |(_,instr)::tl_blc -> aux tl_blc (eval_instr instr env) 

  in aux blc env
  
and eval_instr (i:instr) (env: int option NameTable.t) = 
  match i with 
  |   Set(n,exp)                         -> eval_set n exp env
  |   Read(n)                            -> eval_read n env 1 
  |   Print(expr)                        -> eval_print expr env
  |   If((expr1,cnd,expr2),blc1,blc2)    -> eval_if expr1 cnd expr2 blc1 blc2 env
  |   While ((expr1,cnd,expr2),blc)      -> eval_while expr1 cnd expr2 blc env

and 
  eval_if expr1 cnd expr2 blc1 blc2 env = 
  if  eval_cond cnd expr1 expr2 env  then eval_block blc1 env else eval_block blc2 env
and  
  eval_while expr1 cnd expr2 blc env = 

  if eval_cond cnd expr1 expr2 env then eval_while expr1 cnd expr2 blc (eval_block blc env) else env;;

