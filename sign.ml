module VarTable = Map.Make(String);;
open VarTable;;
open Syntax;;



type sign = Neg | Zero | Pos | Error
         

(*************************** Fonctions d'affichage  ***************************)
let print_sign s = 
  match s with 
  |Neg   -> print_string "- ";
  |Zero  -> print_string "0 ";
  |Pos   -> print_string "+ ";
  |Error -> print_string "!";;

let printlist l = List.iter print_sign l;;

let rec print_bindings l = 
  match l with
  |[] -> print_string("\n")
  |(a,b)::tl -> print_string(a); print_string(" -> "); printlist b; print_string("\n"); print_bindings tl;;



let print_sign vartable = let res = bindings vartable in print_bindings res;;



(*****************************************************************************)
                                                                              
(*************************** Fonctions pour reconnaître un OP  ***************************)
let isAdd ope = ope = Add;;
let isSub ope = ope = Sub;;
let isMul ope = ope = Mul;;
let isDiv ope = ope = Div;;
let isMod ope = ope = Mod;;
(****************************************************************************************)
(* Retire les doublons d'une liste  *)
let no_duplicates l =
  let rec aux l res =
    match l with
    |[] -> res
    |hd::tl -> if List.mem hd res then aux tl res else aux tl (hd::res) in
  aux l [];;



(* Donne le signe d'une constante k *)
let sign_const k = if k = 0 then Zero else if k < 0 then Neg else Pos;;


(* Donne le signe d'une variable v, erreur si elle n'a pas été initialisée avant *)
let var_sign v vartable = begin try VarTable.find v vartable with Not_found 
  ->Printf.printf ("La variable %s n'a pas été initialisée pour être affichée.\n") v;
    exit(1) end;;

(* Donne le signe d'une expression recursivement *)
let rec sign_expr expr vartable =
  match expr with
  |Num(x) -> [sign_const x]
  |Var(v) -> var_sign v vartable 
  |Op(op,expr1,expr2) ->  update_sign_l1 (sign_expr expr1 vartable) (sign_expr expr2 vartable) op

and 
  sign_op (s1:sign) (s2:sign) (ope:op)=
  match s1,s2 with
  | _,Error | Error, _ -> [Error]
  |Zero,Zero           -> if isMod ope || isDiv ope then [Error] else [Zero]
  |Neg,Neg             -> if isDiv ope || isMul ope then [Pos] else if isAdd ope || isMod ope then [Neg] else [Pos;Neg;Zero]
  |Pos,Pos             -> if isAdd ope || isMul ope || isDiv ope || isMod ope then [Pos] else [Pos;Neg;Zero]
  |Pos,Neg             -> if isMul ope || isDiv ope || isMod ope then [Neg] else if isAdd ope then [Pos;Neg;Zero] else [Pos]
  |Neg,Pos             -> if isMul ope || isDiv ope then [Neg] else if isMod ope then [Pos] else if isSub ope then [Neg] else [Pos;Neg;Zero]
  |Pos,Zero            -> if isDiv ope || isMod ope then [Error] else if isMul ope then [Zero] else [Pos]
  |Zero,Pos            -> if isMul ope || isDiv ope || isMod ope then [Zero] else if isAdd ope then [Pos] else [Neg]
  |Neg,Zero            -> if isDiv ope || isMod ope then [Error] else if isMul ope then [Zero] else [Neg] 
  |Zero,Neg            -> if isMul ope || isDiv ope || isMod ope then [Zero] else if isSub ope then [Pos] else [Neg]

and add_signs_l1 s1 l2 op =
  let rec aux l res =
    match l with 
    |[] -> res
    |s2::tl -> aux tl (sign_op s1 s2 op)@res in
  aux l2 []
  
and update_sign_l1 l1 l2 op = 
  let rec aux l res =
    match l with
    |[] -> no_duplicates res
    |sign::tl -> aux tl ((add_signs_l1 sign l2 op)@res) in 
  aux l1 [];;



(*Fonction servant à inverser une condition *)
let inverse_cond cond = 
  match cond with
  | (x,Eq,y) -> (x,Ne,y)
  | (x,Ne,y) -> (x,Eq,y)
  | (x,Lt,y) -> (x,Ge,y)
  | (x,Le,y) -> (x,Gt,y)
  | (x,Gt,y) -> (x,Le,y)
  | (x,Ge,y) -> (x,Lt,y);;

(*Fonction servant à inverse les signes*)
let inverse_sign s = if s = Neg then Pos else if s = Pos then Neg else s;;

(*Fonction servant à inverser les combinaisons de signe selon un opérateur spécifique*)
let inverse_combination y = 
  let (a,b) = List.split y in
  let map_a = List.map inverse_sign a in
  let map_b = List.map inverse_sign b in
  List.combine map_a map_b;;

(*Fonction servant à inverser un comparateur*)
let inverse_cmp cmp = 
  match cmp with
  |Lt -> Gt
  |Gt -> Lt
  |Le -> Ge
  |Ge -> Le
  |_  -> cmp;;

(*Fonction vérifiant si une expression a comme unique signe error*)
let isError expr vartable = (sign_expr expr vartable) = [Error];;
(* retourne liste sans Erreur *)
let rmv_err l = List.filter (fun x -> x <> Error) l;;


let in_s1s2 sign_list1 sign_list2 s1 s2 = List.mem s1 sign_list1 && List.mem s2 sign_list2;;


let rec check_combination sign_expr1 sign_expr2 combinations = 
  match combinations with
  |[] -> false
  |(s1,s2)::tl_combinations -> if in_s1s2 sign_expr1 sign_expr2 s1 s2 then true else check_combination sign_expr1 sign_expr2 tl_combinations;;



(* Pour "=" Si les expressions ont un signe en commun qui n'est pas erreur alors on renvoie vrai, sinon on renvoie faux
   Pour "<>" S'il existe un signe dans la premiere expression qu'il n'y a pas dans la deuxieme alors on renvoie vrai sinon faux
*)
let verify_eq_ne expr1 expr2 vartable op = 
  let sign_expr1 = sign_expr expr1 vartable in
  let sign_expr2 = sign_expr expr2 vartable in
  let inter_s1s2 = List.filter (fun x -> List.mem x sign_expr2 ) sign_expr1  in
  let size_inters1s2 =  (List.length inter_s1s2) in 
  match op with
  | Eq -> size_inters1s2 > 0 && inter_s1s2 <> [Error]
  | Ne -> List.exists (fun x -> not(List.mem x sign_expr2) ) sign_expr1
  | _  -> false;;

(*Si une combinaison est vérifiée parmi la liste des combinaisons proposées alors on renvoie vrai, sinon on renvoie faux*)

let verify_bis expr1 expr2 vartable op combination = 
  let sign_expr1 = sign_expr expr1 vartable in
  let sign_expr2 = sign_expr expr2 vartable in
  check_combination sign_expr1 sign_expr2 combination
 
 


(* Si une des deux expressions n'a qu'Erreur comme signe alors on renvoie faux, sinon on regarde les combinaisons à vérifier*)

let verify_cond cnd vartable = 
  let (expr1,cmp,expr2) = cnd in 
  if isError expr1 vartable  || isError expr2 vartable then false 
  else 
    
    let combination_gt = [(Pos,Pos);(Pos,Neg);(Pos,Zero);(Zero,Neg);(Neg,Neg)] in
    let combination_ge = [(Pos,Pos);(Pos,Neg);(Pos,Zero);(Zero,Neg);(Neg,Neg);(Zero,Zero)] in
    match cmp with 
    |Eq -> verify_eq_ne expr1 expr2 vartable Eq
    |Ne -> verify_eq_ne expr1 expr2 vartable Ne
    |Gt -> verify_bis   expr1 expr2 vartable Gt combination_gt
    |Lt -> verify_bis   expr1 expr2 vartable Lt (inverse_combination combination_gt)
    |Ge -> verify_bis   expr1 expr2 vartable Ge combination_ge
    |Le -> verify_bis   expr1 expr2 vartable Le (inverse_combination combination_ge);;
        

(* fonctions gérant la propagation de variable *)
let propagation_gt sign = sign = [Zero] || sign = [Pos] || sign = [Pos;Zero];;
let propagation_lt sign = sign = [Zero] || sign = [Neg] || sign = [Neg;Zero];;



let cmp_eq v expr vartable =
  let sign_expr = sign_expr expr vartable |> rmv_err in
  let sign_v = find v vartable in 
  let inter = List.filter (fun x -> List.mem x sign_v ) sign_expr in
  add v inter vartable;; 


let cmp_gt v expr vartable = 

  let sign_expr = sign_expr expr vartable |> rmv_err  in
  if propagation_gt sign_expr then add v [Pos] vartable else vartable;;

let cmp_lt v expr vartable = 
  let sign_expr = sign_expr expr vartable |> rmv_err in
  if propagation_gt sign_expr then add v [Neg] vartable else vartable;;


let get_signs_ge_le v sign1 vartable = 
  let v_sign = find v vartable in
  if List.mem sign1 v_sign && not(List.mem Zero v_sign) then add v [sign1] vartable 
  else if List.mem sign1 v_sign && List.mem Zero v_sign then add v [sign1;Zero] vartable
  else if List.mem Zero v_sign && not(List.mem sign1 v_sign) then add v [Zero] vartable
  else vartable


let cmp_ge v expr vartable = 
  let sign_expr = sign_expr expr vartable |> rmv_err in
  match sign_expr with
  |[Zero] -> get_signs_ge_le v Pos vartable
  |[Pos;Zero] -> add v [Pos;Zero] vartable
  |[Pos]               -> add v [Pos] vartable
  |_                   -> vartable;;



let cmp_le v expr vartable = 
  let sign_expr = sign_expr expr vartable |> rmv_err in
  match sign_expr with
  |[Zero] -> get_signs_ge_le v Neg vartable
  |[Zero;Neg] -> add v [Zero;Neg] vartable
  |[Neg]               -> add v [Neg] vartable
  |_                   -> vartable;;

let cmp_ne v expr vartable = 
  let sign_expr = sign_expr expr vartable in
  let v_signs   = find v vartable in 
  let new_signs = List.filter (fun x -> not(List.mem x sign_expr) ) v_signs
  in  add v new_signs vartable;;

let cmp_spread v cmp expr vartable = 
  
  match cmp with 
  |Eq -> cmp_eq v expr vartable
  |Ne -> cmp_ne v expr vartable
  |Gt -> cmp_gt v expr vartable
  |Lt -> cmp_lt v expr vartable
  |Ge -> cmp_ge v expr vartable
  |Le -> cmp_le v expr vartable;;

(*Fonction servant à "join" deux environnement de la façon suivante : 
Pour chaque variable du deuxieme environnement, si elle existe dans le premier environnement on concatène leur signe en enlevant les doublons
                                                sinon on ajoute les nouvelles variables avec leurs propres signes dans le premier environnement *)
let join_vartable  fst_table snd_table = 
  let rec aux l res =
    match l with 
    |[]     -> res 
    |(a,b)::tl -> begin match VarTable.mem a res with 
        |true -> let update_sign_a = (find a res) @ (find a snd_table) |> no_duplicates in  aux tl (add a update_sign_a res) 
        |false -> let new_sign_a = find a snd_table in aux tl (add a new_sign_a res) 
      end
  in
  aux (bindings snd_table) fst_table;;


(*Fonction de propagation de la condition, les cas gérés ici sont : 
   x > y (propagation pour les deux variables x et y)
   x > expr et expr > expr (propagation uniquement pour la variable x)
*)
let condition_spread (cond:cond) vartable = 
  let (expr1,cmp,expr2) = cond in 
  match expr1,expr2 with 
  |Var(v),Var(z) -> join_vartable (cmp_spread v cmp (Var(z)) vartable) (cmp_spread z (inverse_cmp cmp) (Var(v)) vartable)
  |Var(v),expr -> cmp_spread v cmp expr vartable
  |expr,Var(v) -> cmp_spread v (inverse_cmp cmp) expr vartable
  |_ -> vartable;;



(*Fonction gérant l'actualisation de l'environnement après le traitement d'un if/else :
  Si branche if est accessible de par la vérification au préalable de la condition, alors si l'environnement else est innaccessible on renvoie la branche if
                                                                                    sinon on renvoie leur union
  Si la branche if est innaccessible : Si la branche else est accessible alors on renvoie uniquement la branche else et sinon on renvoie l'environnement pré if/else *) 

let update_vartable if_table else_table vartable = 
  match is_empty if_table with
  | true -> if is_empty else_table then vartable else else_table
  | false ->  if  is_empty else_table then if_table else join_vartable if_table else_table;;

let are_equal v1 v2 = 
  let rec aux signv1 =
    match signv1 with
      [] -> true
    |(a,b)::tl -> begin match mem a v2 with 
        | true  -> if List.for_all (fun x -> List.mem x (find a v2)) b then aux tl else false 
        | false ->  false
      end
  in
  aux (bindings v1);;

            
(* Fonction servant à actualiser le signe d'une variable après l'utilisation de l'instruction Set *)
let sign_set v expr vartable = let signs_to_add = sign_expr expr vartable in add v signs_to_add vartable;;
  
(* Fonction principale servant à gérer toutes les instructions, pour le PRINT on renverra juste le même environnement *)
let rec sign_instr instr vartable =
  match instr with 
  |   Set(n,exp)                         -> sign_set n exp vartable
  |   Read(n)                            -> add n [Pos;Neg;Zero] vartable 
  |   If(cond,blc1,blc2)                 -> sign_if cond blc1 blc2 vartable
  |   While (cond,blc)                   -> sign_while cond blc vartable
  |   _                                  -> vartable
    

(*Fonction servant à actualiser l'environnement après un if/else de la façon suivante : 
Si on peut entrer dans le IF alors on fait une propagation de la condition dans l'environnement de base
Si on peut entrer dans le ELSE alors on fait une propagation de la condition inverse dans l'environnement de base
Finalement on join les deux branches selons les règles énnoncées dans update_vartable *)

and sign_if cond b1 b2 vartable =

  let inv_cond = inverse_cond cond in 
  let if_table = if verify_cond cond vartable then sign_block b1 (condition_spread cond vartable) else empty in
  let else_table = if verify_cond inv_cond vartable then sign_block b2 (condition_spread inv_cond vartable) else empty
  in update_vartable if_table else_table vartable


(*Fonction servant à actualiser l'environnement après une boucle while de la façon suivante : 
Si la condition est vérifiée alors on fait une propagation de la condition puis on sort de la boucle quand l'environnement précédent est égal à l'environnement actuel
sans oublier de propager la condition inverse sur ce nouvel environnement.
Cependant, si la condition condition inverse est vérifiée alors on fait une propagationd de cette même condition inverse sur l'environnement actuel et on saute les instructions
du bloc while

*)
and sign_while cond b vartable =  
  let inv_cond = inverse_cond cond in 
  let skip_while_table = if verify_cond inv_cond vartable then condition_spread inv_cond vartable else empty in
  let while_table = get_while_table cond b vartable in 

  join_vartable skip_while_table while_table

and get_while_table cond b vartable = 
  let while_table = if verify_cond cond vartable then sign_block b (condition_spread cond vartable) else empty in
  match is_empty while_table with 
  |true  -> let inv_cond = inverse_cond cond in (condition_spread inv_cond vartable)
  |false -> if are_equal vartable  while_table then vartable else get_while_table cond b (sign_block b while_table) (*boucle infini ou sinon on continue d executer *)

and sign_block b vartable = 
  let rec aux l res =
    match l with
    |[] -> res
    |(pos,instr)::tl -> aux tl (sign_instr instr res) in
  aux b vartable

