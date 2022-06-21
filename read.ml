open Syntax ;;


(*fonction qui prend un string et qui le convertit en liste de char *)

let string_int_list_of_char (s : string) : char list =
  let rec exp (i: int) (l: char list) =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) [];;

(* fonction qui calcule l'indentation*)

let count_indent (line : char list): int  =
  let rec aux line nb_indent =
    match line with
    | a::tl when a = ' ' -> aux tl (nb_indent+1)
    | _ -> if (nb_indent mod 2 <> 0) then failwith " error" else nb_indent/2
  in aux line 0;;
  
(*fonction qui vérifie qu'un string est un opérateur arithmètique*)

let aux_is_op (s: string) : (op option) =
  match s with 
  |"+"-> Some (Add)
  |"-"-> Some (Sub)
  |"*"-> Some (Mul)
  |"/"-> Some(Div)
  |"%"-> Some(Mod)
  |_-> None (*sinon on renvoie None*)
    
(* tester si un mot est Opérateurs de comparaisons *)

let aux_is_comp (s: string) : (comp option) =
  match s with 
  |"="-> Some (Eq)(* = *)
  |"<>"-> Some (Ne) (* Not equal, <> *)
  |"<"-> Some (Lt) (* Less than, < *)
  |"<="-> Some(Le) (* Less or equal, <= *)
  |">"-> Some(Gt) (* Greater than, > *)
  |">="-> Some(Ge)(* Greater or equal, >= *)
  |_-> None (*sinon on renvoie None*)
  
  
            
(* fonction qui teste si le mot est un nombre :int*)

let aux_is_num (s:string) : (int option)=
  if (aux_is_op s = None) then (* si le mot n'est pas un opérateur arithmétique*)
     (* renvoie un None si le mot n'est un un int et Some(x:int) sinon *)
    (int_of_string_opt s) 
    (*en cas de Failure "int_of_string"*)
  else None   
    


 (* tfonctin qui teste si le mot est un nom de variable *) 

let aux_is_var (s:string) : (expr option)= 
  (*si le mot n'est pas un opérateur ni un nombre entier il doit être un nom de variable *)
  if (aux_is_op s=None) && (aux_is_num s= None) && (aux_is_comp s = None) then let name= s in 
    let a = Var(name) 
    in Some(a)
      
  else None 
        

 (*focntion qui vérifie si on est dans le cas d'une affectation *)   
let is_set (s:string) : bool =
  (String.equal s (":=" ))

let aux_give_var (s:string) : (string option) = 
  if (aux_is_op s=None) && (aux_is_num s= None) && (aux_is_comp s = None) then Some(s) 
  else None
    
  
(* cette fonction prends la liste de mots et la convertit en expr*)
let rec aux_give_expr_OP (l:string list ): expr =
  match l with 
  |[]-> failwith "not polish syntax ligne 79 read.ml" 
  |a::b -> match aux_is_num a with 
    |Some (x) ->  Num (x) 
    |None -> match aux_is_var a with  (*si c'est un var*)
      |Some(x)->  x
                     (* le cas d'une expression arithmétique*)
      |None-> (*faire une fonction auxiliaire pour le calcul de l'expression arithmétique*) 
          match aux_is_op a with 
          |None -> failwith" not a polish syntax ligne 87 read.ml"
          |Some(x)-> Op(x,aux_give_expr_OP b , aux_give_expr_OP (List.tl b))




(*fonction qui prend une liste de string et la convertit en affectation "SET"*)       
let aux_instr_of_SET ( l:string list) : instr = 
  match l with 
  |[]-> failwith "not polish syntax cause ligne 96 read.ml" 
  |a::b -> let name =aux_give_var a in 
      match  name  with 
      | Some(x)-> 
          Set (x,aux_give_expr_OP (List.tl b))
      |None -> failwith "Ceci n'est pas une affectation!"


  (***************** Fonctions auxiliaires pour le traitement de la condition de if *************)


 (* une condition = expr * op* expr *)

(** retourne la partie  expr qui vient avant le comp*)
let  aux_is_not_comp (l:string list )  : string list = 
 
  let rec without_comp (l:string list)  (acc: string list) : string list  = 
    match l with 
    |[]-> acc
    |a::b -> match aux_is_comp a with 
      |None -> without_comp b (a::acc)
      | Some (x) -> acc 
  in List.rev (without_comp l [])

 (*fonction qui prend deux listes a1 et a2 et retourne la liste a1 sans les elements de la liste a2*)
 (*elle retourne la partie comp*expr d'un if *)
 let  list_filter (a1: string list ) (a2: string list ) : string list  = 
  let rec aux  (a1: string list )  (acc: string list) (cp: int)  : string list  = 
    
    
    let l = List.length a2 in 
    match a1 with 
    | [] -> acc
    | c::d ->   if cp>=l then aux d (c::acc) (cp) else aux d acc (cp+1)
  in List.rev (aux a1 [] 0 )
            


 (*Cette fonction crée une condition à partir d'une ligne ( la condition qui vient après le while ou le if ) *)
let make_cond_of (ins: string list ): cond = 
  if ins = [] then failwith "ceci n'est pas une condition"
  else 
    let expr_1 = (aux_is_not_comp ins) in  (*on récupére la liste de la première expr*)
    let reste_cond = (list_filter ins expr_1 ) in (*on récupére le reste de la condition (comp*expr) *)
    let comp = List.hd reste_cond in  (*on récupére le comparateur *)

    match aux_is_comp comp with 
    | None-> failwith "this is not a condition"
    | Some (x) -> 
        let expr_2 = List.tl reste_cond  in (*on récupére la deuxièmme expr *)

        ((aux_give_expr_OP expr_1),x,aux_give_expr_OP(expr_2));;
 
          

let is_name (name:string):bool = 
match aux_is_var (name) with 
   |None -> false 
   |Some (x) -> true;;


(* Fonction retournant un tableau de string à partir d'un string en enlevant tous les espaces et mots vides
exemple : "__While if n > 0" retournera "["While";"if";"n";">";"0"]" *)

let string_to_list_no_indent (x:string) = String.split_on_char ' ' x |> List.filter (fun s -> s <> "");;



let rec affichelist l =
  match l with 
  |[] -> ()
  |(a,b,c)::t ->  Printf.printf ("%s;\t") (String.concat "" c); affichelist t;;


(* Fonction servant à lister les éléments appartenant à un bloc while ou if avec les contraintes suivantes :
  0) le tag représentera si on cherche les éléments d'un bloc if ou d'un bloc while, si on gère le bloc if, cette fonction renverra le premier bloc du if
  1) on ne récupère que les éléments d'intentation i+1 
  2) on n'ajoute jamais le second bloc du if dans cette fonction (correspondant au else)                                         
  3) les éléments d'indentation supérieure à i + 1 sont des éléments appartenant à une condition imbriquée qui seront eux mêmes gérés par cette fonction récursivement
  4) des qu'on croise une instruction avec une indentation égale à celle du if ou du while de départ, on retourne les élements.

  pour le programme ci dessous, la fonction nous renverra donc (en ométtant les indentations et les positions) : 
  [Print n, if n = 1, n := + n 1]. et si on appelait cette fonction sur le if on aurait seulement [n := + 1 2];;

  While n > 0 
    PRINT n
    if n = 1
      n := + 1 2
    else 
      n := 2
    n := + n 1
  PRINT n
*)
let block_elements (indent:int) mainlist (tag:string) = 
  
  let rec aux  list res =
    match list with
    | []        ->  res
    |(a,b,c)::t -> match a with
      | x when x = indent + 1 -> begin match tag with 
          |"WHILE" -> if List.hd c <> "ELSE"then aux t ((a,b,c)::res) else aux t res
          |"IF"    -> if List.hd c <> "ELSE"then aux t ((a,b,c)::res) else res 
          | _      -> res
        end
      | x when x > indent + 1 ->  aux t res 
      | x when x = indent     -> res
      | _                     ->  res
  in  List.rev(aux mainlist []);;


(* Fonction retournant la liste des éléments du deuxieme bloc de if, ignore les éléments du premier bloc puis renvoie tous les éléments d'indentation i+1 à partir du else *)
let get_2nd_blok i list =

  let rec aux l res =
    match l with
    | [] ->  res
    |(indent,pos,str::r)::t ->  if indent > i then aux t res else if indent = i && String.equal str "ELSE"  
        then block_elements i t "IF" else [] 
    |_ -> res
                    
  in aux list [];;

  
  (* Fonction servant à supprimer i éléments de la mainlist de la façon suivante : 
  si on obtient un bloc de longueur i dans un IF ou un WHILE, alors on supprime les i premiers éléments de la mainlist
  si on tombe sur un if ou un while alors on retourne la mainlist actualisée ces instructions devront être gérées récursivement. *)

let rec update_main_list i mainlist = 
  match mainlist with
  |[] -> []
  |(_,_,instr)::tl_mainlist -> match List.hd instr with 
    | "WHILE" | "IF" -> mainlist
    | _ ->  if i == 1 then tl_mainlist else update_main_list (i-1) tl_mainlist;;
  
  
  
  let rec rmv_instr mainlist =
  match mainlist with
  |[] -> []
  |(_,_,instr)::tl_mainlist -> match List.hd instr with 
                            |"WHILE" -> mainlist
                            |_       -> rmv_instr tl_mainlist;;


  (*Fonction lisant une instruction puis effectue les opérations nécessaires selon sa nature*)

let  rec read_instr l main_list = 

  match l with 
  |[]   -> failwith "empty line" (*si la liste des mots est vide*)
  |a::b -> match a with
        
    |""-> failwith "ceci est un blanc" 
    |"READ"  -> let name = List.hd (b) in if is_name name then Read(name) else failwith "Error in readinstr, reading something different than a variable" 
    |"PRINT" -> Print (aux_give_expr_OP b) 
    |"WHILE" ->  makewhile (rmv_instr main_list)
    |"IF"    ->  makeif main_list
      
    | _  -> aux_instr_of_SET (l)
and 
  make_block_of elt_to_add main_list  = (*ajoute les éléments dans les blocs du if ou du while*)
  let rec aux l block_res  =
   

    match l with 
    |[] -> block_res
    |(indent,pos,str)::tl -> aux tl ((pos,read_instr str main_list)::block_res)  
                          
                          
  in List.rev(aux elt_to_add [] )
and
  makewhile main_list  = 
      

  match main_list with
  |(indent,pos,str::r)::tl_mainlist ->
  
      let whileblock = block_elements indent tl_mainlist "WHILE" in (*éléments du bloc while*)
      let nb_elt_to_rm = List.length whileblock in 
   
      While(make_cond_of r, make_block_of whileblock (update_main_list nb_elt_to_rm tl_mainlist )) 
  | _ -> failwith "pas une structure while"

and makeif main_list = 
  match main_list with
  |(indent,pos,str::r)::tl_mainlist ->
      let elt_block1 = block_elements indent tl_mainlist "IF" in (*element du bloc1 *)
      let elt_block2 = get_2nd_blok indent tl_mainlist in        (*éléments du bloc2 *)
    

      
      If(make_cond_of r,
         make_block_of elt_block1 (update_main_list (List.length elt_block1) tl_mainlist),            
         make_block_of elt_block2 ((update_main_list (List.length elt_block2) tl_mainlist))) 
  
  
  | _ -> failwith "pas une structure if";;
  

(* Fonction principale servant à parcourir une (int * int * string list) list pour en faire un programme
 - ne regarde que les éléments d'indentation 0, les autres seront gérés récursivement par les blocs IF et WHILE
 - ignore aussi les lignes commençant par ELSE et COMMENT *)
let makeprogram main_list =
  let rec aux current_main_list prog =
    match current_main_list with
    |[] -> prog 
    |(indent,pos,str)::tl_mainlist -> if indent > 0 || String.equal (List.hd str) "ELSE" || String.equal (List.hd str) "COMMENT"
        then aux tl_mainlist prog  else aux tl_mainlist ((pos,read_instr str current_main_list)::prog) in
  List.rev(aux main_list []);;






  (*read_and_extract_all: lit et extrait toutes les lignes en couplant 
                        (num,ligne) et renvoie (int*string) list 
                        -si le fichier n'a pas la bonne extention elle renvoie une exception
 *)
let read_and_extract_all  (filename: string) : (int*int*(string)list) list = (*attention faut donner le bon chemin pour le fichier : "exemples/fichier.p"*)

(*on gére le cas ou ce n'est pas la bonne extention du fichier : *)
let length = String.length filename in 

let extention_of_file= String.sub filename (length-2) 2 in 

if (String.equal (extention_of_file) (".p"))=false  then raise (Failure "file is not a polish program") else 

(*in_channel: stdin (descripteur)*) 
  let in_channel = open_in filename in 

  (*on introduit le type option pour voir si on arrive à lire une ligne ou pas *)
  let try_read () =
    try Some (input_line in_channel) with End_of_file -> None in 

  (*on fait appel à une fonction auxiliaire pour le remplissange de notre liste ligne par ligne*)
  let rec read_line acc i  =
    match try_read () with 

    (*lecture réussie d'une ligne -> on l'ajoute à notre liste*)
    | Some line -> 
    if List.filter (fun s -> s <> "")  (String.split_on_char ' ' line ) = [] then 
    read_line ( acc ) (i)  (*ignorer les lignes vides*)
    else 
    read_line (((count_indent(string_int_list_of_char line)),i,string_to_list_no_indent line) :: acc ) (i+1)

    (*si on arrive à la fin du fichier on ferme le descripteur et on retourne la liste inversée (car le  :: ajoute en tête de liste) *)
    | None -> close_in in_channel; List.rev acc in 
  read_line [] 0;;
