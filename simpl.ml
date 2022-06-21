open Syntax ;;
open Print ;;
open Read ;;
open Eval ;;


(*************************   Simplifier une expression   ********************)
let rec simpl_expr (exp: expr): expr =
  match exp with 
  |Num(x) -> Num (x) 
  |Var(s) -> Var(s)
  |Op(oper,expr1,expr2) -> 
      match oper,expr1 ,expr2 with 
      |Add ,Num(0),expr2                               -> simpl_expr (expr2)
      |Add ,expr1 ,Num(0)                              -> simpl_expr (expr1)
      |Mul ,Num(1),expr2                               -> simpl_expr (expr2)
      |Mul ,expr1 ,Num(1)                              -> simpl_expr (expr1)
      |Mod ,_     ,Num(0) | Div ,_, Num(0)             -> Printf.printf ("Division par zéro interdite.\n"); exit(1)
      |oper, Num(a), Num(b)                            -> simpl_op oper expr1 expr2
      |oper, Op(op,e1,e2) ,Num(a)                      -> simpl_expr (Op(oper, (simpl_op op e1 e2), expr2))
      |oper, Num(a) , Op(op,e1,e2)                     -> simpl_expr (Op(oper, expr1 ,(simpl_op op e1 e2)))
      |_                                               -> Op(oper, simpl_expr expr1 , simpl_expr expr2)     

      (*Simplification d'une opération arithmétique  *)
    and simpl_op oper expr1 expr2 = 
    match oper,expr1 ,expr2 with 
     
    |Add ,Num(x),Num(y)                                                        -> Num(( x ) + ( y ) )
    |Sub ,Num(x),Num(y)                                                        -> Num(( x ) - ( y ))
    |Mul ,Num(x),Num(y)                                                        -> Num((x ) * ( y ))   
    |Div ,Num(x),Num(y)                                                        -> Num(( x ) / ( y ) )
    |Mod ,Num(x),Num(y)                                                        -> Num((x ) mod ( y ))    
    |Div ,Num(0),_ | Mul ,Num(0),_ | Mul ,_, Num(0)                            -> Num(0)
    | _                                                                        -> Op(oper, simpl_expr expr1 , simpl_expr expr2)   

;;

(*************************   Simplifier une condition   ********************)
let simpl_cond (cond: cond): cond = 
let (a,b,c) = cond in 
( simpl_expr a, b, simpl_expr c)
;;

(**********************    fonction pour vérifier si une condition compare deux constantes   **********************)

let verif_constantes (cond : cond ): bool = 
  let (a,b,c)= simpl_cond cond in 
  match a,c with 
  | Num(x),Num(y)  ->true 

  | _,_            ->false 
;; 
(*****************************  Fonction pour retourner l'entier de la constante ***********************************)
let const_value (expr : expr) : int = 
  int_of_string(exp_to_string (expr)) 

(**********************    fonction pour vérifier si une condition est satisfaite   **********************)


let verif_cond (cond: cond ) : bool = 
  (* vérifier si  notre condition (après simplification) compare que deux constantes on peut regarder si notre condition est satisfaite ou non *)
  if verif_constantes (simpl_cond cond)  then 
    let (exp1,cmp,exp2) = (simpl_cond cond) in  
    match cmp with
    | Eq          ->  const_value exp1 =  const_value exp2
    | Ne          ->  const_value exp1 <> const_value exp2
    | Lt          ->  const_value exp1 <  const_value exp2
    | Le          ->  const_value exp1 <= const_value exp2
    | Gt          ->  const_value exp1 >  const_value exp2
    | Ge          ->  const_value exp1 >= const_value exp2
           
  else false                                                              

(*************************  fonction qui convertit une liste d'instruction en un bloc (NB: je ne gére pas bien la pos de l'instr)  ********************)

let  instrList_into_bloc (l:instr list) (cp : int ) = 
  let rec aux l cp (acc: block) = 
    match l with 
    |[] -> acc
    | a::b -> aux b (cp+1) ((cp,a)::acc)
  in List.rev (aux l cp [])


(*************************   Simplifier un bloc  ****************************)

let rec simpl_block (block : block): instr list = 
  let rec aux block acc =
    match block with
    |[]                  -> acc
    |(p,instr)::tl_blc   ->  aux tl_blc ((simpl_instr instr)@acc)
    
  in List.rev (aux block [] )
    
(*************************   Simplifier une expression   ********************)
and simpl_instr (instr: instr): instr list =
  let aux instr acc = 
    match instr with 
    |   Set(n,exp)                         -> (Set(n,simpl_expr exp))::acc
    |   Read(n)                            -> (Read(n))::acc
    |   Print(expr)                        -> ((Print (simpl_expr expr))::acc)
    |   If((cond),blc1,blc2)               ->
    
           (* Si on compare pas entre  deux constantes pas de simplification (j'ignore pour le moment le numéro de ligne dans les sous-block)*)
                                           if verif_constantes(simpl_cond cond) 
                                           then 
                                            ((simpl_if cond blc1 blc2)@acc)
                                           else 
                                           (* Si on compare que deux constantes on fait la simplification *)
                                           If((simpl_cond cond),instrList_into_bloc (simpl_block blc1) 0 ,instrList_into_bloc (simpl_block blc2 ) 1)::acc

 
   |   While ((cond),blc)                  ->  
   (* Si on compare pas entre  deux constantes pas de simplification (j'ignore pour le moment le numéro de ligne dans les sous-block)*)
                                             if verif_constantes(simpl_cond cond)
                                            then 
                                            ((simpl_while cond blc)@acc) 
                                            else 
                                             (* Si on compare que deux constantes on fait la simplification *)
                                            While(simpl_cond cond , instrList_into_bloc (simpl_block blc) 0)::acc 
        
     
  in List.rev (aux instr [])


(*************************   Simplifier une instruction if   ****************************)

and simpl_if  (cond:cond)  (blc1:block) (blc2:block) : instr list = 
  if  verif_cond  cond then simpl_block blc1  else  simpl_block blc2 

(*************************   Simplifier une instruction while  ****************************)

and simpl_while  (cond:cond)  (blc:block) : instr list  = 
if  verif_cond cond then  [(While(simpl_cond cond,instrList_into_bloc (simpl_block blc) 0))] 
  else []
  
;;



