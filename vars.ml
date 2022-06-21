module Names = Set.Make(String);;
open Names;;
open Syntax;;

let print_set_elt s = 
Printf.printf "%s " s;;


let print_final_result env = 
let (a,b,c) = env in 
let (d,e,f) = (union (union b a ) c, b, c) in
  iter print_set_elt d; print_string("\n"); iter print_set_elt e; print_string("\n");;


let rec vars_exp (exp:expr) env = 
  let (init_vars,uninit_vars,while_vars) = env in 
  match exp with 
  |Num(x)           -> empty
  |Var(s)           -> begin match mem s init_vars with 
                      | true ->  empty
                      | false ->  add s uninit_vars
                      end
  |Op(_,exp1,exp2)  -> union (vars_exp exp1 env) (vars_exp exp2 env);;


let vars_cond cond env = 
  let (init_vars,uninit_vars,while_vars) = env in 
  let (exp1,comp,exp2) = cond in 
  let res = union (vars_exp exp1 env) (vars_exp exp2 env) in
  (init_vars, union uninit_vars res, while_vars);;

let filter_set_after_while prg_env while_env =

let (a,b,c) = prg_env in
let (d,e,f) = while_env in 
let new_init_vars = (union a (filter (fun s-> mem s a) e)) in

(new_init_vars, diff  (union b e) new_init_vars, union (union c f) d);;


let rec fill_res instr env  =
 
  let (init_vars,uninit_vars,while_vars) = env in 
  match instr with 
  |   Set(n,exp)                         -> (add n init_vars, union uninit_vars  (vars_exp exp env), while_vars)
  |   Read(n)                            -> (add n init_vars, uninit_vars, while_vars)
  |   Print(exp)                         -> (init_vars , union uninit_vars  (vars_exp exp env), while_vars)
  |   If(cond,blc1,blc2)                 -> vars_if cond blc1 blc2 env
  |   While(cond,blc)                    -> vars_while cond blc env

and vars_blc blc env =
  let rec aux blc new_env =
    match blc with 
    | [] -> new_env
    | (pos,instr)::t -> aux t (fill_res instr new_env) in
  aux blc env

and vars_while cond blc env = 
 
  let (a,b,c) = vars_cond cond env in

  let env_inside_while = vars_blc blc (empty,b,empty) in

  filter_set_after_while (a,b,c) env_inside_while
  
 and vars_if cond blc1 blc2 env = 
  
  let (a,b,c) = vars_cond cond env in
  
  
  let (d,e,f)  = vars_blc blc1 (a,b,c) in

  let (g,h,i) = vars_blc blc2 (a,b,c) in

  let inter_initvars = inter d g in (* élément initialisées dans les deux branches *)
  let union_diff_interinitvars = diff  (union d g) inter_initvars in
  let union_unitvars = union e h |> union union_diff_interinitvars |> union b in 
  
  (union a inter_initvars, diff union_unitvars a , union c f |> union i);;

let get_vars (p:program) : unit =
  let rec aux prog res =
    match prog with 
    |[]             -> print_final_result res
    |(pos,instr)::t -> aux t (fill_res instr res)

  in aux p (empty,empty,empty);;

