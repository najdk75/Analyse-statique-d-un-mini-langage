module Names :
  sig
    type elt = String.t
    type t = Set.Make(String).t
    val empty : t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : (elt -> unit) -> t -> unit
    val map : (elt -> elt) -> t -> t
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val filter : (elt -> bool) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val min_elt_opt : t -> elt option
    val max_elt : t -> elt
    val max_elt_opt : t -> elt option
    val choose : t -> elt
    val choose_opt : t -> elt option
    val split : elt -> t -> t * bool * t
    val find : elt -> t -> elt
    val find_opt : elt -> t -> elt option
    val find_first : (elt -> bool) -> t -> elt
    val find_first_opt : (elt -> bool) -> t -> elt option
    val find_last : (elt -> bool) -> t -> elt
    val find_last_opt : (elt -> bool) -> t -> elt option
    val of_list : elt list -> t
  end
val print_set_elt : string -> unit
val print_final_result : Names.t * Names.t * Names.t -> unit
val vars_exp : Syntax.expr -> Names.t * Names.t * 'a -> Names.t
val vars_cond :
  Syntax.expr * 'a * Syntax.expr ->
  Names.t * Names.t * 'b -> Names.t * Names.t * 'b
val filter_set_after_while :
  Names.t * Names.t * Names.t ->
  Names.t * Names.t * Names.t -> Names.t * Names.t * Names.t
val fill_res :
  Syntax.instr -> Names.t * Names.t * Names.t -> Names.t * Names.t * Names.t
val vars_blc :
  Syntax.block -> Names.t * Names.t * Names.t -> Names.t * Names.t * Names.t
val vars_while :
  Syntax.cond ->
  Syntax.block -> Names.t * Names.t * Names.t -> Names.t * Names.t * Names.t
val vars_if :
  Syntax.cond ->
  Syntax.block ->
  Syntax.block -> Names.t * Names.t * Names.t -> Names.t * Names.t * Names.t
val get_vars : Syntax.program -> unit