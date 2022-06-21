module VarTable :
  sig
    type key = String.t
    type 'a t = 'a Map.Make(String).t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val mem : key -> 'a t -> bool
    val add : key -> 'a -> 'a t -> 'a t
    val update : key -> ('a option -> 'a option) -> 'a t -> 'a t
    val singleton : key -> 'a -> 'a t
    val remove : key -> 'a t -> 'a t
    val merge :
      (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
    val union : (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val for_all : (key -> 'a -> bool) -> 'a t -> bool
    val exists : (key -> 'a -> bool) -> 'a t -> bool
    val filter : (key -> 'a -> bool) -> 'a t -> 'a t
    val filter_map : (key -> 'a -> 'b option) -> 'a t -> 'b t
    val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
    val cardinal : 'a t -> int
    val bindings : 'a t -> (key * 'a) list
    val min_binding : 'a t -> key * 'a
    val min_binding_opt : 'a t -> (key * 'a) option
    val max_binding : 'a t -> key * 'a
    val max_binding_opt : 'a t -> (key * 'a) option
    val choose : 'a t -> key * 'a
    val choose_opt : 'a t -> (key * 'a) option
    val split : key -> 'a t -> 'a t * 'a option * 'a t
    val find : key -> 'a t -> 'a
    val find_opt : key -> 'a t -> 'a option
    val find_first : (key -> bool) -> 'a t -> key * 'a
    val find_first_opt : (key -> bool) -> 'a t -> (key * 'a) option
    val find_last : (key -> bool) -> 'a t -> key * 'a
    val find_last_opt : (key -> bool) -> 'a t -> (key * 'a) option
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
    val to_seq : 'a t -> (key * 'a) Seq.t
    val to_rev_seq : 'a t -> (key * 'a) Seq.t
    val to_seq_from : key -> 'a t -> (key * 'a) Seq.t
    val add_seq : (key * 'a) Seq.t -> 'a t -> 'a t
    val of_seq : (key * 'a) Seq.t -> 'a t
  end
type sign = Neg | Zero | Pos | Error
val printlist : sign list -> unit
val print_bindings : (string * sign list) list -> unit
val print_sign : sign list VarTable.t -> unit
val isAdd : Syntax.op -> bool
val isSub : Syntax.op -> bool
val isMul : Syntax.op -> bool
val isDiv : Syntax.op -> bool
val isMod : Syntax.op -> bool
val no_duplicates : 'a list -> 'a list
val sign_const : int -> sign
val var_sign : VarTable.key -> 'a VarTable.t -> 'a
val sign_expr : Syntax.expr -> sign list VarTable.t -> sign list
val sign_op : sign -> sign -> Syntax.op -> sign list
val add_signs_l1 : sign -> sign list -> Syntax.op -> sign list
val update_sign_l1 : sign list -> sign list -> Syntax.op -> sign list
val inverse_cond : 'a * Syntax.comp * 'b -> 'a * Syntax.comp * 'b
val inverse_sign : sign -> sign
val inverse_combination : (sign * sign) list -> (sign * sign) list
val inverse_cmp : Syntax.comp -> Syntax.comp
val isError : Syntax.expr -> sign list VarTable.t -> bool
val rmv_err : sign list -> sign list
val in_s1s2 : 'a list -> 'b list -> 'a -> 'b -> bool
val check_combination : 'a list -> 'b list -> ('a * 'b) list -> bool
val verify_eq_ne :
  Syntax.expr -> Syntax.expr -> sign list VarTable.t -> Syntax.comp -> bool
val verify_bis :
  Syntax.expr ->
  Syntax.expr -> sign list VarTable.t -> 'a -> (sign * sign) list -> bool
val verify_cond :
  Syntax.expr * Syntax.comp * Syntax.expr -> sign list VarTable.t -> bool
val propagation_gt : sign list -> bool
val propagation_lt : sign list -> bool
val cmp_eq :
  VarTable.key -> Syntax.expr -> sign list VarTable.t -> sign list VarTable.t
val cmp_gt :
  VarTable.key -> Syntax.expr -> sign list VarTable.t -> sign list VarTable.t
val cmp_lt :
  VarTable.key -> Syntax.expr -> sign list VarTable.t -> sign list VarTable.t
val get_signs_ge_le :
  VarTable.key -> sign -> sign list VarTable.t -> sign list VarTable.t
val cmp_ge :
  VarTable.key -> Syntax.expr -> sign list VarTable.t -> sign list VarTable.t
val cmp_le :
  VarTable.key -> Syntax.expr -> sign list VarTable.t -> sign list VarTable.t
val cmp_ne :
  VarTable.key -> Syntax.expr -> sign list VarTable.t -> sign list VarTable.t
val cmp_spread :
  VarTable.key ->
  Syntax.comp -> Syntax.expr -> sign list VarTable.t -> sign list VarTable.t
val join_vartable :
  'a list VarTable.t -> 'a list VarTable.t -> 'a list VarTable.t
val condition_spread :
  Syntax.cond -> sign list VarTable.t -> sign list VarTable.t
val update_vartable :
  'a list VarTable.t ->
  'a list VarTable.t -> 'a list VarTable.t -> 'a list VarTable.t
val are_equal : 'a list VarTable.t -> 'a list VarTable.t -> bool
val sign_set :
  VarTable.key -> Syntax.expr -> sign list VarTable.t -> sign list VarTable.t
val sign_instr : Syntax.instr -> sign list VarTable.t -> sign list VarTable.t
val sign_if :
  Syntax.cond ->
  Syntax.block ->
  Syntax.block -> sign list VarTable.t -> sign list VarTable.t
val sign_while :
  Syntax.cond -> Syntax.block -> sign list VarTable.t -> sign list VarTable.t
val get_while_table :
  Syntax.cond -> Syntax.block -> sign list VarTable.t -> sign list VarTable.t
val sign_block : Syntax.block -> sign list VarTable.t -> sign list VarTable.t