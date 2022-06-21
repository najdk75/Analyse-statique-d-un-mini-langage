val string_int_list_of_char : string -> char list
val count_indent : char list -> int
val aux_is_op : string -> Syntax.op option
val aux_is_comp : string -> Syntax.comp option
val aux_is_num : string -> int option
val aux_is_var : string -> Syntax.expr option
val is_set : string -> bool
val aux_give_var : string -> string option
val aux_give_expr_OP : string list -> Syntax.expr
val aux_instr_of_SET : string list -> Syntax.instr
val aux_is_not_comp : string list -> string list
val list_filter : string list -> string list -> string list
val make_cond_of : string list -> Syntax.cond
val is_name : string -> bool
val string_to_list_no_indent : string -> string list
val affichelist : ('a * 'b * string list) list -> unit
val block_elements :
  int ->
  (int * 'a * string list) list -> string -> (int * 'a * string list) list
val get_2nd_blok :
  int -> (int * 'a * String.t list) list -> (int * 'a * String.t list) list
val update_main_list :
  int -> ('a * 'b * string list) list -> ('a * 'b * string list) list
val rmv_instr : ('a * 'b * string list) list -> ('a * 'b * string list) list
val read_instr :
  Syntax.name list ->
  (int * Syntax.position * string list) list -> Syntax.instr
val make_block_of :
  (int * Syntax.position * Syntax.name list) list ->
  (int * Syntax.position * string list) list -> Syntax.block
val makewhile : (int * Syntax.position * string list) list -> Syntax.instr
val makeif : (int * Syntax.position * string list) list -> Syntax.instr
val makeprogram :
  (int * Syntax.position * Syntax.name list) list ->
  (Syntax.position * Syntax.instr) list
val read_and_extract_all : string -> (int * int * string list) list