val indentation : string
val print_indent : int -> unit
val comp_to_string : Syntax.comp -> string
val operand_to_string : Syntax.op -> string
val op_to_string : Syntax.op -> Syntax.expr -> Syntax.expr -> string
val exp_to_string : Syntax.expr -> string
val cond_to_string : Syntax.cond -> string
val print_block : Syntax.block -> int -> unit
val print_instr : Syntax.instr -> int -> unit