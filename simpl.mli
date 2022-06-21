val simpl_expr : Syntax.expr -> Syntax.expr
val simpl_op : Syntax.op -> Syntax.expr -> Syntax.expr -> Syntax.expr
val simpl_cond : Syntax.cond -> Syntax.cond
val verif_constantes : Syntax.cond -> bool
val const_value : Syntax.expr -> int
val verif_cond : Syntax.cond -> bool
val instrList_into_bloc :
  Syntax.instr list -> int -> (Syntax.position * Syntax.instr) list
val simpl_block : Syntax.block -> Syntax.instr list
val simpl_instr : Syntax.instr -> Syntax.instr list
val simpl_if :
  Syntax.cond -> Syntax.block -> Syntax.block -> Syntax.instr list
val simpl_while : Syntax.cond -> Syntax.block -> Syntax.instr list