val read_polish : string -> Syntax.program
val print_polish : Syntax.program -> unit
val eval_polish : Syntax.program -> unit
val simpl_polish : Syntax.program -> Syntax.program
val vars_polish : Syntax.program -> unit
val usage : unit -> unit
val main : unit -> unit