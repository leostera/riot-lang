val check : Syn.Cst.expression -> Diagnostics.Diagnostic.t list

val check_parameter : Syn.Cst.parameter -> Diagnostics.Diagnostic.t list

val check_let_binding : Syn.Cst.LetBinding.t -> Diagnostics.Diagnostic.t list
