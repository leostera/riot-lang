open Std

let syntax_node = Syn.Ast.Node.text

let expression = Syn.Ast.Node.text

let pattern = Syn.Ast.Node.text

let parenthesize = fun text -> "(" ^ text ^ ")"
