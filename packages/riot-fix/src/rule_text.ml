open Std

let rec syntax_node = fun node ->
  Syn.Ceibo.Red.SyntaxNode.children node |> List.map
    ~fn:(
      function
      | Syn.Ceibo.Red.Node child -> syntax_node child
      | Syn.Ceibo.Red.Token token -> Syn.Ceibo.Red.SyntaxToken.text token
    ) |> String.concat ""

let expression = fun expr -> Syn.Cst.Expression.syntax_node expr |> syntax_node

let pattern = fun pattern -> Syn.Cst.Pattern.syntax_node pattern |> syntax_node

let parenthesize = fun text -> "(" ^ text ^ ")"
