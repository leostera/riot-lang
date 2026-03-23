open Std

let format (result : Syn.Parser.parse_result) =
  let buffer = IO.Buffer.create 1024 in
  let root = Syn.Ceibo.Red.new_root result.tree in
  Syn.Ceibo.Red.SyntaxNode.preorder root (function
    | Syn.Ceibo.Red.Token token ->
        IO.Buffer.add_string buffer (Syn.Ceibo.Red.SyntaxToken.text token)
    | Syn.Ceibo.Red.Node _ ->
        ());
  IO.Buffer.contents buffer

let write ~writer result = IO.write_all writer ~buf:(format result)
