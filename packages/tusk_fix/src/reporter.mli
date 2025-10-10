open Std

type format = Text | Json

val red_tree_to_json :
  (Syn.SyntaxKind.t, string) Syn.Ceibo.Red.syntax_node -> Data.Json.t
