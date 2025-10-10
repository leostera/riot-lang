open Std

type format = Text | Json

let red_tree_to_json (red_root : (Syn.SyntaxKind.t, string) Syn.Ceibo.Red.syntax_node) =
  let kind_to_json kind =
    (* Convert SyntaxKind to string *)
    Data.Json.String
      (match kind with
       | Syn.SyntaxKind.SOURCE_FILE -> "SOURCE_FILE"
       | Syn.SyntaxKind.COMMENT -> "COMMENT"
       | Syn.SyntaxKind.WHITESPACE -> "WHITESPACE"
       | Syn.SyntaxKind.IDENT_EXPR -> "IDENT_EXPR"
       | Syn.SyntaxKind.TYPE_CONSTR -> "TYPE_CONSTR"
       | Syn.SyntaxKind.OPEN_STMT -> "OPEN_STMT"
       | Syn.SyntaxKind.PATH_EXPR -> "PATH_EXPR"
       | Syn.SyntaxKind.EXTERNAL_DECL -> "EXTERNAL_DECL"
       | _ -> "OTHER")
  in
  let text_to_json text = Data.Json.String text in
  Syn.Ceibo.Red.to_json ~kind_to_json ~text_to_json
    (Syn.Ceibo.Red.Node red_root)
