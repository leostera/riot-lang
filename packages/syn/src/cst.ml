open Std
open Std.Collections

type syntax_node = (Syntax_kind.t, string) Ceibo.Red.syntax_node
type syntax_token = (Syntax_kind.t, string) Ceibo.Red.syntax_token
type green_node = (Syntax_kind.t, string) Ceibo.Green.node

let is_trivia kind =
  let open Syntax_kind in
  kind = WHITESPACE || kind = COMMENT || kind = DOCSTRING

module Token = struct
  type t = { syntax_token : syntax_token }

  let syntax_token token = token.syntax_token
  let text token = Ceibo.Red.SyntaxToken.text token.syntax_token
  let span token = Ceibo.Red.SyntaxToken.span token.syntax_token
end

module TypeVariable = struct
  type t = {
    syntax_node : syntax_node;
    name_token : Token.t;
  }

  let syntax_node type_variable = type_variable.syntax_node
  let name_token type_variable = type_variable.name_token

  let text type_variable =
    Ceibo.Red.SyntaxNode.children type_variable.syntax_node
    |> Array.to_list
    |> List.filter_map (function
         | Ceibo.Red.Token tok
           when not (is_trivia (Ceibo.Red.SyntaxToken.kind tok)) ->
             Some (Ceibo.Red.SyntaxToken.text tok)
         | _ -> None)
    |> String.concat ""

  let name type_variable = Token.text type_variable.name_token
end

module TypeParameter = struct
  type t = {
    syntax_node : syntax_node;
    type_variable : TypeVariable.t option;
  }

  let syntax_node type_param = type_param.syntax_node
  let type_variable type_param = type_param.type_variable
end

module ModulePath = struct
  type t = {
    syntax_node : syntax_node;
    segments : Token.t list;
  }

  let syntax_node path = path.syntax_node
  let segments path = path.segments
  let last_segment path =
    match List.rev path.segments with
    | segment :: _ -> Some segment
    | [] -> None

  let name path =
    match last_segment path with
    | Some segment -> Some (Token.text segment)
    | None -> None
end

module TypeDeclaration = struct
  type t = {
    syntax_node : syntax_node;
    type_name : ModulePath.t;
    type_params : TypeParameter.t list;
  }

  let syntax_node decl = decl.syntax_node
  let type_name decl = decl.type_name
  let type_params decl = decl.type_params

  let name_token decl =
    match ModulePath.last_segment decl.type_name with
    | Some token -> token
    | None -> panic "TypeDeclaration.name_token: missing type name token"
end

module Item = struct
  type t =
    | TypeDeclaration of TypeDeclaration.t
    | Unknown of syntax_node

  let syntax_node = function
    | TypeDeclaration decl -> TypeDeclaration.syntax_node decl
    | Unknown node -> node
end

module SourceFile = struct
  type t = {
    syntax_node : syntax_node;
    items : Item.t list;
  }

  let syntax_node source_file = source_file.syntax_node
  let items source_file = source_file.items
end

type source_file = SourceFile.t

let token token = Token.{ syntax_token = token }

let direct_non_trivia_nodes node =
  Ceibo.Red.SyntaxNode.children node
  |> Array.to_list
  |> List.filter_map (function
       | Ceibo.Red.Node child -> Some child
       | Ceibo.Red.Token tok
         when is_trivia (Ceibo.Red.SyntaxToken.kind tok) ->
           None
       | Ceibo.Red.Token _ -> None)

let direct_non_trivia_tokens node =
  Ceibo.Red.SyntaxNode.children node
  |> Array.to_list
  |> List.filter_map (function
       | Ceibo.Red.Token tok
         when not (is_trivia (Ceibo.Red.SyntaxToken.kind tok)) ->
           Some tok
       | _ -> None)

let type_variable_from_node node =
  match List.rev (direct_non_trivia_tokens node) with
  | name_tok :: _ ->
      Some TypeVariable.{ syntax_node = node; name_token = token name_tok }
  | [] -> None

let type_parameter_from_node node =
  let type_var =
    direct_non_trivia_nodes node
    |> List.find_opt (fun child ->
           Ceibo.Red.SyntaxNode.kind child = Syntax_kind.TYPE_VAR)
    |> function
    | Some child -> type_variable_from_node child
    | None -> None
  in
  TypeParameter.{ syntax_node = node; type_variable = type_var }

let module_path_from_node node =
  let parts = direct_non_trivia_tokens node |> List.map token in
  ModulePath.{ syntax_node = node; segments = parts }

let ident_path_from_node node =
  let parts =
    match direct_non_trivia_tokens node with
    | first :: _ -> [ token first ]
    | [] -> []
  in
  ModulePath.{ syntax_node = node; segments = parts }

let type_declaration_name_path node =
  let is_name_node child =
    let kind = Ceibo.Red.SyntaxNode.kind child in
    kind = Syntax_kind.IDENT_EXPR || kind = Syntax_kind.MODULE_PATH
  in
  direct_non_trivia_nodes node
  |> List.find_opt is_name_node
  |> Option.map (fun child ->
         match Ceibo.Red.SyntaxNode.kind child with
         | Syntax_kind.MODULE_PATH -> module_path_from_node child
         | Syntax_kind.IDENT_EXPR -> ident_path_from_node child
         | _ -> ModulePath.{ syntax_node = child; segments = [] })

let type_declaration_from_node node =
  let params =
    direct_non_trivia_nodes node
    |> List.filter (fun child ->
           Ceibo.Red.SyntaxNode.kind child = Syntax_kind.TYPE_PARAM)
    |> List.map type_parameter_from_node
  in
  match type_declaration_name_path node with
  | Some path -> (
      match ModulePath.last_segment path with
      | Some _ ->
          Some TypeDeclaration.{ syntax_node = node; type_name = path; type_params = params }
      | None -> None)
  | None -> None

let item_from_node node =
  match Ceibo.Red.SyntaxNode.kind node with
  | Syntax_kind.TYPE_DECL -> (
      match type_declaration_from_node node with
      | Some decl -> Item.TypeDeclaration decl
      | None -> Item.Unknown node)
  | _ -> Item.Unknown node

let of_green_tree tree =
  let root = Ceibo.Red.new_root tree in
  let file_items =
    direct_non_trivia_nodes root
    |> List.map item_from_node
  in
  SourceFile.{ syntax_node = root; items = file_items }

let syntax_node_of_source_file source_file = SourceFile.syntax_node source_file
