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

module LetBinding = struct
  type t = {
    syntax_node : syntax_node;
    binding_name : Token.t;
    parameters : syntax_node list;
    value_syntax_node : syntax_node;
    is_recursive : bool;
  }

  let syntax_node binding = binding.syntax_node
  let binding_name_token binding = binding.binding_name
  let name binding = Token.text binding.binding_name
  let parameters binding = binding.parameters
  let value_syntax_node binding = binding.value_syntax_node
  let is_recursive binding = binding.is_recursive

  let is_function binding =
    List.length binding.parameters > 0
    ||
    match Ceibo.Red.SyntaxNode.kind binding.value_syntax_node with
    | Syntax_kind.FUN_EXPR | Syntax_kind.FUNCTION_EXPR -> true
    | _ -> false
end

module ModuleDeclaration = struct
  type t = {
    syntax_node : syntax_node;
    module_name : Token.t;
  }

  let syntax_node decl = decl.syntax_node
  let module_name_token decl = decl.module_name
  let name decl = Token.text decl.module_name
end

module ModuleTypeDeclaration = struct
  type t = {
    syntax_node : syntax_node;
    module_type_name : Token.t;
  }

  let syntax_node decl = decl.syntax_node
  let module_type_name_token decl = decl.module_type_name
  let name decl = Token.text decl.module_type_name
end

module Item = struct
  type t =
    | TypeDeclaration of TypeDeclaration.t
    | LetBinding of LetBinding.t
    | ModuleDeclaration of ModuleDeclaration.t
    | ModuleTypeDeclaration of ModuleTypeDeclaration.t
    | Unknown of syntax_node

  let syntax_node = function
    | TypeDeclaration decl -> TypeDeclaration.syntax_node decl
    | LetBinding binding -> LetBinding.syntax_node binding
    | ModuleDeclaration decl -> ModuleDeclaration.syntax_node decl
    | ModuleTypeDeclaration decl -> ModuleTypeDeclaration.syntax_node decl
    | Unknown node -> node
end

module SourceFile = struct
  type t = {
    syntax_node : syntax_node;
    items : Item.t list;
    let_bindings : LetBinding.t list;
  }

  let syntax_node source_file = source_file.syntax_node
  let items source_file = source_file.items
  let let_bindings source_file = source_file.let_bindings
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

let name_token_from_ident_pattern node =
  match direct_non_trivia_tokens node with
  | first :: _ -> Some (token first)
  | [] -> None

let is_parameter_like_kind = function
  | Syntax_kind.IDENT_PATTERN
  | Syntax_kind.WILDCARD_PATTERN
  | Syntax_kind.LITERAL_PATTERN
  | Syntax_kind.CONSTRUCTOR_PATTERN
  | Syntax_kind.TUPLE_PATTERN
  | Syntax_kind.LIST_PATTERN
  | Syntax_kind.ARRAY_PATTERN
  | Syntax_kind.CONS_PATTERN
  | Syntax_kind.RECORD_PATTERN
  | Syntax_kind.OR_PATTERN
  | Syntax_kind.AS_PATTERN
  | Syntax_kind.RANGE_PATTERN
  | Syntax_kind.TYPED_PATTERN
  | Syntax_kind.LAZY_PATTERN
  | Syntax_kind.EXCEPTION_PATTERN
  | Syntax_kind.PAREN_PATTERN
  | Syntax_kind.POLY_VARIANT_PATTERN
  | Syntax_kind.POLY_VARIANT_TYPE_PATTERN
  | Syntax_kind.LOCAL_OPEN_PATTERN
  | Syntax_kind.OPERATOR_PATTERN
  | Syntax_kind.FIRST_CLASS_MODULE_PATTERN
  | Syntax_kind.LABELED_PARAM
  | Syntax_kind.OPTIONAL_PARAM
  | Syntax_kind.OPTIONAL_PARAM_DEFAULT ->
      true
  | _ -> false

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

let let_binding_from_node ~is_recursive_binding node =
  match direct_non_trivia_nodes node with
  | name_node :: rest -> (
      match List.rev rest with
      | value_node :: rev_params
        when Ceibo.Red.SyntaxNode.kind name_node = Syntax_kind.IDENT_PATTERN ->
          name_token_from_ident_pattern name_node
          |> Option.map (fun binding_name ->
                 LetBinding.
                   {
                     syntax_node = node;
                     binding_name;
                     parameters = List.rev rev_params;
                     value_syntax_node = value_node;
                     is_recursive = is_recursive_binding;
                   })
      | _ -> None)
  | [] -> None

let let_expression_binding_from_node ~is_recursive_binding node =
  let rec find_name_node = function
    | [] -> None
    | child :: rest ->
        if Ceibo.Red.SyntaxNode.kind child = Syntax_kind.IDENT_PATTERN then
          Some (child, rest)
        else
          find_name_node rest
  in
  let rec split_parameters params = function
    | child :: rest when is_parameter_like_kind (Ceibo.Red.SyntaxNode.kind child)
      ->
        split_parameters (child :: params) rest
    | child :: rest when Ceibo.Red.SyntaxNode.kind child = Syntax_kind.TYPE_CONSTRAINT
      ->
        split_parameters params rest
    | child :: _ -> Some (List.rev params, child)
    | [] -> None
  in
  match find_name_node (direct_non_trivia_nodes node) with
  | Some (name_node, rest) -> (
      match name_token_from_ident_pattern name_node, split_parameters [] rest with
      | Some binding_name, Some (param_nodes, bound_value_node) ->
          Some LetBinding.
                 {
                   syntax_node = node;
                   binding_name = binding_name;
                   parameters = param_nodes;
                   value_syntax_node = bound_value_node;
                   is_recursive = is_recursive_binding;
                 }
      | _ -> None)
  | None -> None

let module_declaration_from_node node =
  match direct_non_trivia_tokens node with
  | _module_kw :: module_name :: _ ->
      Some ModuleDeclaration.{ syntax_node = node; module_name = token module_name }
  | _ -> None

let module_type_declaration_from_node node =
  match direct_non_trivia_tokens node with
  | _module_kw :: _type_kw :: module_type_name :: _ ->
      Some ModuleTypeDeclaration.
             { syntax_node = node; module_type_name = token module_type_name }
  | _ -> None

let rec collect_let_bindings node =
  let bindings_here =
    match Ceibo.Red.SyntaxNode.kind node with
    | Syntax_kind.LET_BINDING ->
        Option.to_list (let_binding_from_node ~is_recursive_binding:false node)
    | Syntax_kind.LET_REC_BINDING ->
        Option.to_list (let_binding_from_node ~is_recursive_binding:true node)
    | Syntax_kind.LET_EXPR ->
        Option.to_list
          (let_expression_binding_from_node ~is_recursive_binding:false node)
    | Syntax_kind.LET_REC_EXPR ->
        Option.to_list
          (let_expression_binding_from_node ~is_recursive_binding:true node)
    | _ -> []
  in
  let nested =
    direct_non_trivia_nodes node |> List.concat_map collect_let_bindings
  in
  bindings_here @ nested

let rec items_from_node node =
  match Ceibo.Red.SyntaxNode.kind node with
  | Syntax_kind.TYPE_DECL -> (
      match type_declaration_from_node node with
      | Some decl -> [ Item.TypeDeclaration decl ]
      | None -> [ Item.Unknown node ])
  | Syntax_kind.TYPE_MUTUAL_DECL ->
      direct_non_trivia_nodes node |> List.concat_map items_from_node
  | Syntax_kind.LET_BINDING -> (
      match let_binding_from_node ~is_recursive_binding:false node with
      | Some binding -> [ Item.LetBinding binding ]
      | None -> [ Item.Unknown node ])
  | Syntax_kind.LET_REC_BINDING -> (
      match let_binding_from_node ~is_recursive_binding:true node with
      | Some binding -> [ Item.LetBinding binding ]
      | None -> [ Item.Unknown node ])
  | Syntax_kind.LET_MUTUAL_DECL ->
      direct_non_trivia_nodes node
      |> List.filter (fun child ->
             let kind = Ceibo.Red.SyntaxNode.kind child in
             kind = Syntax_kind.LET_BINDING || kind = Syntax_kind.LET_REC_BINDING)
      |> List.concat_map items_from_node
  | Syntax_kind.MODULE_DECL -> (
      match module_declaration_from_node node with
      | Some decl -> [ Item.ModuleDeclaration decl ]
      | None -> [ Item.Unknown node ])
  | Syntax_kind.MODULE_TYPE_DECL -> (
      match module_type_declaration_from_node node with
      | Some decl -> [ Item.ModuleTypeDeclaration decl ]
      | None -> [ Item.Unknown node ])
  | _ -> [ Item.Unknown node ]

let of_green_tree tree =
  let root = Ceibo.Red.new_root tree in
  let file_items =
    direct_non_trivia_nodes root
    |> List.concat_map items_from_node
  in
  let file_let_bindings = collect_let_bindings root in
  SourceFile.
    { syntax_node = root; items = file_items; let_bindings = file_let_bindings }

let syntax_node_of_source_file source_file = SourceFile.syntax_node source_file
