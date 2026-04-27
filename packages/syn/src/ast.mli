open Std
open Std.Collections

(**
   Typed views over `Syntax_tree`.

   Ast values are small handles: a tree pointer plus a node or token id. They
   do not own syntax, allocate a second CST, or guarantee that a node is
   complete. Accessors return `option` where recovery or malformed input can
   leave a child missing.
*)
type node = {
  tree: Syntax_tree.t;
  id: int;
}
type token = {
  tree: Syntax_tree.t;
  id: int;
}
type source_file = node
type implementation = node
type interface = node
type structure_item = node
type signature_item = node
type let_declaration = node
type let_binding = node
type type_declaration = node
type type_extension_declaration = node
type module_declaration = node
type module_type_declaration = node
type module_type_constraint = node
type open_declaration = node
type include_declaration = node
type value_declaration = node
type external_declaration = node
type exception_declaration = node
type extension_item = node
type attribute_item = node
type expr_item = node
type expr = node
type pattern = node
type parameter = node
type match_case = node
type type_expr = node
type record_type = node
type record_field = node
type record_expr_field = node
type variant_type = node
type variant_constructor = node
type path = node
type record_expr_field_view = {
  path: path option;
  value: expr option;
  node: record_expr_field;
}
type record_pattern_field_view = {
  path: path option;
  pattern: pattern option;
  node: pattern;
}
type first_class_module_pattern_ascription =
  | NoAscription
  | PathAscription
  | UnsupportedAscription
type type_item =
  | TypeDeclarationItem of type_declaration
  | TypeExtensionItem of type_extension_declaration

(** Root view for a parsed syntax tree. *)
val root: Syntax_tree.t -> node

module Token: sig
  type t = token
  (**
     Delimited comment/docstring trivia split once by the lexer/Ast layer.

     `content` excludes the opening and closing delimiters, so formatter and
     documentation tools do not need to rescan raw comment text.
  *)
  type delimited_trivia = {
    text: string;
    opening: string;
    content: string;
    closing: string option;
  }
  type leading_trivia =
    | Whitespace
    | Comment of delimited_trivia
    | Docstring of delimited_trivia
  val kind: t -> Syntax_kind.t

  val width: t -> int

  val contains_char: t -> char -> bool

  val text_is: t -> string -> bool

  val text_equal: t -> t -> bool

  val has_newline: t -> bool

  val slice: t -> IO.IoVec.IoSlice.t

  val text: t -> string

  (** Source span of the token body, excluding leading trivia. *)
  val span: t -> Ceibo.Span.t

  val span_start: t -> int

  val span_end: t -> int

  (** Materialize all leading trivia attached to this token. *)
  val leading_text: t -> string

  (** Iterate raw leading trivia as syntax kind/text pairs. *)
  val for_each_leading_trivia: t -> fn:(kind:Syntax_kind.t -> text:string -> unit) -> unit

  (**
     Iterate normalized leading trivia items. Whitespace is structural and
     comment/docstring delimiters are split from their content.
  *)
  val for_each_leading_trivia_item: t -> fn:(leading_trivia -> unit) -> unit

  val has_leading_whitespace: t -> bool

  val has_leading_comment: t -> bool

  val has_leading_docstring: t -> bool

  val full_text: t -> string

  (** Raw-token range owned by this token, including leading trivia. *)
  val raw_range: t -> int * int
end

module Node: sig
  type t = node
  val kind: t -> Syntax_kind.t

  val text: t -> string

  (** Source span covered by this node's syntactic tokens, excluding leading trivia. *)
  val span: t -> Ceibo.Span.t

  val span_start: t -> int

  val span_end: t -> int

  val raw_range: t -> int * int

  val full_width: t -> int

  val token_width: t -> int

  val child_count: t -> int

  (**
     Access a raw child edge by index. Most callers should prefer typed view
     accessors on the domain-specific modules below.
  *)
  val child_at: t -> int -> Syntax_tree.child option

  val for_each_child: t -> fn:(Syntax_tree.child -> unit) -> unit

  val for_each_child_node: t -> fn:(t -> unit) -> unit

  val for_each_child_token: t -> fn:(Token.t -> unit) -> unit

  val for_each_token: t -> fn:(Token.t -> unit) -> unit

  val first_child_node: t -> kind:Syntax_kind.t -> t option

  val first_child_token: t -> kind:Syntax_kind.t -> Token.t option

  val first_token: t -> Token.t option

  val first_descendant_token: t -> Token.t option
end

module TypeExpr: sig
  type t = type_expr
  type arrow_label = {
    name: Token.t option;
    optional_: bool;
  }
  type view =
    | Ident of { path: path }
    | Var of {
        name: Token.t;
      }
    | Wildcard
    | Arrow of {
        label: arrow_label option;
        arg: t;
        ret: t;
      }
    | Poly of {
        body: t;
      }
    | Tuple of {
        parts: t Vector.t;
      }
    | Apply of {
        ident: path;
        args: t Vector.t;
      }
    | Error of Node.t
    | Unknown of Node.t
  val cast: Node.t -> t option

  val view: t -> view

  val poly_type_keyword_token: t -> Token.t option

  val for_each_poly_type_name: t -> fn:(Token.t -> unit) -> unit

  val for_each_child_type: t -> fn:(t -> unit) -> unit

  val inner_without_attribute_suffix: t -> t option

  val for_each_attribute_suffix_token: t -> fn:(Token.t -> unit) -> unit
end

module RecordField: sig
  type t = record_field
  val cast: Node.t -> t option

  val mutable_token: t -> Token.t option

  val name: t -> Token.t option

  val colon_token: t -> Token.t option

  val type_annotation: t -> type_expr option
end

module RecordType: sig
  type t = record_type
  val cast: Node.t -> t option

  val private_token: t -> Token.t option

  val opening_token: t -> Token.t option

  val closing_token: t -> Token.t option

  val for_each_field: t -> fn:(record_field -> unit) -> unit
end

module VariantConstructor: sig
  type t = variant_constructor
  val cast: Node.t -> t option

  val pipe_token: t -> Token.t option

  val name: t -> Token.t option

  val of_token: t -> Token.t option

  val colon_token: t -> Token.t option

  val payload_type: t -> type_expr option

  val result_type: t -> type_expr option

  val record_payload: t -> record_type option
end

module VariantType: sig
  type t = variant_type
  val cast: Node.t -> t option

  val private_token: t -> Token.t option

  val for_each_constructor: t -> fn:(variant_constructor -> unit) -> unit
end

module Pattern: sig
  type t = pattern
  type view =
    | Unit
    | Wildcard
    | Ident of { path: path }
    | Construct of {
        constructor: path;
        payload: t option;
      }
    | Literal of {
        token: Token.t;
      }
    | Tuple of {
        parts: t Vector.t;
      }
    | List of {
        items: t Vector.t;
      }
    | Array of {
        items: t Vector.t;
      }
    | Record of {
        fields: record_pattern_field_view Vector.t;
        open_wildcard: Token.t option;
      }
    | PolyVariant of {
        tag: Token.t;
        payload: t option;
      }
    | FirstClassModule of {
        binder: Token.t;
        ascription: first_class_module_pattern_ascription;
        ascription_path: Token.t Vector.t;
      }
    | Interval of {
        left: t;
        right: t;
      }
    | Constraint of {
        pattern: t;
        annotation: type_expr;
      }
    | Alias of {
        pattern: t;
        alias: t;
      }
    | Or of {
        left: t;
        right: t;
      }
    | Cons of {
        head: t;
        tail: t;
      }
    | Lazy of {
        pattern: t;
      }
    | Exception of {
        pattern: t;
      }
    | Error of Node.t
    | Unknown of Node.t
  val cast: Node.t -> t option

  val view: t -> view

  val literal_token: t -> Token.t option

  val literal_sign_token: t -> Token.t option

  val for_each_child_pattern: t -> fn:(t -> unit) -> unit
end

module AttributePattern: sig
  type t = pattern
  val cast: pattern -> t option

  val inner: t -> pattern option

  val for_each_shell_token: t -> fn:(Token.t -> unit) -> unit
end

module ExtensionPattern: sig
  type t = pattern
  val cast: pattern -> t option

  val for_each_shell_token: t -> fn:(Token.t -> unit) -> unit
end

module LocallyAbstractTypePattern: sig
  type t = pattern
  val cast: pattern -> t option

  val opening_token: t -> Token.t option

  val type_token: t -> Token.t option

  val closing_token: t -> Token.t option

  val for_each_type_name: t -> fn:(Token.t -> unit) -> unit
end

module FirstClassModulePattern: sig
  type t = pattern
  type ascription = first_class_module_pattern_ascription =
    | NoAscription
    | PathAscription
    | UnsupportedAscription
  val cast: pattern -> t option

  val opening_token: t -> Token.t option

  val module_token: t -> Token.t option

  val binder: t -> Token.t option

  val colon_token: t -> Token.t option

  val closing_token: t -> Token.t option

  val ascription: t -> ascription

  val for_each_ascription_path_ident: t -> fn:(Token.t -> unit) -> unit
end

module RecordPattern: sig
  type t = pattern
  type field = record_pattern_field_view
  val cast: pattern -> t option

  val open_wildcard: t -> Token.t option

  val for_each_field: t -> fn:(field -> unit) -> unit
end

module LocalOpenPattern: sig
  type t = pattern
  val cast: pattern -> t option

  val dot_token: t -> Token.t option

  val opening_token: t -> Token.t option

  val closing_token: t -> Token.t option

  val pattern: t -> pattern option

  val for_each_module_path_ident: t -> fn:(Token.t -> unit) -> unit
end

module Parameter: sig
  type t = parameter
  type view =
    | Positional of {
        pattern: pattern;
      }
    | Labeled of {
        label: Token.t option;
        pattern: pattern option;
      }
    | Optional of {
        label: Token.t option;
        pattern: pattern option;
      }
    | OptionalDefault of {
        label: Token.t option;
        pattern: pattern option;
        default: expr option;
      }
    | Unknown of Node.t
  val cast: Node.t -> t option

  val view: t -> view

  val has_explicit_pattern_parens: t -> bool
end

module MatchCase: sig
  type t = match_case
  type view =
    | Case of {
        pattern: pattern;
        guard: expr option;
        body: expr;
      }
    | Unknown of Node.t
  val cast: Node.t -> t option

  val view: t -> view

  val pattern: t -> pattern option

  val guard: t -> expr option

  val body: t -> expr option
end

module LetBinding: sig
  type t = let_binding
  type view =
    | Binding of {
        pattern: pattern;
        body: expr;
      }
    | Unknown of Node.t
  val cast: Node.t -> t option

  val view: t -> view

  val pattern: t -> pattern option

  val body: t -> expr option

  val for_each_parameter: t -> fn:(parameter -> unit) -> unit

  val type_annotation: t -> type_expr option
end

module Expr: sig
  type t = expr
  type fun_body =
    | Body_expr of t
    | Body_cases of {
        first_case: match_case;
      }

  type view =
    | Unit
    | Let of {
        first_binding: let_binding;
        body: t;
      }
    | LocalOpen of {
        body: t;
      }
    | LetModule of {
        body: t;
      }
    | LetException of {
        body: t;
      }
    | If of {
        condition: t;
        then_branch: t;
        else_branch: t option;
      }
    | Match of {
        scrutinee: t;
        first_case: match_case;
      }
    | Fun of {
        body: fun_body;
      }
    | Try of {
        body: t;
        first_case: match_case;
      }
    | While of {
        condition: t;
        body: t;
      }
    | For of {
        pattern: pattern;
        start_: t;
        stop: t;
        body: t;
      }
    | Sequence of {
        left: t;
        right: t option;
      }
    | Apply of {
        callee: t;
        argument: t;
      }
    | Infix of {
        left: t;
        operator: Token.t;
        right: t;
      }
    | Prefix of {
        operator: Token.t;
        operand: t;
      }
    | Assign of {
        target: t;
        operator: Token.t;
        value: t;
      }
    | FieldAccess of {
        target: t;
        field: Token.t;
      }
    | PolyVariant of {
        tag: Token.t;
        payload: t option;
      }
    | Ident of { path: path }
    | Literal of {
        token: Token.t;
      }
    | Tuple of {
        items: t Vector.t;
      }
    | List of {
        items: t Vector.t;
      }
    | Array of {
        items: t Vector.t;
      }
    | Record of {
        base: t option;
        fields: record_expr_field_view Vector.t;
      }
    | Annotated of {
        expr: t;
        annotation: type_expr;
      }
    | Error of Node.t
    | Unknown of Node.t
  val cast: Node.t -> t option

  val view: t -> view

  val literal_token: t -> Token.t option

  val list_has_trailing_separator: t -> bool

  val for_each_child_expr: t -> fn:(t -> unit) -> unit

  val for_each_match_case: t -> fn:(match_case -> unit) -> unit
end

module AttributeExpr: sig
  type t = expr
  val cast: expr -> t option

  val inner: t -> expr option

  val for_each_shell_token: t -> fn:(Token.t -> unit) -> unit
end

module ExtensionExpr: sig
  type t = expr
  val cast: expr -> t option

  val for_each_shell_token: t -> fn:(Token.t -> unit) -> unit
end

module RecordExpr: sig
  type t = expr
  type field = record_expr_field_view
  val cast: expr -> t option

  val base: t -> expr option

  val for_each_field: t -> fn:(field -> unit) -> unit
end

module LocalOpenExpr: sig
  type t = expr
  type view =
    | LetOpen of {
        let_token: Token.t option;
        open_token: Token.t option;
        bang_token: Token.t option;
        module_path: path option;
        in_token: Token.t option;
        body: expr option;
      }
    | Delimited of {
        module_path: path option;
        dot_token: Token.t option;
        opening_token: Token.t option;
        body: expr option;
        closing_token: Token.t option;
      }
  val cast: expr -> t option

  val view: t -> view
end

module LetModuleExpr: sig
  type t = expr
  type module_body =
    | Path
    | EmptyStruct
    | Unsupported
  val cast: expr -> t option

  val let_token: t -> Token.t option

  val module_token: t -> Token.t option

  val name: t -> Token.t option

  val equals_token: t -> Token.t option

  val in_token: t -> Token.t option

  val module_body: t -> module_body

  val module_body_node: t -> Node.t option

  val body: t -> expr option

  val for_each_module_body_path_ident: t -> fn:(Token.t -> unit) -> unit
end

module LetExceptionExpr: sig
  type t = expr
  val cast: expr -> t option

  val let_token: t -> Token.t option

  val exception_token: t -> Token.t option

  val name: t -> Token.t option

  val of_token: t -> Token.t option

  val in_token: t -> Token.t option

  val body: t -> expr option

  val for_each_payload_token: t -> fn:(Token.t -> unit) -> unit
end

module UnreachableExpr: sig
  type t = expr
  val cast: expr -> t option

  val dot_token: t -> Token.t option
end

module FirstClassModuleExpr: sig
  type t = expr
  type module_path =
    | ModulePath
    | UnsupportedModulePath
  type ascription =
    | NoAscription
    | PathAscription
    | UnsupportedAscription
  val cast: expr -> t option

  val opening_token: t -> Token.t option

  val module_token: t -> Token.t option

  val colon_token: t -> Token.t option

  val closing_token: t -> Token.t option

  val module_path: t -> module_path

  val ascription: t -> ascription

  val for_each_module_path_ident: t -> fn:(Token.t -> unit) -> unit

  val for_each_ascription_path_ident: t -> fn:(Token.t -> unit) -> unit
end

module BindingOperatorExpr: sig
  type t = expr
  type clause = {
    keyword: Token.t option;
    operator: Token.t option;
    binding: let_binding;
  }
  val cast: expr -> t option

  val in_token: t -> Token.t option

  val body: t -> expr option

  val for_each_clause: t -> fn:(clause -> unit) -> unit
end

module Path: sig
  type t = path
  val cast: Node.t -> t option

  val text: t -> string

  val first_ident: t -> Token.t option

  val last_ident: t -> Token.t option

  val for_each_ident: t -> fn:(Token.t -> unit) -> unit
end

module StructureItem: sig
  type t = structure_item
  type view =
    | Let of let_declaration
    | Type of type_item
    | Module of module_declaration
    | ModuleType of module_type_declaration
    | Open of open_declaration
    | Include of include_declaration
    | External of external_declaration
    | Exception of exception_declaration
    | Extension of extension_item
    | Attribute of attribute_item
    | Expr of expr_item
    | Error of Node.t
    | Unknown of Node.t
  val cast: Node.t -> t option

  val view: t -> view

  val declaration: t -> Node.t option
end

module SignatureItem: sig
  type t = signature_item
  type view =
    | Value of value_declaration
    | Type of type_item
    | Module of module_declaration
    | ModuleType of module_type_declaration
    | Open of open_declaration
    | Include of include_declaration
    | External of external_declaration
    | Exception of exception_declaration
    | Extension of extension_item
    | Attribute of attribute_item
    | Error of Node.t
    | Unknown of Node.t
  val cast: Node.t -> t option

  val view: t -> view

  val declaration: t -> Node.t option
end

module LetDeclaration: sig
  type t = let_declaration
  val cast: Node.t -> t option

  val rec_token: t -> Token.t option

  val first_binding: t -> let_binding option

  val for_each_binding: t -> fn:(let_binding -> unit) -> unit
end

module TypeDeclaration: sig
  type t = type_declaration
  type member
  type parameter =
    | Named of {
        name: Token.t;
        quote: Token.t option;
        variance: Token.t option;
        injective: Token.t option;
      }
    | Wildcard of {
        wildcard: Token.t;
        variance: Token.t option;
        injective: Token.t option;
      }

  module Member: sig
    type t = member
    val declaration: t -> type_declaration

    val start_index: t -> int

    val stop_index: t -> int

    val child_count: t -> int

    val child_at: t -> int -> Syntax_tree.child option

    val child_token_at: t -> int -> Token.t option

    val child_node_at: t -> int -> Node.t option

    val child_token_kind_is: t -> int -> Syntax_kind.t -> bool

    val for_each_child: t -> fn:(Syntax_tree.child -> unit) -> unit

    val for_each_child_token: t -> fn:(Token.t -> unit) -> unit

    val for_each_child_node: t -> fn:(Node.t -> unit) -> unit

    val record_type: t -> record_type option

    val variant_type: t -> variant_type option

    val shell_token: t -> Token.t option

    val nonrec_token: t -> Token.t option

    val name: t -> Token.t option

    val for_each_parameter: t -> fn:(parameter -> unit) -> unit

    val manifest: t -> type_expr option
  end

  val cast: Node.t -> t option

  val for_each_token: t -> fn:(Token.t -> unit) -> unit

  val keyword_token: t -> Token.t option

  val nonrec_token: t -> Token.t option

  val name: t -> Token.t option

  val for_each_parameter: t -> fn:(parameter -> unit) -> unit

  val manifest: t -> type_expr option

  val for_each_member: t -> fn:(member -> unit) -> unit

  val fold_members: t -> 'acc -> ('acc -> member -> 'acc) -> 'acc
end

module TypeExtensionDeclaration: sig
  type t = type_extension_declaration
  type parameter = TypeDeclaration.parameter
  val cast: Node.t -> t option

  val keyword_token: t -> Token.t option

  val plus_token: t -> Token.t option

  val equals_token: t -> Token.t option

  val name: t -> Token.t option

  val for_each_name_ident: t -> fn:(Token.t -> unit) -> unit

  val for_each_parameter: t -> fn:(parameter -> unit) -> unit

  val variant_type: t -> variant_type option
end

module ModuleDeclaration: sig
  type t = module_declaration
  type member

  module Member: sig
    type t = member
    val declaration: t -> module_declaration

    val start_index: t -> int

    val stop_index: t -> int

    val child_count: t -> int

    val child_at: t -> int -> Syntax_tree.child option

    val child_token_at: t -> int -> Token.t option

    val child_node_at: t -> int -> Node.t option

    val child_token_kind_is: t -> int -> Syntax_kind.t -> bool

    val for_each_child: t -> fn:(Syntax_tree.child -> unit) -> unit

    val for_each_child_token: t -> fn:(Token.t -> unit) -> unit

    val for_each_child_node: t -> fn:(Node.t -> unit) -> unit

    val name: t -> Token.t option

    val find_token: t -> Syntax_kind.t -> int option

    val find_node: t -> matches:(Syntax_kind.t -> bool) -> Node.t option

    val module_expr: t -> Node.t option

    val module_type: t -> Node.t option
  end

  type body =
    | Path
    | Struct
    | EmptyStruct
    | EmptySig
    | Sig
    | Unsupported
  val cast: Node.t -> t option

  val name: t -> Token.t option

  val rec_token: t -> Token.t option

  val is_recursive: t -> bool

  val separator_token: t -> Token.t option

  val for_each_member: t -> fn:(member -> unit) -> unit

  val fold_members: t -> 'acc -> ('acc -> member -> 'acc) -> 'acc

  val body: t -> body

  val struct_token: t -> Token.t option

  val sig_token: t -> Token.t option

  val end_token: t -> Token.t option

  val for_each_body_path_ident: t -> fn:(Token.t -> unit) -> unit

  val has_typeof_body: t -> bool

  val for_each_typeof_body_path_ident: t -> fn:(Token.t -> unit) -> unit

  val for_each_structure_item: t -> fn:(structure_item -> unit) -> unit

  val for_each_signature_item: t -> fn:(signature_item -> unit) -> unit

  val for_each_sig_body_token: t -> fn:(Token.t -> unit) -> unit
end

module ModuleTypeDeclaration: sig
  type t = module_type_declaration
  type body =
    | Abstract
    | Path
    | EmptySig
    | Sig
    | With
    | Unsupported
  val cast: Node.t -> t option

  val name: t -> Token.t option

  val equals_token: t -> Token.t option

  val for_each_head_token: t -> fn:(Token.t -> unit) -> unit

  val body: t -> body

  val sig_token: t -> Token.t option

  val end_token: t -> Token.t option

  val for_each_body_path_ident: t -> fn:(Token.t -> unit) -> unit

  val for_each_signature_item: t -> fn:(signature_item -> unit) -> unit

  val for_each_sig_body_token: t -> fn:(Token.t -> unit) -> unit

  val base_module_type: t -> Node.t option

  val for_each_constraint: t -> fn:(module_type_constraint -> unit) -> unit
end

module ModuleTypeConstraint: sig
  type t = module_type_constraint
  type view =
    | Type of {
        path: path option;
        operator: Token.t option;
        body: type_expr option;
      }
    | Module of {
        path: path option;
        body: Node.t option;
      }
    | Unknown of Node.t
  val cast: Node.t -> t option

  val view: t -> view
end

module OpenDeclaration: sig
  type t = open_declaration
  val cast: Node.t -> t option

  val path_text: t -> string

  val first_path_ident: t -> Token.t option

  val last_path_ident: t -> Token.t option

  val for_each_path_ident: t -> fn:(Token.t -> unit) -> unit
end

module IncludeDeclaration: sig
  type t = include_declaration
  val cast: Node.t -> t option

  val path_text: t -> string

  val body_node: t -> Node.t option

  val first_path_ident: t -> Token.t option

  val last_path_ident: t -> Token.t option

  val for_each_path_ident: t -> fn:(Token.t -> unit) -> unit
end

module ValueDeclaration: sig
  type t = value_declaration
  val cast: Node.t -> t option

  val name: t -> Token.t option

  val colon_token: t -> Token.t option

  val type_annotation: t -> type_expr option

  val for_each_name_token: t -> fn:(Token.t -> unit) -> unit

  val for_each_annotation_token: t -> fn:(Token.t -> unit) -> unit
end

module ExternalDeclaration: sig
  type t = external_declaration
  val cast: Node.t -> t option

  val name: t -> Token.t option

  val colon_token: t -> Token.t option

  val type_annotation: t -> type_expr option

  val for_each_name_token: t -> fn:(Token.t -> unit) -> unit

  val for_each_primitive_string: t -> fn:(Token.t -> unit) -> unit

  val for_each_attribute_token: t -> fn:(Token.t -> unit) -> unit
end

module ExceptionDeclaration: sig
  type t = exception_declaration
  type payload =
    | TypeExpr of type_expr
    | Record of record_type
  type view =
    | Bare
    | Alias of {
        equals_token: Token.t option;
        path: path option;
      }
    | Payload of {
        of_token: Token.t option;
        payload: payload option;
      }
  val cast: Node.t -> t option

  val keyword_token: t -> Token.t option

  val name: t -> Token.t option

  val view: t -> view
end

module ExtensionItem: sig
  type t = extension_item
  val cast: Node.t -> t option

  val for_each_shell_token: t -> fn:(Token.t -> unit) -> unit
end

module AttributeItem: sig
  type t = attribute_item
  val cast: Node.t -> t option

  val for_each_shell_token: t -> fn:(Token.t -> unit) -> unit
end

module ExprItem: sig
  type t = expr_item
  val cast: Node.t -> t option

  val expr: t -> expr option
end

module Implementation: sig
  type t = implementation
  val cast: Node.t -> t option

  val for_each_item: t -> fn:(structure_item -> unit) -> unit
end

module Interface: sig
  type t = interface
  val cast: Node.t -> t option

  val for_each_item: t -> fn:(signature_item -> unit) -> unit
end

module SourceFile: sig
  type t = source_file
  type view =
    | Implementation of implementation
    | Interface of interface
    | Empty
  val make: Syntax_tree.t -> t

  val view: t -> view

  val implementation: t -> implementation option

  val interface: t -> interface option

  val for_each_item: t -> fn:(Node.t -> unit) -> unit

  val for_each_structure_item: t -> fn:(structure_item -> unit) -> unit

  val for_each_signature_item: t -> fn:(signature_item -> unit) -> unit
end
