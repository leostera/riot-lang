open Std

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
type module_declaration = node
type module_type_declaration = node
type open_declaration = node
type include_declaration = node
type value_declaration = node
type external_declaration = node
type exception_declaration = node
type class_declaration = node
type extension_item = node
type attribute_item = node
type expr_item = node
type expr = node
type pattern = node
type parameter = node
type match_case = node
type type_expr = node
type path = node
val root: Syntax_tree.t -> node

module Token: sig
  type t = token
  val kind: t -> Syntax_kind2.t

  val text: t -> string

  val raw_range: t -> int * int
end

module Node: sig
  type t = node
  val kind: t -> Syntax_kind2.t

  val text: t -> string

  val raw_range: t -> int * int

  val full_width: t -> int

  val child_count: t -> int

  val child_at: t -> int -> Syntax_tree.child option

  val for_each_child: t -> fn:(Syntax_tree.child -> unit) -> unit

  val for_each_child_node: t -> fn:(t -> unit) -> unit

  val for_each_child_token: t -> fn:(Token.t -> unit) -> unit

  val first_child_node: t -> kind:Syntax_kind2.t -> t option

  val first_child_token: t -> kind:Syntax_kind2.t -> Token.t option

  val first_token: t -> Token.t option
end

module TypeExpr: sig
  type t = type_expr
  type view =
    | Path of { path: path }
    | Var of { name: Token.t option }
    | Wildcard
    | Arrow of { left: t option; right: t option }
    | Tuple of { left: t option; right: t option }
    | Apply of { argument: t option; constructor: t option }
    | Parenthesized of { inner: t option }
    | Opaque of Node.t
    | Error of Node.t
    | Unknown of Node.t
  val cast: Node.t -> t option

  val view: t -> view

  val for_each_child_type: t -> fn:(t -> unit) -> unit
end

module Pattern: sig
  type t = pattern
  type view =
    | Wildcard
    | Path of { path: path }
    | Apply of { callee: t option; argument: t option }
    | Literal of { token: Token.t option }
    | Parenthesized of { inner: t option }
    | Tuple
    | List
    | Array
    | Record
    | PolyVariant
    | Extension
    | Attribute of { inner: t option }
    | LocalOpen
    | LocallyAbstractType
    | FirstClassModule
    | Interval of { left: t option; right: t option }
    | Constraint of { pattern: t option; annotation: type_expr option }
    | Alias of { pattern: t option; alias: t option }
    | Or of { left: t option; right: t option }
    | Cons of { head: t option; tail: t option }
    | Lazy of { pattern: t option }
    | Exception of { pattern: t option }
    | LabeledParam of parameter
    | OptionalParam of parameter
    | OptionalParamDefault of parameter
    | Error of Node.t
    | Unknown of Node.t
  val cast: Node.t -> t option

  val view: t -> view

  val literal_token: t -> Token.t option

  val for_each_child_pattern: t -> fn:(t -> unit) -> unit
end

module Parameter: sig
  type t = parameter
  type view =
    | Labeled of { label: Token.t option; pattern: pattern option }
    | Optional of { label: Token.t option; pattern: pattern option }
    | OptionalDefault of { label: Token.t option; pattern: pattern option; default: expr option }
    | Unknown of Node.t
  val cast: Node.t -> t option

  val view: t -> view
end

module MatchCase: sig
  type t = match_case
  type view = {
    pattern: pattern option;
    guard: expr option;
    body: expr option;
  }
  val cast: Node.t -> t option

  val view: t -> view
end

module LetBinding: sig
  type t = let_binding
  type view = {
    pattern: pattern option;
    body: expr option;
  }
  val cast: Node.t -> t option

  val view: t -> view

  val pattern: t -> pattern option

  val body: t -> expr option

  val for_each_parameter: t -> fn:(pattern -> unit) -> unit

  val type_annotation: t -> type_expr option
end

module Expr: sig
  type t = expr
  type view =
    | Let of { first_binding: let_binding option; body: t option }
    | LocalOpen of { body: t option }
    | LetModule of { body: t option }
    | LetException of { body: t option }
    | BindingOperator of { first_binding: let_binding option; body: t option }
    | FirstClassModule
    | Extension
    | Unreachable
    | Object
    | New
    | If of { condition: t option; then_branch: t option; else_branch: t option }
    | Match of { scrutinee: t option; first_case: match_case option }
    | Fun of { body: t option }
    | Function of { first_case: match_case option }
    | Try of { body: t option; first_case: match_case option }
    | While of { condition: t option; body: t option }
    | For of { pattern: pattern option; start_: t option; stop: t option; body: t option }
    | Assert of { argument: t option }
    | Lazy of { argument: t option }
    | Attribute of { inner: t option }
    | Sequence of { left: t option; right: t option }
    | Apply of { callee: t option; argument: t option }
    | Infix of { left: t option; operator: Token.t option; right: t option }
    | Prefix of { operator: Token.t option; operand: t option }
    | Assign of { target: t option; value: t option }
    | FieldAccess of { target: t option; field: Token.t option }
    | MethodCall of { target: t option; method_: Token.t option }
    | PolyVariant of { payload: t option }
    | Path of { path: path }
    | Literal of { token: Token.t option }
    | Parenthesized of { inner: t option }
    | Tuple
    | List
    | Array
    | Record
    | RecordUpdate
    | ArrayIndex of { target: t option; index: t option }
    | StringIndex of { target: t option; index: t option }
    | Typed of { expr: t option; annotation: type_expr option }
    | LabeledArg of { label: Token.t option; value: t option }
    | OptionalArg of { label: Token.t option; value: t option }
    | Error of Node.t
    | Unknown of Node.t
  val cast: Node.t -> t option

  val view: t -> view

  val literal_token: t -> Token.t option

  val for_each_child_expr: t -> fn:(t -> unit) -> unit

  val for_each_match_case: t -> fn:(match_case -> unit) -> unit
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
    | Type of type_declaration
    | Module of module_declaration
    | ModuleType of module_type_declaration
    | Open of open_declaration
    | Include of include_declaration
    | External of external_declaration
    | Exception of exception_declaration
    | Class of class_declaration
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
    | Type of type_declaration
    | Module of module_declaration
    | ModuleType of module_type_declaration
    | Open of open_declaration
    | Include of include_declaration
    | External of external_declaration
    | Exception of exception_declaration
    | Class of class_declaration
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
  val cast: Node.t -> t option

  val name: t -> Token.t option

  val for_each_parameter: t -> fn:(parameter -> unit) -> unit

  val manifest: t -> type_expr option
end

module ModuleDeclaration: sig
  type t = module_declaration
  val cast: Node.t -> t option

  val name: t -> Token.t option
end

module ModuleTypeDeclaration: sig
  type t = module_type_declaration
  val cast: Node.t -> t option

  val name: t -> Token.t option
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
end

module ValueDeclaration: sig
  type t = value_declaration
  val cast: Node.t -> t option

  val name: t -> Token.t option

  val type_annotation: t -> type_expr option
end

module ExternalDeclaration: sig
  type t = external_declaration
  val cast: Node.t -> t option

  val name: t -> Token.t option

  val type_annotation: t -> type_expr option
end

module ExceptionDeclaration: sig
  type t = exception_declaration
  val cast: Node.t -> t option

  val name: t -> Token.t option
end

module ClassDeclaration: sig
  type t = class_declaration
  val cast: Node.t -> t option

  val name: t -> Token.t option
end

module ExtensionItem: sig
  type t = extension_item
  val cast: Node.t -> t option
end

module AttributeItem: sig
  type t = attribute_item
  val cast: Node.t -> t option
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
