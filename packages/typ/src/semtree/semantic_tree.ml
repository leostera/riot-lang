open Std

type file_kind =
[
  `Implementation
  | `Interface
]

type path = string list

type arrow_label = {
  name: string;
  optional_: bool;
}

type type_constr = {
  path: path;
  arguments: type_expr list;
}

and type_alias = {
  type_: type_expr;
  name: string;
}

and type_poly = {
  binders: string list;
  body: type_expr;
}

and type_arrow = {
  label: arrow_label option;
  parameter: type_expr;
  result: type_expr;
}

and type_expr =
  | AnyType
  | TypeVar of string
  | TypeConstr of type_constr
  | TypeAlias of type_alias
  | TypePoly of type_poly
  | TypeArrow of type_arrow
  | TypeTuple of type_expr list
  | TypeUnsupported of string

type type_declaration = {
  id: Model.Binding_id.t;
  span: Syn.Ceibo.Span.t;
  name: string;
  params: string list;
  manifest: type_expr option;
  nonrec_: bool;
  private_: bool;
}

type value_declaration = {
  id: Model.Binding_id.t;
  span: Syn.Ceibo.Span.t;
  name: string option;
  recursive: bool;
  parameter_count: int;
  declared: bool;
  annotation: type_expr option;
}

type module_definition =
  | Alias of path
  | Opaque

type module_declaration = {
  id: Model.Binding_id.t;
  span: Syn.Ceibo.Span.t;
  name: string;
  recursive: bool;
  definition: module_definition;
}

type module_type_declaration = {
  id: Model.Binding_id.t;
  span: Syn.Ceibo.Span.t;
  name: string;
  has_definition: bool;
}

type open_statement = {
  span: Syn.Ceibo.Span.t;
  target: path option;
  override_: bool;
}

type include_target =
  | ModulePath of path
  | ModuleTypePath of path
  | Opaque

type include_statement = {
  span: Syn.Ceibo.Span.t;
  target: include_target;
}

type exception_rhs =
  | ExceptionAlias of path
  | ExceptionPayload of type_expr

type exception_declaration = {
  id: Model.Binding_id.t;
  span: Syn.Ceibo.Span.t;
  name: string;
  rhs: exception_rhs option;
}

type external_declaration = {
  id: Model.Binding_id.t;
  span: Syn.Ceibo.Span.t;
  name: string;
  annotation: type_expr;
}

type expression_item = {
  span: Syn.Ceibo.Span.t;
}

type unsupported_item = {
  span: Syn.Ceibo.Span.t;
  kind: Syn.SyntaxKind.t;
  summary: string;
}

type item =
  | TypeDeclaration of type_declaration
  | ValueDeclaration of value_declaration
  | ModuleDeclaration of module_declaration
  | ModuleTypeDeclaration of module_type_declaration
  | OpenStatement of open_statement
  | IncludeStatement of include_statement
  | ExceptionDeclaration of exception_declaration
  | ExternalDeclaration of external_declaration
  | Expression of expression_item
  | Unsupported of unsupported_item

type t = {
  kind: file_kind;
  items: item list;
  exports: item list;
  diagnostics: Diagnostics.Diagnostic.t list;
}

let exports_of_items = fun items ->
  List.filter items ~fn:(
    function
    | TypeDeclaration _ -> true
    | ValueDeclaration _ -> true
    | ModuleDeclaration _ -> true
    | ModuleTypeDeclaration _ -> true
    | IncludeStatement _ -> true
    | ExceptionDeclaration _ -> true
    | ExternalDeclaration _ -> true
    | OpenStatement _ -> false
    | Expression _ -> false
    | Unsupported _ -> false
  )

let empty = fun ~kind -> { kind; items = []; exports = []; diagnostics = [] }
