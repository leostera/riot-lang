open Std

(** UntypedTree - Clean AST for type checking

    This intermediate representation sits between Syn's CST and TypedTree. It
    provides a clean, pattern-matchable AST while preserving source locations.
*)

(** {1 Location Information} *)

type location = private { span : Syn.Ceibo.Span.t; source_id : int option }

val make_location : ?source_id:int option -> Syn.Ceibo.Span.t -> location

(** {1 Constants} *)

type constant =
  | ConstantInt of int
  | ConstantFloat of float
  | ConstantString of string
  | ConstantChar of char
  | ConstantBool of bool
  | ConstantUnit

(** {1 Operators} *)

type binary_op =
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | Eq
  | Neq
  | Lt
  | Le
  | Gt
  | Ge
  | And
  | Or
  | Cons
  | At

type unary_op = Neg | Not

(** {1 Patterns} *)

type pattern = private { pattern_desc : pattern_desc; pattern_loc : location }

and pattern_desc =
  | PatternAny
  | PatternVar of string
  | PatternAlias of pattern * string
  | PatternConstant of constant
  | PatternTuple of pattern list
  | PatternConstruct of { constructor : string; arg : pattern option }
  | PatternOr of pattern * pattern
  | PatternConstraint of pattern * core_type
  | PatternRecord of (string * pattern) list

(** {1 Type Expressions} *)

and core_type = private { type_desc : type_desc; type_loc : location }

and type_desc =
  | TypeVar of string
  | TypeArrow of core_type * core_type
  | TypeTuple of core_type list
  | TypeConstr of { name : string; args : core_type list }

(** {1 Expressions} *)

type expression = private { expr_desc : expression_desc; expr_loc : location }

and expression_desc =
  | ExprConstant of constant
  | ExprIdent of string
  | ExprLet of {
      recursive : bool;
      pattern : pattern;
      value : expression;
      body : expression;
    }
  | ExprFunction of { param : pattern; body : expression }
  | ExprApply of { func : expression; arg : expression }
  | ExprMatch of { expr : expression; cases : case list }
  | ExprTuple of expression list
  | ExprConstruct of { constructor : string; arg : expression option }
  | ExprRecord of (string * expression) list
  | ExprField of { record : expression; field : string }
  | ExprIfThenElse of {
      condition : expression;
      then_branch : expression;
      else_branch : expression option;
    }
  | ExprSequence of expression * expression
  | ExprBinaryOp of { op : binary_op; left : expression; right : expression }
  | ExprUnaryOp of { op : unary_op; arg : expression }
  | ExprConstraint of { expr : expression; typ : core_type }

and case = { pattern : pattern; guard : expression option; rhs : expression }

(** {1 Structure Items} *)

type structure_item = private {
  item_desc : structure_item_desc;
  item_loc : location;
}

and structure_item_desc =
  | ItemValue of { recursive : bool; pattern : pattern; expr : expression }
  | ItemType of {
      name : string;
      params : string list;
      manifest : type_definition;
    }

and type_definition =
  | TypeAlias of core_type
  | TypeVariant of constructor_decl list
  | TypeRecord of field_decl list

and constructor_decl = {
  constructor_name : string;
  constructor_arg : core_type option;
  constructor_loc : location;
}

and field_decl = {
  field_name : string;
  field_type : core_type;
  field_mutable : bool;
  field_loc : location;
}

type structure = structure_item list

(** {1 Constructor Functions} *)

val make_constant : const:constant -> loc:location -> expression
val make_ident : name:string -> loc:location -> expression

val make_let :
  recursive:bool ->
  pattern:pattern ->
  value:expression ->
  body:expression ->
  loc:location ->
  expression

val make_function :
  param:pattern -> body:expression -> loc:location -> expression

val make_apply : func:expression -> arg:expression -> loc:location -> expression
val make_tuple : elements:expression list -> loc:location -> expression

val make_if :
  condition:expression ->
  then_branch:expression ->
  else_branch:expression option ->
  loc:location ->
  expression

val make_binary_op :
  op:binary_op ->
  left:expression ->
  right:expression ->
  loc:location ->
  expression

val make_pattern_var : name:string -> loc:location -> pattern
val make_pattern_any : loc:location -> pattern
val make_pattern_constant : const:constant -> loc:location -> pattern
val make_pattern_tuple : elements:pattern list -> loc:location -> pattern
val make_type_var : name:string -> loc:location -> core_type

val make_type_arrow :
  param:core_type -> result:core_type -> loc:location -> core_type

val make_type_constr :
  name:string -> args:core_type list -> loc:location -> core_type

val make_structure_item_value :
  recursive:bool ->
  pattern:pattern ->
  expr:expression ->
  loc:location ->
  structure_item

val make_structure_item_type :
  name:string ->
  params:string list ->
  manifest:type_definition ->
  loc:location ->
  structure_item
