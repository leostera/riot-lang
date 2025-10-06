open Std

(** Span information for AST nodes *)
type span = {
  start_pos : int;
  end_pos : int;
}

(** Binary operators *)
type binop =
  | Add | Sub | Mul | Div | Mod
  | Lt | Le | Gt | Ge | Eq | Ne
  | And | Or
  | Cons  (* :: *)
  | Concat (* ^ *)

(** Unary operators *)
type unop =
  | Not | Neg

(** Literal values *)
type literal =
  | Int of int
  | Float of float
  | String of string
  | Char of char
  | Bool of bool
  | Unit

(** Patterns for pattern matching *)
type pattern = {
  pat_desc : pattern_desc;
  pat_span : span;
}

and pattern_desc =
  | PatVar of string
  | PatWildcard
  | PatLiteral of literal
  | PatTuple of pattern list
  | PatCons of pattern * pattern
  | PatList of pattern list
  | PatRecord of (string * pattern) list
  | PatConstructor of string * pattern option
  | PatAs of pattern * string
  | PatOr of pattern * pattern

(** Type expressions *)
type type_expr = {
  type_desc : type_desc;
  type_span : span;
}

and type_desc =
  | TypeVar of string
  | TypeConstructor of string * type_expr list
  | TypeTuple of type_expr list
  | TypeFunction of type_expr * type_expr
  | TypeRecord of (string * type_expr) list

(** Expressions *)
type expr = {
  expr_desc : expr_desc;
  expr_span : span;
}

and expr_desc =
  | ExprLiteral of literal
  | ExprIdent of string
  | ExprLet of rec_flag * (pattern * expr) list * expr
  | ExprFunction of pattern * expr
  | ExprApply of expr * expr
  | ExprMatch of expr * case list
  | ExprTuple of expr list
  | ExprList of expr list
  | ExprArray of expr list
  | ExprRecord of (string * expr) list
  | ExprField of expr * string
  | ExprIfThenElse of expr * expr * expr option
  | ExprSequence of expr * expr
  | ExprWhile of expr * expr
  | ExprFor of string * expr * expr * direction_flag * expr
  | ExprTry of expr * case list
  | ExprBinaryOp of binop * expr * expr
  | ExprUnaryOp of unop * expr
  | ExprConstructor of string * expr option
  | ExprBeginEnd of expr

and case = {
  case_pattern : pattern;
  case_guard : expr option;
  case_expr : expr;
}

and rec_flag = Recursive | NonRecursive
and direction_flag = To | Downto

(** Type definitions *)
type type_def = {
  type_name : string;
  type_params : string list;
  type_manifest : type_manifest;
  type_span : span;
}

and type_manifest =
  | TypeAlias of type_expr
  | TypeVariant of constructor_decl list
  | TypeRecord of (string * type_expr) list

and constructor_decl = {
  constr_name : string;
  constr_args : type_expr option;
}

(** Module structure items *)
type structure_item = {
  str_desc : structure_item_desc;
  str_span : span;
}

and structure_item_desc =
  | StrEval of expr
  | StrLet of rec_flag * (pattern * expr) list
  | StrType of rec_flag * type_def list
  | StrModule of string * module_expr
  | StrOpen of string
  | StrInclude of module_expr
  | StrException of string * type_expr option
  | StrExternal of string * type_expr * string

and module_expr = {
  mod_desc : module_expr_desc;
  mod_span : span;
}

and module_expr_desc =
  | ModIdent of string
  | ModStruct of structure_item list
  | ModFunctor of string * module_type option * module_expr
  | ModApply of module_expr * module_expr

and module_type = {
  mty_desc : module_type_desc;
  mty_span : span;
}

and module_type_desc =
  | ModTypeIdent of string
  | ModTypeSig of signature_item list
  | ModTypeFunctor of string * module_type option * module_type
  | ModTypeWith of module_type * (string * type_expr) list

and signature_item = {
  sig_desc : signature_item_desc;
  sig_span : span;
}

and signature_item_desc =
  | SigVal of string * type_expr
  | SigType of rec_flag * type_def list
  | SigModule of string * module_type
  | SigOpen of string
  | SigInclude of module_type

(** Top-level program structure *)
type program = structure_item list