open Std

(** UntypedTree - Clean AST for type checking

    This is the intermediate representation between Syn's CST and TypedTree.
    Each node has a location pointing back to the source for diagnostics.

    Design principles:
    - Pattern-matchable: Easy to write type checking code against
    - Locations: Every node tracks its source position
    - Simple: No type information yet
    - References: Can point back to CST for detailed error messages *)

type location = {
  span : Syn.Ceibo.Span.t;  (** Start/end positions in source *)
  source_id : int option;  (** Optional source file identifier *)
}
(** Location information pointing back to source *)

(** Make a location from span *)
let make_location ?(source_id = None) span = { span; source_id }

(** Constants *)
type constant =
  | ConstantInt of int
  | ConstantFloat of float
  | ConstantString of string
  | ConstantChar of char
  | ConstantBool of bool
  | ConstantUnit

(** Binary operators *)
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
  | Cons  (** :: *)
  | At  (** @ *)

(** Unary operators *)
type unary_op = Neg | Not

type pattern = { pattern_desc : pattern_desc; pattern_loc : location }
(** Patterns - what you match against *)

and pattern_desc =
  | PatternAny  (** _ *)
  | PatternVar of string  (** x *)
  | PatternAlias of pattern * string  (** p as x *)
  | PatternConstant of constant  (** 42, "hello" *)
  | PatternTuple of pattern list  (** (p1, p2, p3) *)
  | PatternConstruct of { constructor : string; arg : pattern option }
      (** Some x, None, Cons (h, t) *)
  | PatternOr of pattern * pattern  (** p1 | p2 *)
  | PatternConstraint of pattern * core_type  (** (p : int) *)
  | PatternRecord of (string * pattern) list  (** {x; y = z} *)

and core_type = { type_desc : type_desc; type_loc : location }
(** Type expressions - for annotations *)

and type_desc =
  | TypeVar of string  (** 'a *)
  | TypeArrow of core_type * core_type  (** t1 -> t2 *)
  | TypeTuple of core_type list  (** t1 * t2 * t3 *)
  | TypeConstr of { name : string; args : core_type list }
      (** int, 'a list, ('a, 'b) map *)

type expression = { expr_desc : expression_desc; expr_loc : location }
(** Expressions - what you compute *)

and expression_desc =
  (* Core *)
  | ExprConstant of constant  (** 42, "hello", true *)
  | ExprIdent of string  (** x, List.map *)
  | ExprLet of {
      recursive : bool;
      pattern : pattern;
      value : expression;
      body : expression;
    }  (** let x = 1 in x + 2 *)
  | ExprFunction of { param : pattern; body : expression }
      (** fun x -> x + 1 *)
  | ExprApply of { func : expression; arg : expression }  (** f x *)
  | ExprMatch of { expr : expression; cases : case list }
      (** match e with | p1 -> e1 | p2 -> e2 *)
  (* Data structures *)
  | ExprTuple of expression list  (** (1, "hello", true) *)
  | ExprConstruct of { constructor : string; arg : expression option }
      (** Some 42, None *)
  | ExprRecord of (string * expression) list  (** {x = 1; y = 2} *)
  | ExprField of { record : expression; field : string }  (** r.x *)
  (* Control flow *)
  | ExprIfThenElse of {
      condition : expression;
      then_branch : expression;
      else_branch : expression option;
    }  (** if c then e1 else e2 *)
  | ExprSequence of expression * expression  (** e1; e2 *)
  (* Operators *)
  | ExprBinaryOp of { op : binary_op; left : expression; right : expression }
      (** e1 + e2 *)
  | ExprUnaryOp of { op : unary_op; arg : expression }  (** -e, not e *)
  (* Type annotations *)
  | ExprConstraint of { expr : expression; typ : core_type }  (** (e : int) *)

and case = {
  pattern : pattern;
  guard : expression option;  (** when clause *)
  rhs : expression;  (** right-hand side *)
}
(** Match case *)

type structure_item = { item_desc : structure_item_desc; item_loc : location }
(** Structure items - top-level declarations *)

and structure_item_desc =
  | ItemValue of { recursive : bool; pattern : pattern; expr : expression }
      (** let x = 42 *)
  | ItemType of {
      name : string;
      params : string list;
      manifest : type_definition;
    }  (** type t = ... *)

(** Type definitions *)
and type_definition =
  | TypeAlias of core_type  (** type t = int *)
  | TypeVariant of constructor_decl list  (** type t = A | B of int *)
  | TypeRecord of field_decl list  (** type t = {x: int; y: string} *)

and constructor_decl = {
  constructor_name : string;
  constructor_arg : core_type option;
  constructor_loc : location;
}
(** Constructor declaration *)

and field_decl = {
  field_name : string;
  field_type : core_type;
  field_mutable : bool;
  field_loc : location;
}
(** Field declaration *)

type structure = structure_item list
(** Top-level structure *)

(** Helper functions *)

let make_constant ~const ~loc =
  { expr_desc = ExprConstant const; expr_loc = loc }

let make_ident ~name ~loc = { expr_desc = ExprIdent name; expr_loc = loc }

let make_let ~recursive ~pattern ~value ~body ~loc =
  { expr_desc = ExprLet { recursive; pattern; value; body }; expr_loc = loc }

let make_function ~param ~body ~loc =
  { expr_desc = ExprFunction { param; body }; expr_loc = loc }

let make_apply ~func ~arg ~loc =
  { expr_desc = ExprApply { func; arg }; expr_loc = loc }

let make_tuple ~elements ~loc =
  { expr_desc = ExprTuple elements; expr_loc = loc }

let make_if ~condition ~then_branch ~else_branch ~loc =
  {
    expr_desc = ExprIfThenElse { condition; then_branch; else_branch };
    expr_loc = loc;
  }

let make_binary_op ~op ~left ~right ~loc =
  { expr_desc = ExprBinaryOp { op; left; right }; expr_loc = loc }

let make_pattern_var ~name ~loc =
  { pattern_desc = PatternVar name; pattern_loc = loc }

let make_pattern_any ~loc = { pattern_desc = PatternAny; pattern_loc = loc }

let make_pattern_constant ~const ~loc =
  { pattern_desc = PatternConstant const; pattern_loc = loc }

let make_pattern_tuple ~elements ~loc =
  { pattern_desc = PatternTuple elements; pattern_loc = loc }

let make_type_var ~name ~loc = { type_desc = TypeVar name; type_loc = loc }

let make_type_arrow ~param ~result ~loc =
  { type_desc = TypeArrow (param, result); type_loc = loc }

let make_type_constr ~name ~args ~loc =
  { type_desc = TypeConstr { name; args }; type_loc = loc }

let make_structure_item_value ~recursive ~pattern ~expr ~loc =
  { item_desc = ItemValue { recursive; pattern; expr }; item_loc = loc }

let make_structure_item_type ~name ~params ~manifest ~loc =
  { item_desc = ItemType { name; params; manifest }; item_loc = loc }
