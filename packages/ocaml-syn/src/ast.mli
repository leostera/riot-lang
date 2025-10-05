open Std

type location = { start : int; end_ : int }
type ident = string
type 'a node = { loc : location; data : 'a }

type pattern = pattern_kind node

and pattern_kind =
  | PatVar of ident
  | PatAny
  | PatConst of constant
  | PatTuple of pattern list
  | PatList of pattern list
  | PatCons of pattern * pattern
  | PatConstruct of ident * pattern option
  | PatRecord of (ident * pattern) list
  | PatOr of pattern * pattern
  | PatAlias of pattern * ident

and constant =
  | Int of int
  | Float of float
  | String of string
  | Char of char
  | Bool of bool

and expr = expr_kind node

and expr_kind =
  | Var of ident
  | Const of constant
  | Let of rec_flag * pattern * expr * expr
  | LetIn of rec_flag * (pattern * expr) list * expr
  | Fun of pattern list * expr
  | Function of case list
  | Apply of expr * expr list
  | Infix of string * expr * expr
  | Prefix of string * expr
  | Tuple of expr list
  | List of expr list
  | Cons of expr * expr
  | Record of (ident * expr) list
  | RecordWith of expr * (ident * expr) list
  | Field of expr * ident
  | Match of expr * case list
  | If of expr * expr * expr option
  | Sequence of expr list
  | Construct of ident * expr option

and case = { pattern : pattern; guard : expr option; body : expr }
and rec_flag = Recursive | Nonrecursive
and type_expr = type_expr_kind node

and type_expr_kind =
  | TVar of string
  | TConstr of ident * type_expr list
  | TArrow of type_expr * type_expr
  | TTuple of type_expr list
  | TRecord of (ident * type_expr) list

and type_decl = {
  name : ident;
  params : string list;
  manifest : type_expr option;
  kind : type_kind;
}

and type_kind =
  | Abstract
  | Alias of type_expr
  | Record of (ident * type_expr) list
  | Variant of (ident * type_expr option) list

and structure = structure_item list
and structure_item = structure_item_kind node

and structure_item_kind =
  | LetItem of rec_flag * (pattern * expr) list
  | TypeItem of type_decl list
  | ModuleItem of ident * module_expr
  | OpenItem of ident
  | IncludeItem of module_expr
  | ExternalItem of ident * type_expr * string list

and module_expr = module_expr_kind node

and module_expr_kind =
  | ModIdent of ident
  | ModStruct of structure
  | ModFunctor of ident * module_type option * module_expr

and module_type = module_type_kind node
and module_type_kind = MtSig of signature | MtIdent of ident
and signature = signature_item list
and signature_item = signature_item_kind node

and signature_item_kind =
  | ValSig of ident * type_expr
  | TypeSig of type_decl
  | ModuleSig of ident * module_type

val mk_node : location -> 'a -> 'a node
val mk_expr : location -> expr_kind -> expr
val mk_pattern : location -> pattern_kind -> pattern
