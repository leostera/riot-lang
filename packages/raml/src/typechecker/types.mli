type type_id = int

type type_expr = {
  mutable desc : type_desc;
  mutable level : int;
  mutable scope : int;
  id : type_id;
}

and type_desc =
  | Variable of string option
  | Arrow of arg_label * type_expr * type_expr
  | Tuple of type_expr list
  | Constructor of ModulePath.t * type_expr list
  | Link of type_expr
  | Substitution of type_expr
  | UniversalVariable of string option
  | Polymorphic of type_expr * type_expr list

and arg_label = Nolabel | Labelled of string | Optional of string

module TypeOps : sig
  type t = type_expr

  val compare : t -> t -> int
  val hash : t -> int
  val equal : t -> t -> bool
end

module TypeMap : sig
  type ('k, 'v) t

  val create : unit -> ('k, 'v) t
end

module TypeSet : sig
  type 'a t

  val create : unit -> 'a t
end

type variance =
  | Covariant
  | Contravariant
  | Invariant
  | MayPositive
  | MayNegative
  | MayWeak

type type_declaration = {
  type_params : type_expr list;
  type_arity : int;
  type_kind : type_kind;
  type_manifest : type_expr option;
  type_variance : variance list;
}

and type_kind =
  | Abstract
  | Record of label_declaration list
  | Variant of constructor_declaration list
  | Open

and label_declaration = {
  ld_name : string;
  ld_mutable : bool;
  ld_type : type_expr;
}

and constructor_declaration = {
  cd_name : string;
  cd_args : constructor_arguments;
  cd_res : type_expr option;
}

and constructor_arguments = ConstructorTuple of type_expr list

type value_description = { val_type : type_expr }

type context = {
  type_id_counter : int;
  type_level : int;
  identifier_ctx : Identifier.context;
}

val create_context : unit -> context
val new_type_id : context -> type_id * context
val newty : ctx:context -> type_desc -> type_expr * context
val new_type : ctx:context -> type_desc -> type_expr * context
val newvar : ctx:context -> string option -> type_expr * context
val repr : type_expr -> type_expr
val type_expr_to_string : type_expr -> string
val pp_type_expr : Format.formatter -> type_expr -> unit
