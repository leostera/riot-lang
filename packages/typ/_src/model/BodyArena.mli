open Std

(** Body-local semantic structure for one lowered source. *)
type pattern_desc =
  (** Variable binder pattern. *)
  | PVar of string
  (** Wildcard pattern. *)
  | PWildcard
  (** Integer literal pattern. *)
  | PInt of string
  (** Floating-point literal pattern. *)
  | PFloat of string
  (** Boolean literal pattern. *)
  | PBool of bool
  (** String literal pattern. *)
  | PString of string
  (** Character literal pattern. *)
  | PChar of string
  (** Unit pattern. *)
  | PUnit
  (** Tuple pattern with child pattern IDs. *)
  | PTuple of PatternArenaId.t list
  (** Or-pattern with child pattern IDs in source order. *)
  | POr of PatternArenaId.t list
  (** Constructor pattern with a stable constructor name and lowered payloads. *)
  | PConstructor of { constructor: SurfacePath.t; arguments: PatternArenaId.t list }
  (** Record pattern with lowered field patterns and explicit openness. *)
  | PRecord of { fields: record_pattern_field list; open_: bool }
  (** List pattern with lowered element patterns. *)
  | PList of PatternArenaId.t list
  (** Alias pattern that binds the matched value under an extra name. *)
  | PAlias of { pattern_id: PatternArenaId.t; alias: string }
  (** Unpack pattern that binds one packaged module under a module name. *)
  | PFirstClassModule of { module_name: string option; package_type: TypeRepr.t option }
  (** Lenient polymorphic-variant pattern with an optional payload pattern. *)
  | PPolyVariant of { tag: string; payload: PatternArenaId.t option }
  (** Recovery pattern preserved after unsupported surface syntax. *)
  | PUnsupported of string
and record_pattern_field = {
  (** Stable field label name as it appeared in the source. *)
  label: string;
  (** Lowered child pattern bound for this field. *)
  pattern_id: PatternArenaId.t;
}

type pattern_node = {
  (** Best-effort stable pattern identifier. *)
  pat_id: PatternArenaId.t;
  (** Source origin for this pattern node. *)
  origin_id: OriginId.t;
  (** Optional explicit type annotation preserved from the source pattern. *)
  annotation: TypeRepr.t option;
  (** Semantic payload for the pattern. *)
  desc: pattern_desc;
}

type match_case = {
  (** Pattern tested by this case. *)
  pattern_id: PatternArenaId.t;
  (** Optional guard expression evaluated after the pattern binds. *)
  guard_id: ExprArenaId.t option;
  (** Body expression evaluated when the pattern matches. *)
  body_id: ExprArenaId.t;
}

type label =
  (** Ordinary unlabeled argument or parameter. *)
  | Positional
  (** A labeled argument or parameter introduced with `~label:`. *)
  | Labeled of string
  (** An optional argument or parameter introduced with `?label:`. *)
  | Optional of string

type function_parameter = {
  (** Calling-convention label preserved from the source surface. *)
  label: label;
  (** Semantic pattern bound for this parameter. *)
  pattern_id: PatternArenaId.t;
  (** Lowered default expression for optional parameters such as `?(x = expr)`. *)
  default_value_id: ExprArenaId.t option;
}

type apply_argument = {
  (** Calling-convention label preserved from the call site. *)
  label: label;
  (** Whether this optional argument used implicit forwarding syntax (`?label`). *)
  implicit: bool;
  (** Lowered argument value expression. *)
  value_id: ExprArenaId.t;
}

type local_module_binding_group = { binding_ids: BindingArenaId.t list }

type local_module_scope = {
  (** Value-binding groups introduced by the local module body. *)
  binding_groups: local_module_binding_group list;
  (**
     Local type declarations owned by the local module body. These stay
     unqualified inside the scope and are qualified when the scope is attached
     under a module name.
  *)
  type_decls: FileSummary.type_decl list;
}

type expr_desc =
  (** Variable reference. *)
  | EVar of SurfacePath.t
  (** Integer literal expression. *)
  | EInt of string
  (** Floating-point literal expression. *)
  | EFloat of string
  (** Boolean literal expression. *)
  | EBool of bool
  (** String literal expression. *)
  | EString of string
  (** Character literal expression. *)
  | EChar of string
  (** Unit expression. *)
  | EUnit
  (** Tuple expression with child expression IDs. *)
  | ETuple of ExprArenaId.t list
  (** Array expression with child expression IDs. *)
  | EArray of ExprArenaId.t list
  (** Sequence expression evaluated left-to-right, returning the last type. *)
  | ESequence of ExprArenaId.t list
  (** While-loop with a boolean condition and a unit body. *)
  | EWhile of { condition_id: ExprArenaId.t; body_id: ExprArenaId.t }
  (** Integer for-loop with a scoped iterator, integer bounds, and a unit body. *)
  | EFor of {
    iterator_pattern_id: PatternArenaId.t;
    descending: bool;
    start_id: ExprArenaId.t;
    end_id: ExprArenaId.t;
    body_id: ExprArenaId.t;
  }
  (** Function expression with parameter patterns and one body expression. *)
  | EFun of function_parameter list * ExprArenaId.t
  (** Application with one callee and labeled or positional arguments. *)
  | EApply of ExprArenaId.t * apply_argument list
  (** Record literal or record update with lowered field expressions. *)
  | ERecord of { base_id: ExprArenaId.t option; fields: record_expr_field list }
  (** Record field access off one receiver expression. *)
  | EFieldAccess of { receiver_id: ExprArenaId.t; label: string }
  (** Record field assignment returning unit. *)
  | EFieldAssign of { receiver_id: ExprArenaId.t; label: string; value_id: ExprArenaId.t }
  (** Indexed access into one collection expression at one index expression. *)
  | EIndex of ExprArenaId.t * ExprArenaId.t
  (** Let-expression with local binding IDs and one body expression. *)
  | ELet of BindingArenaId.t list * ExprArenaId.t
  (** Conditional expression. *)
  | EIf of ExprArenaId.t * ExprArenaId.t * ExprArenaId.t
  (** Match expression with normalized cases. *)
  | EMatch of ExprArenaId.t * match_case list
  (** Try-expression with normalized exception handler cases. *)
  | ETry of ExprArenaId.t * match_case list
  (** Lenient polymorphic-variant expression with an optional payload. *)
  | EPolyVariant of { tag: string; payload: ExprArenaId.t option }
  (** Explicit coercion expression lowered from `(expr :> target)`. *)
  | ECoerce of { value_id: ExprArenaId.t; target_type: TypeRepr.t }
  (** First-class module pack expression lowered from `(module M [: S])`. *)
  | EModulePack of { module_path: SurfacePath.t; package_type: TypeRepr.t option }
  (**
     Local first-class module pack lowered from `(module M)` where [M] comes
     from one surrounding [let module M = struct ... end in ...].
  *)
  | ELocalModulePack of { local_scope: local_module_scope; package_type: TypeRepr.t option }
  (** Local module binding with a scoped module name available in the body. *)
  | ELocalModule of { module_name: string; local_scope: local_module_scope; body_id: ExprArenaId.t }
  (** Local module open expression with the lowered body expression. *)
  | ELocalOpen of { module_path: SurfacePath.t; body_id: ExprArenaId.t }
  (** Unsupported semantic node that still reached the inferencer. *)
  | EUnsupported of string
  (** Recovery hole introduced during lowering. *)
  | EHole of string
and record_expr_field = {
  (** Stable field label name as it appeared in the source. *)
  label: string;
  (** Lowered child expression used for this field. *)
  value_id: ExprArenaId.t;
}
and expr_node = {
  (** Best-effort stable expression identifier. *)
  expr_id: ExprArenaId.t;
  (** Source origin for this expression node. *)
  origin_id: OriginId.t;
  (** Semantic payload for the expression. *)
  desc: expr_desc;
}
and binding = {
  (** Stable binding identifier. *)
  binding_id: BindingArenaId.t;
  (** Source origin for this binding. *)
  origin_id: OriginId.t;
  (** Lexical module path that owns this binding, empty at top level. *)
  scope_path: SurfacePath.t;
  (** Simple binder name when one exists. *)
  name: string option;
  (** Pattern bound by this binding. *)
  pattern_id: PatternArenaId.t;
  (** Optional explicit type annotation preserved from the binding source. *)
  annotation: TypeScheme.t option;
  (** Value expression assigned by this binding. *)
  value_id: ExprArenaId.t;
  (** Whether the binding participates in a recursive group. *)
  recursive: bool;
}

(** Arena-style storage for patterns, bindings, and expressions. *)
type t

(** Empty body arena. *)
val empty: t

(** Build one arena from prepared node lists. *)
val of_lists: patterns:pattern_node list -> expressions:expr_node list -> bindings:binding list -> t

(** Enumerate all stored patterns. *)
val patterns: t -> pattern_node list

(** Enumerate all stored expressions. *)
val expressions: t -> expr_node list

(** Enumerate all stored bindings. *)
val bindings: t -> binding list

(** Find one pattern node by [PatternArenaId]. *)
val find_pattern: t -> PatternArenaId.t -> pattern_node option

(** Find one expression node by [ExprArenaId]. *)
val find_expr: t -> ExprArenaId.t -> expr_node option

(** Find one binding node by [BindingArenaId]. *)
val find_binding: t -> BindingArenaId.t -> binding option

(** Encode the arena as structured JSON for snapshot tests and tooling. *)
val to_json: t -> Data.Json.t

(** Render the body arena as debug text. *)
val to_string: t -> string
