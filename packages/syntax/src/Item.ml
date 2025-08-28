open Sexplib.Std

type literal = String of string [@@deriving sexp]
type extension = unit [@@deriving sexp]
type annotation = unit [@@deriving sexp]
type pattern = Bind of Symbol.t [@@deriving sexp]

type expression = Var of Symbol.t | Let of let_binding | Literal of literal
[@@deriving sexp]

and let_binding = {
  ext : extension option;
  annot : annotation option;
  pat : pattern;
  expr : expression;
}
[@@deriving sexp]

type item_kind = Let of let_binding [@@deriving sexp]
type t = Item of { kind : item_kind } [@@deriving sexp]
type items = t list [@@deriving sexp]

let pp ppf item =
  let sexp = sexp_of_t item in
  Format.fprintf ppf "%a" (Sexplib.Sexp.pp_hum_indent 2) sexp

let pp_items ppf items =
  let sexp = sexp_of_items items in
  Format.fprintf ppf "%a" (Sexplib.Sexp.pp_hum_indent 2) sexp
