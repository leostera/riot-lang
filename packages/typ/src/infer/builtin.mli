(**
   Built-in nominal types known by the inferencer.

   These are still ordinary `Ast.Type.Apply` values. The module only centralizes
   the canonical surface identifiers used by the checker while the real
   prelude/environment story is still being built.
*)

(** Built-in `int` type identifier. *)
val int_ident: Ast.ident

(** Built-in `bool` type identifier. *)
val bool_ident: Ast.ident

(** Built-in `float` type identifier. *)
val float_ident: Ast.ident

(** Built-in `char` type identifier. *)
val char_ident: Ast.ident

(** Built-in `string` type identifier. *)
val string_ident: Ast.ident

(** Built-in `unit` type identifier. *)
val unit_ident: Ast.ident

(** Built-in `list` type identifier. *)
val list_ident: Ast.ident

(** Built-in `int` type. *)
val int: Ast.Type.t

(** Built-in `bool` type. *)
val bool: Ast.Type.t

(** Built-in `float` type. *)
val float: Ast.Type.t

(** Built-in `char` type. *)
val char: Ast.Type.t

(** Built-in `string` type. *)
val string: Ast.Type.t

(** Built-in `unit` type. *)
val unit: Ast.Type.t

(** Build a `list` type application. *)
val list: Ast.Type.t -> Ast.Type.t

(** True when the identifier is the canonical `unit` type/constructor name. *)
val is_unit: Ast.ident -> bool
