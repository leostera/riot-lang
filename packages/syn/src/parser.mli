open Std

(** Parse a complete program from tokens *)
val parse_program : Token.t list -> (Ast.program, Parse_stream.error list) result

(** Parse a single expression *)
val parse_expr : Token.t list -> (Ast.expr, Parse_stream.error list) result

(** Parse a type expression *)
val parse_type : Token.t list -> (Ast.type_expr, Parse_stream.error list) result