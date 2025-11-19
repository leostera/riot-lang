open Std

(** FromSyntax - Convert Syn CST to UntypedTree AST

    This module converts from Syn's lossless concrete syntax tree (CST) to our
    clean untyped abstract syntax tree (AST) for type checking. *)

type error =
  | UnexpectedNode of {
      expected : string;
      got : Syn.SyntaxKind.t;
      span : Syn.Ceibo.Span.t;
    }
  | MissingNode of { expected : string; span : Syn.Ceibo.Span.t }
  | UnsupportedFeature of { feature : string; span : Syn.Ceibo.Span.t }

val from_red_tree :
  (Syn.SyntaxKind.t, string) Syn.Ceibo.Red.syntax_node ->
  (UntypedTree.structure, error list) result
(** Convert a Syn red tree to UntypedTree structure. Returns either the
    structure or a list of conversion errors. *)

val from_parse_result :
  Syn.Parser.parse_result -> (UntypedTree.structure, error list) result
(** Convert a Syn parse result to UntypedTree structure. This is the main entry
    point for converting parsed OCaml code. *)
