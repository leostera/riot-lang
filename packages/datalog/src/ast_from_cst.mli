open Std

(** {1 AST From CST - Convert Parser CST to Datalog AST}
    
    Converts the Ceibo Green tree produced by the parser into
    our typed AST representation.
*)

(** Convert a parsed program (CST) into an AST program *)
val program_of_cst :
  (Parser.Syntax_kind.t, string) Ceibo.Green.node -> 
  (Ast.program, string) Result.t

(** Convert a parsed query (CST) into an AST query.
    Handles both single-atom and multi-atom queries:
    - Single: person(X) → Single atom
    - Multi: parent(X,Y), age(X,A) → Multi [Atom ...; Atom ...]
*)
val query_of_cst :
  (Parser.Syntax_kind.t, string) Ceibo.Green.node -> 
  (Ast.query, string) Result.t
