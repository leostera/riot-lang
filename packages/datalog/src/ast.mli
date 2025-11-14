open Std

(** {1 AST - Datalog Abstract Syntax Tree}
    
    Represents the structure of parsed Datalog programs.
*)

type atom = {
  predicate : string;     (** Predicate name: edge, path, person *)
  args : Term.t list;     (** Arguments: [Var "X"; Const (Int 42)] *)
}
(** An atom is a predicate applied to terms.
    Examples:
    - edge(1, 2)
    - person(X, "alice")
    - parent(X, Y)
*)

type clause =
  | Atom of atom                        (** Positive atom: edge(X, Y) *)
  | Negated of atom                     (** Negated atom: !edge(X, Y) *)
  | Builtin of string * Term.t list     (** Built-in: X = Y, X > 10 *)
(** A clause appears in rule bodies *)

type rule = {
  head : atom;            (** What we derive *)
  body : clause list;     (** Conditions *)
}
(** A rule: head :- body1, body2, ...
    Example: path(X, Z) :- edge(X, Y), path(Y, Z)
*)

type program = {
  facts : atom list;      (** Ground atoms - no variables *)
  rules : rule list;      (** Derivation rules *)
}
(** A complete Datalog program *)

type query = 
  | Single of atom              (** Single atom query: person(X) *)
  | Multi of clause list        (** Multi-atom query: parent(X,Y), age(X,A) *)
(** A query can be:
    - Single atom: person(X) 
    - Multiple atoms joined: parent(X, Y), age(X, A)
*)

(** {2 Constructors} *)

val atom : predicate:string -> args:Term.t list -> atom
(** Create an atom *)

val rule : head:atom -> body:clause list -> rule
(** Create a rule *)

val program : facts:atom list -> rules:rule list -> program
(** Create a program *)

(** {2 Predicates} *)

val is_ground : atom -> bool
(** Check if atom has no variables (is a fact) *)

val vars_in_atom : atom -> string list
(** Get all variable names in atom *)

val vars_in_clause : clause -> string list
(** Get all variable names in clause *)

val vars_in_rule : rule -> string list
(** Get all variable names in rule (head + body) *)

(** {2 Conversion} *)

val atom_to_string : atom -> string
(** Convert atom to string: edge(1, 2) *)

val rule_to_string : rule -> string
(** Convert rule to string: path(X,Z) :- edge(X,Y), path(Y,Z) *)

val clause_to_string : clause -> string
(** Convert clause to string *)
