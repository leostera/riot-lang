open Std

type atom = { predicate : string; args : Term.t list }

type clause =
  | Atom of atom
  | Negated of atom
  | Builtin of string * Term.t list

type rule = { head : atom; body : clause list }

type program = { facts : atom list; rules : rule list }

type query = 
  | Single of atom
  | Multi of clause list

(* Constructors *)

let atom ~predicate ~args = { predicate; args }

let rule ~head ~body = { head; body }

let program ~facts ~rules = { facts; rules }

(* Predicates *)

let is_ground atom = List.for_all Term.is_const atom.args

let vars_in_atom atom =
  atom.args |> List.map Term.vars |> List.flatten
  |> List.sort_uniq String.compare

let rec vars_in_clause = function
  | Atom a -> vars_in_atom a
  | Negated a -> vars_in_atom a
  | Builtin (_, terms) ->
      terms |> List.map Term.vars |> List.flatten
      |> List.sort_uniq String.compare

let vars_in_rule rule =
  let head_vars = vars_in_atom rule.head in
  let body_vars =
    rule.body |> List.map vars_in_clause |> List.flatten
    |> List.sort_uniq String.compare
  in
  List.sort_uniq String.compare (head_vars @ body_vars)

(* Conversion *)

let atom_to_string atom =
  let args_str = atom.args |> List.map Term.to_string |> String.concat ", " in
  atom.predicate ^ "(" ^ args_str ^ ")"

let clause_to_string = function
  | Atom a -> atom_to_string a
  | Negated a -> "!" ^ atom_to_string a
  | Builtin (op, terms) ->
      let terms_str = terms |> List.map Term.to_string |> String.concat ", " in
      op ^ "(" ^ terms_str ^ ")"

let rule_to_string rule =
  let head_str = atom_to_string rule.head in
  let body_str =
    rule.body |> List.map clause_to_string |> String.concat ", "
  in
  head_str ^ " :- " ^ body_str ^ "."
