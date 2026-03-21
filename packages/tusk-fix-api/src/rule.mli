open Std

type t
type green_tree = (Syn.SyntaxKind.t, string) Syn.Ceibo.Green.node
type red_tree = (Syn.SyntaxKind.t, string) Syn.Ceibo.Red.syntax_node
type context = {
  file_path : string;
  cst : Syn.Cst.source_file option;
}

val make :
  id:string ->
  ?code:string ->
  name:string ->
  description:string ->
  ?message:string ->
  explain:string ->
  ?enabled:bool ->
  run:(context -> red_tree -> Diagnostic.t list) ->
  unit ->
  t

val id : t -> string
val code : t -> string option
val name : t -> string
val description : t -> string
val message : t -> string option
val explain : t -> string
val explanation : t -> Explanation.t option
val enabled : t -> bool
val run : t -> context -> red_tree -> Diagnostic.t list
