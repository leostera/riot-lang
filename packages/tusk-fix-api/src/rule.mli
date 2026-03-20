open Std

type t
type green_tree = (Syn.SyntaxKind.t, string) Syn.Ceibo.Green.node
type red_tree = (Syn.SyntaxKind.t, string) Syn.Ceibo.Red.syntax_node
type context = { file_path : string }

val make :
  id:string ->
  name:string ->
  description:string ->
  ?enabled:bool ->
  run:(context -> red_tree -> Diagnostic.t list) ->
  unit ->
  t

val id : t -> string
val name : t -> string
val description : t -> string
val enabled : t -> bool
val run : t -> context -> red_tree -> Diagnostic.t list
