open Std

type typing_result = { tree : TypedTree.expression; diagnostics : string list }

val typecheck : string -> (typing_result, string) result
