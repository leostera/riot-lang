open Std

(** The syntax object an operation should act on. *)
(** What should replace the target.

    `Source_of_*` reuses the exact original source slice covered by the given
    syntax object. `Text` is the escape hatch for literal replacement text.
*)
type target =
  | Node of Syn.Cst.syntax_node
  | Token of Syn.Cst.syntax_token
(** A single syntax-directed operation. *)
type replacement =
  | SourceOfNode of Syn.Cst.syntax_node
  | SourceOfToken of Syn.Cst.syntax_token
  | Text of string
(** A fix that can be applied to source code. *)
type operation =
  | Delete of { target: target }
  | Replace of { target: target; replacement: replacement }
  | InsertBefore of { anchor: target; content: replacement }
  | InsertAfter of { anchor: target; content: replacement }
  | Swap of { left: target; right: target }
type fix = {
  title: string;
  operations: operation list;
}
type text_edit = {
  span: Syn.Ceibo.Span.t;
  new_text: string;
}
val source_of_node: Syn.Cst.syntax_node -> replacement

val source_of_token: Syn.Cst.syntax_token -> replacement

val text: string -> replacement

val delete: target:target -> operation

val delete_node: Syn.Cst.syntax_node -> operation

val replace: target:target -> replacement:replacement -> operation

val replace_node: target:Syn.Cst.syntax_node -> replacement:Syn.Cst.syntax_node -> operation

val replace_node_with_text: target:Syn.Cst.syntax_node -> text:string -> operation

val replace_token_with_text: target:Syn.Cst.syntax_token -> text:string -> operation

val insert_before: anchor:target -> content:replacement -> operation

val insert_after: anchor:target -> content:replacement -> operation

val swap: left:target -> right:target -> operation

val make: title:string -> operations:operation list -> fix

val title: fix -> string

val operations: fix -> operation list

val apply_operation: source:string -> operation -> (string, string) result

val lower_fix: source:string -> fix -> (text_edit list, string) result

val lower_fixes: source:string -> fix list -> (text_edit list, string) result

val apply_fix: source:string -> fix -> (string, string) result

val apply_fixes: source:string -> fix list -> (string, string) result

val validate_fix: source:string -> fix -> (unit, string) result
