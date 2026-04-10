open Std

(** Syntax target an edit should act on. *)
type target =
  | Node of Syn.Cst.syntax_node
  | Token of Syn.Cst.syntax_token
(** Replacement content for an edit.

    Use [SourceOfNode] or [SourceOfToken] when you want to preserve the exact
    original source slice. Use [Text] for literal replacement text.
*)
type replacement =
  | SourceOfNode of Syn.Cst.syntax_node
  | SourceOfToken of Syn.Cst.syntax_token
  | Text of string
(** One syntax-directed edit operation. *)
type operation =
  | Delete of { target: target }
  | Replace of { target: target; replacement: replacement }
  | InsertBefore of { anchor: target; content: replacement }
  | InsertAfter of { anchor: target; content: replacement }
  | Swap of { left: target; right: target }
(** A named fix composed of one or more edit operations. *)
type fix = {
  (** Human-readable fix title shown to users. *)
  title: string;
  (** Operations applied when the fix is executed. *)
  operations: operation list;
}
(** Concrete text edit produced after lowering a syntax-directed fix. *)
type text_edit = {
  span: Syn.Ceibo.Span.t;
  new_text: string;
}

(** Reuse the exact source slice covered by a syntax node. *)
val source_of_node: Syn.Cst.syntax_node -> replacement

(** Reuse the exact source slice covered by a syntax token. *)
val source_of_token: Syn.Cst.syntax_token -> replacement

(** Use literal text as replacement content. *)
val text: string -> replacement

(** Delete the given syntax target. *)
val delete: target:target -> operation

(** Delete a syntax node. *)
val delete_node: Syn.Cst.syntax_node -> operation

(** Replace a target with the given replacement content. *)
val replace: target:target -> replacement:replacement -> operation

(** Replace one syntax node with the exact source of another node. *)
val replace_node: target:Syn.Cst.syntax_node -> replacement:Syn.Cst.syntax_node -> operation

(** Replace a syntax node with literal text. *)
val replace_node_with_text: target:Syn.Cst.syntax_node -> text:string -> operation

(** Replace a syntax token with literal text. *)
val replace_token_with_text: target:Syn.Cst.syntax_token -> text:string -> operation

(** Insert content immediately before the anchor. *)
val insert_before: anchor:target -> content:replacement -> operation

(** Insert content immediately after the anchor. *)
val insert_after: anchor:target -> content:replacement -> operation

(** Swap the source covered by two targets. *)
val swap: left:target -> right:target -> operation

(** Build a named fix from edit operations.

    Use the title to explain what will change when the fix is applied.
*)
val make: title:string -> operations:operation list -> fix

(** Return the fix title. *)
val title: fix -> string

(** Return the operations belonging to the fix. *)
val operations: fix -> operation list

(** Apply one operation directly to source text.

    This is useful when testing or debugging a single rewrite step.
*)
val apply_operation: source:string -> operation -> (string, string) result

(** Lower one fix into concrete text edits. *)
val lower_fix: source:string -> fix -> (text_edit list, string) result

(** Lower multiple fixes into concrete text edits. *)
val lower_fixes: source:string -> fix list -> (text_edit list, string) result

(** Apply one fix directly to source text.

    Example:
    ```ocaml
    let op = Fix.replace_node_with_text ~target:node ~text:"()" in
    let fix = Fix.make ~title:"replace with unit" ~operations:[ op ] in
    Fix.apply_fix ~source fix
    ```
*)
val apply_fix: source:string -> fix -> (string, string) result

(** Apply multiple fixes directly to source text. *)
val apply_fixes: source:string -> fix list -> (string, string) result

(** Check whether a fix can be safely lowered and applied to the source. *)
val validate_fix: source:string -> fix -> (unit, string) result
