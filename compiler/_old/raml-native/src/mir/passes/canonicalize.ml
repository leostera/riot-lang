(** This pass cleans up the structured parts of [MIR] before the heavier MIR
    analyses run. It walks conditionals recursively, drops no-op moves, folds
    constant boolean branches, and erases conditionals whose branches are empty
    or identical. The result is a smaller, more regular tree, which makes the
    later dataflow-oriented passes cheaper and easier to trust. *)
open Std
module Program = Types.Program
module Procedure = Types.Procedure
module Instruction = Types.Instruction
module Operand = Types.Operand
module Literal = Types.Literal

let is_noop_move = fun instruction ->
  match instruction with
  | Instruction.Move { dst; src=Operand.Register src } -> String.equal dst src
  | _ -> false

let rec canonicalize_instruction = fun instruction ->
  match instruction with
  | Instruction.If_then_else if_then_else -> canonicalize_if_then_else if_then_else
  | instruction ->
      if is_noop_move instruction then
        []
      else
        [ instruction ]

and canonicalize_if_then_else = fun (if_then_else: Instruction.if_then_else) ->
  let then_ = canonicalize_instructions if_then_else.then_ in
  let else_ = canonicalize_instructions if_then_else.else_ in
  match if_then_else.condition with
  | Operand.Literal (Literal.Bool true) -> then_
  | Operand.Literal (Literal.Bool false) -> else_
  | _ when then_ = [] && else_ = [] -> []
  | _ when then_ = else_ -> then_
  | _ -> [
    Instruction.If_then_else Instruction.{ condition = if_then_else.condition; then_; else_ };
  ]

and canonicalize_instructions = fun instructions ->
  List.flat_map instructions ~fn:canonicalize_instruction

let canonicalize_procedure = fun (procedure: Procedure.t) ->
  { procedure with body = canonicalize_instructions procedure.body }

let program = fun (program: Program.t) ->
  { program with procedures = List.map program.procedures ~fn:canonicalize_procedure }
