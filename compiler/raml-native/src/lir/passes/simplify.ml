(** This pass applies a handful of cheap local rewrites to linear [LIR]. It
    scans left-to-right, removes no-op moves, folds constant conditional
    branches, drops branches that only jump to the next label, and turns
    [move; return] into a direct return of the moved operand. These are small
    wins, but they are worth taking before scheduling so later cleanup sees a
    simpler stream. *)
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

let zero_test_of_literal = fun literal ->
  match literal with
  | Literal.Unit -> Some true
  | Literal.Bool value -> Some (not value)
  | Literal.Int value -> Some (Int.equal value 0)
  | Literal.Float _
  | Literal.String _ -> None

let rec rewrite_instructions = fun instructions ->
  match instructions with
  | [] ->
      []
  | instruction :: rest when is_noop_move instruction ->
      rewrite_instructions rest
  | Instruction.Branch_if_zero { operand=Operand.Literal literal; target } :: rest -> (
      match zero_test_of_literal literal with
      | Some true -> Instruction.Jump target :: rewrite_instructions rest
      | Some false -> rewrite_instructions rest
      | None -> Instruction.Branch_if_zero { operand = Operand.Literal literal; target }
      :: rewrite_instructions rest
    )
  | Instruction.Branch_if_zero { target; _ } :: Instruction.Label next :: rest when String.equal
    target
    next ->
      Instruction.Label next :: rewrite_instructions rest
  | Instruction.Move { dst; src } :: Instruction.Return (Some (Operand.Register name)) :: rest when String.equal
    dst
    name ->
      Instruction.Return (Some src) :: rewrite_instructions rest
  | instruction :: rest ->
      instruction :: rewrite_instructions rest

let rewrite_procedure = fun (procedure: Procedure.t) ->
  { procedure with body = rewrite_instructions procedure.body }

let program = fun (program: Program.t) ->
  { program with procedures = List.map rewrite_procedure program.procedures }
