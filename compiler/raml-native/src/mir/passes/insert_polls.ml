(** Insert explicit runtime polling calls into [MIR].

    The current policy is simple and intentionally conservative: every non-poll
    call site gets a synthetic [raml_poll] call immediately before it, and
    structured conditionals are rewritten recursively so the policy applies in
    both branches.

    This keeps polling visible in snapshots and gives later MIR/LIR passes a
    concrete call site to optimize around. *)
open Std
module Program = Types.Program
module Procedure = Types.Procedure
module Instruction = Types.Instruction
module Callee = Types.Callee

let poll_call = Instruction.Call { dst = None; callee = Callee.Direct "raml_poll"; arguments = [] }

let rec insert_instruction = fun instruction ->
  match instruction with
  | Instruction.Call { callee=Callee.Direct "raml_poll"; _ } -> [ instruction ]
  | Instruction.Call _ -> [ poll_call; instruction ]
  | Instruction.If_then_else if_then_else -> [
    Instruction.If_then_else Instruction.{
      condition = if_then_else.condition;
      then_ = insert_instructions if_then_else.then_;
      else_ = insert_instructions if_then_else.else_
    };
  ]
  | instruction -> [ instruction ]

and insert_instructions = fun instructions ->
  List.concat_map insert_instruction instructions

let insert_polls_procedure = fun (procedure: Procedure.t) ->
  { procedure with body = insert_instructions procedure.body }

let program = fun (program: Program.t) ->
  { program with procedures = List.map insert_polls_procedure program.procedures }
