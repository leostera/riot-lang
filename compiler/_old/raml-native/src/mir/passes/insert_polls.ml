(** This pass makes runtime polling explicit in [MIR]. It walks the structured
    instruction tree and inserts a synthetic [raml_poll] call before every
    non-poll call, including calls nested under conditionals. That turns an
    implicit runtime obligation into ordinary IR, which means later passes and
    snapshots can reason about it directly instead of hiding it in emission. *)
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
  List.flat_map instructions ~fn:insert_instruction

let insert_polls_procedure = fun (procedure: Procedure.t) ->
  { procedure with body = insert_instructions procedure.body }

let program = fun (program: Program.t) ->
  { program with procedures = List.map program.procedures ~fn:insert_polls_procedure }
