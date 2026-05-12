(** This pass rewrites virtual register references in [LIR] into explicit
    homes. It uses the frame metadata from [layout_frames] to map each virtual
    name to a concrete stack slot, rewrites operands and destinations through
    that mapping, and leaves the program in the form the emitter should
    consume. The point is to make value locations a first-class compiler
    concern instead of an emitter convention. *)
open Std
module Lir = Types

let home_of_name = fun (frame: Lir.Frame.t) name ->
  frame.homes
  |> List.find ~fn:(fun (binding: Lir.Home_binding.t) -> String.equal binding.name name)
  |> Option.map ~fn:(fun (binding: Lir.Home_binding.t) -> binding.home)
  |> Option.expect ~msg:(format Format.[ str "missing home for register "; str name ])

let rewrite_operand = fun frame operand ->
  match operand with
  | Lir.Operand.Register name -> Lir.Operand.Home (home_of_name frame name)
  | Lir.Operand.Home _
  | Lir.Operand.Global _
  | Lir.Operand.Symbol_address _
  | Lir.Operand.Literal _ -> operand

let rewrite_destination = fun frame destination ->
  match destination with
  | Lir.Destination.Register name -> Lir.Destination.Home (home_of_name frame name)
  | Lir.Destination.Home _ -> destination

let rewrite_instruction = fun frame instruction ->
  match instruction with
  | Lir.Instruction.Label _
  | Lir.Instruction.Comment _
  | Lir.Instruction.Jump _ ->
      instruction
  | Lir.Instruction.Move { dst; src } ->
      Lir.Instruction.Move { dst = rewrite_destination frame dst; src = rewrite_operand frame src }
  | Lir.Instruction.Store_global { symbol; src } ->
      Lir.Instruction.Store_global { symbol; src = rewrite_operand frame src }
  | Lir.Instruction.Call { dst; callee; arguments } ->
      let callee =
        match callee with
        | Lir.Callee.Direct _ -> callee
        | Lir.Callee.Indirect operand -> Lir.Callee.Indirect (rewrite_operand frame operand)
      in
      Lir.Instruction.Call {
        dst = Option.map dst ~fn:(rewrite_destination frame);
        callee;
        arguments = List.map arguments ~fn:(rewrite_operand frame)
      }
  | Lir.Instruction.Branch_if_zero { operand; target } ->
      Lir.Instruction.Branch_if_zero { operand = rewrite_operand frame operand; target }
  | Lir.Instruction.Return operand ->
      Lir.Instruction.Return (Option.map operand ~fn:(rewrite_operand frame))

let rewrite_procedure = fun (procedure: Lir.Procedure.t) ->
  { procedure with body = List.map procedure.body ~fn:(rewrite_instruction procedure.frame) }

let program = fun (program: Lir.Program.t) ->
  { program with procedures = List.map program.procedures ~fn:rewrite_procedure }
