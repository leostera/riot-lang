(** This pass rewrites home-assigned [LIR] into forms the AArch64 Darwin
    emitter can treat as direct target operations instead of ad hoc reload
    cases.

    It introduces explicit scratch-register moves for cases like stack-to-stack
    copies, stack-backed indirect callees, zero-branches on non-register
    operands, and returns from non-`x0` locations.

    The effect is that the emitter stops inventing these reloads on demand and
    instead consumes an instruction stream that already makes temporary value
    movement visible in snapshots.

    The rationale is the same as [asmcomp]'s reload layer: after allocation,
    there is still target-specific legalization work to do before emission, and
    that work belongs in the compiler pipeline, not hidden inside the renderer.
    The full compilation context is the input for exactly that reason. *)
open Std
module Lir = Types
module Target_profile = Target_profile

let home_register = fun name -> Lir.Home.Register name

let destination_register = fun name -> Lir.Destination.Home (home_register name)

let operand_register = fun name -> Lir.Operand.Home (home_register name)

let needs_value_reload = fun operand ->
  match operand with
  | Lir.Operand.Home (Lir.Home.Register _) -> false
  | Lir.Operand.Literal _ -> false
  | Lir.Operand.Home (Lir.Home.Stack_slot _)
  | Lir.Operand.Global _
  | Lir.Operand.Symbol_address _ -> true
  | Lir.Operand.Register name -> panic
    (format Format.[ str "unassigned virtual register reached legalize: "; str name ])

let legalize_instruction = fun profile instruction ->
  let value_destination = destination_register profile.Target_profile.value_scratch_register in
  let callee_destination = destination_register profile.Target_profile.callee_scratch_register in
  let return_destination = destination_register profile.Target_profile.return_register in
  let value_operand = operand_register profile.Target_profile.value_scratch_register in
  let callee_operand = operand_register profile.Target_profile.callee_scratch_register in
  let return_operand = operand_register profile.Target_profile.return_register in
  match instruction with
  | Lir.Instruction.Move {
    dst=Lir.Destination.Home (Lir.Home.Stack_slot _ as dst);
    src=Lir.Operand.Home (Lir.Home.Stack_slot _ as src);

  } -> [
    Lir.Instruction.Move { dst = value_destination; src = Lir.Operand.Home src };
    Lir.Instruction.Move { dst = Lir.Destination.Home dst; src = value_operand };
  ]
  | Lir.Instruction.Call { dst; callee=Lir.Callee.Indirect operand; arguments } when needs_value_reload
    operand -> [
    Lir.Instruction.Move { dst = callee_destination; src = operand };
    Lir.Instruction.Call { dst; callee = Lir.Callee.Indirect callee_operand; arguments };
  ]
  | Lir.Instruction.Branch_if_zero { operand; target } when needs_value_reload operand -> [
    Lir.Instruction.Move { dst = value_destination; src = operand };
    Lir.Instruction.Branch_if_zero { operand = value_operand; target };
  ]
  | Lir.Instruction.Return (Some operand) when needs_value_reload operand -> [
    Lir.Instruction.Move { dst = return_destination; src = operand };
    Lir.Instruction.Return (Some return_operand);
  ]
  | Lir.Instruction.Return (Some (Lir.Operand.Home (Lir.Home.Register name as home))) when not
    (String.equal name profile.Target_profile.return_register) -> [
    Lir.Instruction.Move { dst = return_destination; src = Lir.Operand.Home home };
    Lir.Instruction.Return (Some return_operand);
  ]
  | Lir.Instruction.Label _
  | Lir.Instruction.Comment _
  | Lir.Instruction.Move _
  | Lir.Instruction.Store_global _
  | Lir.Instruction.Call _
  | Lir.Instruction.Branch_if_zero _
  | Lir.Instruction.Jump _
  | Lir.Instruction.Return _ -> [ instruction ]

let rewrite_procedure = fun profile (procedure: Lir.Procedure.t) ->
  { procedure with body = List.flat_map procedure.body ~fn:(legalize_instruction profile) }

let program = fun ~ctx (program: Lir.Program.t) ->
  match Target_profile.from_context ctx with
  | None -> program
  | Some profile -> {
    program
    with procedures = List.map program.procedures ~fn:(rewrite_procedure profile)
  }
