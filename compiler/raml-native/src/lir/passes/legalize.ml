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
module Compilation_context = Raml_core.Compilation_context
module Compiler_target = Raml_core.Target

let value_scratch = Lir.Home.Register "x9"

let callee_scratch = Lir.Home.Register "x16"

let return_register = Lir.Home.Register "x0"

let value_destination = Lir.Destination.Home value_scratch

let callee_destination = Lir.Destination.Home callee_scratch

let return_destination = Lir.Destination.Home return_register

let value_operand = Lir.Operand.Home value_scratch

let callee_operand = Lir.Operand.Home callee_scratch

let return_operand = Lir.Operand.Home return_register

let needs_value_reload = fun operand ->
  match operand with
  | Lir.Operand.Home (Lir.Home.Register _) -> false
  | Lir.Operand.Literal _ -> false
  | Lir.Operand.Home (Lir.Home.Stack_slot _)
  | Lir.Operand.Global _
  | Lir.Operand.Symbol_address _ -> true
  | Lir.Operand.Register name -> panic
    (format Format.[ str "unassigned virtual register reached legalize: "; str name ])

let legalize_instruction = fun instruction ->
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
    (String.equal name "x0") -> [
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

let rewrite_procedure = fun (procedure: Lir.Procedure.t) ->
  { procedure with body = List.concat_map legalize_instruction procedure.body }

let supports_aarch64_darwin_legalization = fun (target: Compiler_target.t) ->
  String.equal target.architecture "aarch64"
  && String.equal target.vendor "apple"
  && String.equal target.system "darwin"

let program = fun ~ctx (program: Lir.Program.t) ->
  if supports_aarch64_darwin_legalization (Compilation_context.target ctx) then
    { program with procedures = List.map rewrite_procedure program.procedures }
  else
    program
