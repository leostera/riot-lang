open Std
module HashSet = Collections.HashSet
module Lir = Types

type result = {
  contains_calls: bool;
  frame_required: bool;
  slot_names: string list;
}

type slots = {
  seen: string HashSet.t;
  ordered_rev: string list;
}

let empty_slots = fun () -> { seen = HashSet.create (); ordered_rev = [] }

let add_slot = fun slots name ->
  if HashSet.contains slots.seen name then
    slots
  else
    (
      let _ = HashSet.insert slots.seen name in
      { slots with ordered_rev = name :: slots.ordered_rev }
    )

let ordered_slots = fun slots -> List.rev slots.ordered_rev

let rec collect_operand_registers = fun slots operand ->
  match operand with
  | Lir.Operand.Register name -> add_slot slots name
  | Lir.Operand.Global _
  | Lir.Operand.Symbol_address _
  | Lir.Operand.Literal _ -> slots

let collect_callee_registers = fun slots callee ->
  match callee with
  | Lir.Callee.Direct _ -> slots
  | Lir.Callee.Indirect operand -> collect_operand_registers slots operand

let collect_instruction = fun (contains_calls, slot_names) instruction ->
  match instruction with
  | Lir.Instruction.Label _
  | Lir.Instruction.Comment _
  | Lir.Instruction.Jump _ ->
      (contains_calls, slot_names)
  | Lir.Instruction.Move { dst; src } ->
      (contains_calls, add_slot (collect_operand_registers slot_names src) dst)
  | Lir.Instruction.Store_global { src; _ } ->
      (contains_calls, collect_operand_registers slot_names src)
  | Lir.Instruction.Call { dst; callee; arguments } ->
      let slot_names = collect_callee_registers slot_names callee in
      let slot_names = List.fold_left collect_operand_registers slot_names arguments in
      let slot_names =
        match dst with
        | Some name -> add_slot slot_names name
        | None -> slot_names
      in
      (true, slot_names)
  | Lir.Instruction.Branch_if_zero { operand; _ } ->
      (contains_calls, collect_operand_registers slot_names operand)
  | Lir.Instruction.Return operand ->
      (
        contains_calls,
        Option.map (collect_operand_registers slot_names) operand |> Option.unwrap_or ~default:slot_names
      )

let analyze_procedure = fun (procedure: Lir.Procedure.t) ->
  let contains_calls, slot_names = List.fold_left
    collect_instruction
    (false, List.fold_left add_slot (empty_slots ()) procedure.params)
    procedure.body in
  let slot_names = ordered_slots slot_names in
  let frame_required = contains_calls || slot_names <> [] in
  { contains_calls; frame_required; slot_names }
