open Std
module HashSet = Collections.HashSet
module Lir = Types

type result = {
  contains_calls: bool;
  virtual_names: string list;
}

type slots = {
  seen: string HashSet.t;
  ordered_rev: string list;
}

let empty_slots = fun () -> { seen = HashSet.create (); ordered_rev = [] }

let add_slot = fun slots name ->
  if HashSet.contains slots.seen ~value:name then
    slots
  else
    (
      let _ = HashSet.insert slots.seen ~value:name in
      { slots with ordered_rev = name :: slots.ordered_rev }
    )

let ordered_slots = fun slots -> List.rev slots.ordered_rev

let rec collect_operand_registers = fun slots operand ->
  match operand with
  | Lir.Operand.Register name -> add_slot slots name
  | Lir.Operand.Home _
  | Lir.Operand.Global _
  | Lir.Operand.Symbol_address _
  | Lir.Operand.Literal _ -> slots

let collect_callee_registers = fun slots callee ->
  match callee with
  | Lir.Callee.Direct _ -> slots
  | Lir.Callee.Indirect operand -> collect_operand_registers slots operand

let collect_instruction = fun (contains_calls, virtual_names) instruction ->
  match instruction with
  | Lir.Instruction.Label _
  | Lir.Instruction.Comment _
  | Lir.Instruction.Jump _ ->
      (contains_calls, virtual_names)
  | Lir.Instruction.Move { dst; src } ->
      let virtual_names = collect_operand_registers virtual_names src in
      let virtual_names =
        match dst with
        | Lir.Destination.Register name -> add_slot virtual_names name
        | Lir.Destination.Home _ -> virtual_names
      in
      (contains_calls, virtual_names)
  | Lir.Instruction.Store_global { src; _ } ->
      (contains_calls, collect_operand_registers virtual_names src)
  | Lir.Instruction.Call { dst; callee; arguments } ->
      let virtual_names = collect_callee_registers virtual_names callee in
      let virtual_names = List.fold_left arguments ~init:virtual_names ~fn:collect_operand_registers in
      let virtual_names =
        match dst with
        | Some (Lir.Destination.Register name) -> add_slot virtual_names name
        | Some (Lir.Destination.Home _) -> virtual_names
        | None -> virtual_names
      in
      (true, virtual_names)
  | Lir.Instruction.Branch_if_zero { operand; _ } ->
      (contains_calls, collect_operand_registers virtual_names operand)
  | Lir.Instruction.Return operand ->
      (
        contains_calls,
        Option.map operand ~fn:(collect_operand_registers virtual_names)
        |> Option.unwrap_or ~default:virtual_names
      )

let analyze_procedure = fun (procedure: Lir.Procedure.t) ->
  let initial_slots = List.fold_left procedure.params ~init:(empty_slots ()) ~fn:add_slot in
  let contains_calls, virtual_names = List.fold_left
    procedure.body
    ~init:(false, initial_slots)
    ~fn:collect_instruction in
  let virtual_names = ordered_slots virtual_names in
  { contains_calls; virtual_names }
