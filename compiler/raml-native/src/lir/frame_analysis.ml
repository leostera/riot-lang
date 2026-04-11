open Std
module Lir = Types

type result = {
  contains_calls: bool;
  frame_required: bool;
  slot_names: string list;
}

let add_unique = fun names name ->
  if List.exists (String.equal name) names then
    names
  else
    names @ [ name ]

let rec collect_operand_registers = fun names operand ->
  match operand with
  | Lir.Operand.Register name -> add_unique names name
  | Lir.Operand.Global _
  | Lir.Operand.Symbol_address _
  | Lir.Operand.Literal _ -> names

let collect_callee_registers = fun names callee ->
  match callee with
  | Lir.Callee.Direct _ -> names
  | Lir.Callee.Indirect operand -> collect_operand_registers names operand

let collect_instruction = fun (contains_calls, slot_names) instruction ->
  match instruction with
  | Lir.Instruction.Label _
  | Lir.Instruction.Comment _
  | Lir.Instruction.Jump _ ->
      (contains_calls, slot_names)
  | Lir.Instruction.Move { dst; src } ->
      (contains_calls, add_unique (collect_operand_registers slot_names src) dst)
  | Lir.Instruction.Store_global { src; _ } ->
      (contains_calls, collect_operand_registers slot_names src)
  | Lir.Instruction.Call { dst; callee; arguments } ->
      let slot_names = collect_callee_registers slot_names callee in
      let slot_names = List.fold_left collect_operand_registers slot_names arguments in
      let slot_names =
        match dst with
        | Some name -> add_unique slot_names name
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
    (false, procedure.params)
    procedure.body in
  let frame_required = contains_calls || slot_names <> [] in
  { contains_calls; frame_required; slot_names }
