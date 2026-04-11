open Std
module Lir = Types

let pointer_width = 8

let add_unique = fun names name ->
  if List.exists (String.equal name) names then
    names
  else
    names @ [ name ]

let align_to = fun value ~alignment ->
  if value mod alignment = 0 then
    value
  else
    value + (alignment - (value mod alignment))

let rec collect_operand_registers = fun names operand ->
  match operand with
  | Lir.Operand.Register name -> add_unique names name
  | Lir.Operand.Global _ -> names
  | Lir.Operand.Symbol_address _ -> names
  | Lir.Operand.Literal _ -> names

let collect_callee_registers = fun names callee ->
  match callee with
  | Lir.Callee.Direct _ -> names
  | Lir.Callee.Indirect operand -> collect_operand_registers names operand

let collect_instruction_registers = fun names instruction ->
  match instruction with
  | Lir.Instruction.Label _ ->
      names
  | Lir.Instruction.Comment _ ->
      names
  | Lir.Instruction.Move { dst; src } ->
      add_unique (collect_operand_registers names src) dst
  | Lir.Instruction.Store_global { src; _ } ->
      collect_operand_registers names src
  | Lir.Instruction.Call { dst; callee; arguments } ->
      let names = collect_callee_registers names callee in
      let names = List.fold_left collect_operand_registers names arguments in
      (
        match dst with
        | Some name -> add_unique names name
        | None -> names
      )
  | Lir.Instruction.Branch_if_zero { operand; _ } ->
      collect_operand_registers names operand
  | Lir.Instruction.Jump _ ->
      names
  | Lir.Instruction.Return operand ->
      Option.map (collect_operand_registers names) operand |> Option.unwrap_or ~default:names

let layout_of_procedure = fun (procedure: Lir.Procedure.t) ->
  let slot_names = List.fold_left collect_instruction_registers procedure.params procedure.body in
  let slots =
    List.mapi (fun index name -> Lir.Slot.{ name; offset = index * pointer_width }) slot_names
  in
  let frame_size = align_to (List.length slots * pointer_width) ~alignment:16 in
  Lir.Frame.{ slots; frame_size }

let program = fun (program: Lir.Program.t) ->
  {
    program
    with procedures = List.map
      (fun (procedure: Lir.Procedure.t) -> { procedure with frame = layout_of_procedure procedure })
      program.procedures
  }
