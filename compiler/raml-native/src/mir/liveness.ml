open Std
module HashSet = Collections.HashSet
module Operand = Types.Operand
module Callee = Types.Callee
module Instruction = Types.Instruction

type live_set = string HashSet.t

let empty = HashSet.create

let mem = fun live name ->
  HashSet.contains live name

let copy = fun live -> HashSet.of_list (HashSet.to_list live)

let add = fun live name ->
  let next = copy live in
  let _ = HashSet.insert next name in
  next

let remove = fun live name ->
  let next = copy live in
  let _ = HashSet.remove next name in
  next

let union = fun left right ->
  HashSet.union left right

let of_operand = fun operand ->
  match operand with
  | Operand.Register name -> HashSet.of_list [ name ]
  | Operand.Global _
  | Operand.Symbol_address _
  | Operand.Literal _ -> empty ()

let of_callee = fun callee ->
  match callee with
  | Callee.Direct _ -> empty ()
  | Callee.Indirect operand -> of_operand operand

let of_operands = fun operands ->
  List.fold_left (fun live operand -> union live (of_operand operand)) (empty ()) operands

let rec before_instruction = fun ~after instruction ->
  match instruction with
  | Instruction.Move { dst; src } ->
      union (remove after dst) (of_operand src)
  | Instruction.Store_global { src; _ } ->
      union after (of_operand src)
  | Instruction.Call { dst; callee; arguments } ->
      union
        (
          union
            (
              match dst with
              | Some dst -> remove after dst
              | None -> after
            )
            (of_callee callee)
        )
        (of_operands arguments)
  | Instruction.If_then_else if_then_else ->
      union
        (union
          (before_instructions ~after if_then_else.then_)
          (before_instructions ~after if_then_else.else_))
        (of_operand if_then_else.condition)
  | Instruction.Return operand -> (
      match operand with
      | Some operand -> of_operand operand
      | None -> empty ()
    )
  | Instruction.Comment _ ->
      after

and before_instructions = fun ~after instructions ->
  List.fold_right
    (fun instruction live_after -> before_instruction ~after:live_after instruction)
    instructions
    after
