open Std

module Program = Types.Program
module Procedure = Types.Procedure
module Instruction = Types.Instruction
module Operand = Types.Operand
module Callee = Types.Callee

type live_set = string list

let add_live = fun live name ->
  if List.exists (String.equal name) live then
    live
  else
    name :: live

let union_live = fun left right ->
  List.fold_left add_live left right

let remove_live = fun live name ->
  List.filter (fun current -> not (String.equal current name)) live

let live_of_operand = fun operand ->
  match operand with
  | Operand.Register name -> [ name ]
  | Operand.Global _
  | Operand.Symbol_address _
  | Operand.Literal _ -> []

let live_of_callee = fun callee ->
  match callee with
  | Callee.Direct _ -> []
  | Callee.Indirect operand -> live_of_operand operand

let live_of_operands = fun operands ->
  List.fold_left
    (fun live operand -> union_live live (live_of_operand operand))
    []
    operands

let rec rewrite_instruction = fun live_after instruction ->
  match instruction with
  | Instruction.Move { dst; src } ->
      if List.exists (String.equal dst) live_after then
        let live_before = union_live (remove_live live_after dst) (live_of_operand src) in
        (Some instruction, live_before)
      else
        (None, live_after)
  | Instruction.Store_global { src; _ } ->
      (Some instruction, union_live live_after (live_of_operand src))
  | Instruction.Call { dst; callee; arguments } ->
      let live_before =
        union_live
          (union_live
             (match dst with
             | Some dst -> remove_live live_after dst
             | None -> live_after)
             (live_of_callee callee))
          (live_of_operands arguments)
      in
      let dst =
        match dst with
        | Some dst when List.exists (String.equal dst) live_after -> Some dst
        | _ -> None
      in
      (Some (Instruction.Call { dst; callee; arguments }), live_before)
  | Instruction.If_then_else if_then_else ->
      let then_, live_then =
        rewrite_instructions if_then_else.then_ live_after
      in
      let else_, live_else =
        rewrite_instructions if_then_else.else_ live_after
      in
      if then_ = [] && else_ = [] then
        (None, live_after)
      else
        let live_before =
          union_live
            (union_live live_then live_else)
            (live_of_operand if_then_else.condition)
        in
        (Some (Instruction.If_then_else Instruction.{ condition = if_then_else.condition; then_; else_ }), live_before)
  | Instruction.Return operand ->
      let live_before =
        match operand with
        | Some operand -> live_of_operand operand
        | None -> []
      in
      (Some instruction, live_before)
  | Instruction.Comment _ ->
      (None, live_after)

and rewrite_instructions = fun instructions live_after ->
  List.fold_right
    (fun instruction (kept, live_after) ->
      let rewritten, live_before = rewrite_instruction live_after instruction in
      let kept =
        match rewritten with
        | Some instruction -> instruction :: kept
        | None -> kept
      in
      (kept, live_before))
    instructions
    ([], live_after)

let rewrite_procedure = fun (procedure: Procedure.t) ->
  let body, _ = rewrite_instructions procedure.body [] in
  { procedure with body }

let program = fun (program: Program.t) ->
  { program with procedures = List.map rewrite_procedure program.procedures }
