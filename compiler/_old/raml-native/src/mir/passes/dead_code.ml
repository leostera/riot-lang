(** This pass removes local work from [MIR] that no longer matters. It walks
    each procedure backwards using [Mir.Liveness], keeps instructions whose
    results are still live or whose effects must happen, drops dead pure moves
    and empty conditionals, and preserves calls while discarding unused result
    destinations. The goal is simple: once earlier passes have exposed
    redundancy, this pass strips the remaining temporary traffic before
    linearization. *)
open Std
module Program = Types.Program
module Procedure = Types.Procedure
module Instruction = Types.Instruction

let rec rewrite_instruction = fun live_after instruction ->
  match instruction with
  | Instruction.Move { dst; src } ->
      if Liveness.mem live_after dst then
        let live_before = Liveness.before_instruction
          ~after:live_after
          (Instruction.Move { dst; src }) in
        (Some instruction, live_before)
      else
        (None, live_after)
  | Instruction.Store_global _ ->
      (Some instruction, Liveness.before_instruction ~after:live_after instruction)
  | Instruction.Call { dst; callee; arguments } ->
      let live_before = Liveness.before_instruction ~after:live_after instruction in
      let dst =
        match dst with
        | Some dst when Liveness.mem live_after dst -> Some dst
        | _ -> None
      in
      (Some (Instruction.Call { dst; callee; arguments }), live_before)
  | Instruction.If_then_else if_then_else ->
      let then_, _ = rewrite_instructions if_then_else.then_ live_after in
      let else_, _ = rewrite_instructions if_then_else.else_ live_after in
      if then_ = [] && else_ = [] then
        (None, live_after)
      else
        let rewritten = Instruction.If_then_else Instruction.{
          condition = if_then_else.condition;
          then_;
          else_
        } in
        (Some rewritten, Liveness.before_instruction ~after:live_after rewritten)
  | Instruction.Return _ ->
      (Some instruction, Liveness.before_instruction ~after:live_after instruction)
  | Instruction.Comment _ ->
      (None, live_after)

and rewrite_instructions = fun instructions live_after ->
  List.fold_right instructions ~init:([], live_after)
    ~fn:(fun instruction (kept, live_after) ->
      let rewritten, live_before = rewrite_instruction live_after instruction in
      let kept =
        match rewritten with
        | Some instruction -> instruction :: kept
        | None -> kept
      in
      (kept, live_before))

let rewrite_procedure = fun (procedure: Procedure.t) ->
  let body, _ = rewrite_instructions procedure.body (Liveness.empty ()) in
  { procedure with body }

let program = fun (program: Program.t) ->
  { program with procedures = List.map program.procedures ~fn:rewrite_procedure }
