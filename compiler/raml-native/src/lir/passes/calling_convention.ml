(** This pass applies the target calling convention after homes and
    target-owned reloads are already explicit.

    For the current AArch64 Darwin slice it inserts entry moves from incoming
    argument registers into the assigned parameter homes, rewrites call
    arguments to explicit pre-call moves into `x0`-`x7`, and materializes call
    results as explicit post-call moves from `x0`.

    The effect is that the emitter stops owning argument placement, parameter
    prologue moves, and call-result shuffling. Those ABI choices become normal
    compiler instructions that show up in snapshots.

    The rationale is the same one that drives the rest of this cleanup: the
    emitter should render target code, not decide calling-convention mechanics
    on the fly. The shared compilation context is the input because the pass is
    target-owned by design. *)
open Std
module Compilation_context = Raml_core.Compilation_context
module Compiler_target = Raml_core.Target
module Lir = Types

let supports_aarch64_apple_darwin = fun (target: Compiler_target.t) ->
  String.equal target.architecture "aarch64"
  && String.equal target.vendor "apple"
  && String.equal target.system "darwin"

let register_home = fun index -> Lir.Home.Register (format Format.[ str "x"; int index ])

let register_destination = fun index -> Lir.Destination.Home (register_home index)

let register_operand = fun index -> Lir.Operand.Home (register_home index)

let home_of_name = fun (frame: Lir.Frame.t) name ->
  frame.homes |> List.find_map
    (fun (binding: Lir.Home_binding.t) ->
      if String.equal binding.name name then
        Some binding.home
      else
        None) |> Option.expect ~msg:(format Format.[ str "missing home for "; str name ])

let destination_matches_operand = fun dst src ->
  match (dst, src) with
  | (Lir.Destination.Home (Lir.Home.Register left), Lir.Operand.Home (Lir.Home.Register right)) -> String.equal
    left
    right
  | (Lir.Destination.Home (Lir.Home.Stack_slot left), Lir.Operand.Home (Lir.Home.Stack_slot right)) -> Int.equal
    left.index
    right.index
  | _ -> false

let move_if_needed = fun ~dst ~src ->
  if destination_matches_operand dst src then
    []
  else
    [ Lir.Instruction.Move { dst; src } ]

let parameter_moves = fun (procedure: Lir.Procedure.t) ->
  procedure.params
  |> List.mapi
    (fun index name ->
      move_if_needed
        ~dst:(Lir.Destination.Home (home_of_name procedure.frame name))
        ~src:(register_operand index))
  |> List.concat

let rewrite_call = fun dst callee arguments ->
  let argument_moves = arguments
  |> List.mapi (fun index argument -> move_if_needed ~dst:(register_destination index) ~src:argument)
  |> List.concat in
  let rewritten_call = Lir.Instruction.Call {
    dst = None;
    callee;
    arguments = List.mapi (fun index _ -> register_operand index) arguments
  } in
  let result_moves =
    match dst with
    | None -> []
    | Some dst -> move_if_needed ~dst ~src:(register_operand 0)
  in
  argument_moves @ [ rewritten_call ] @ result_moves

let rewrite_instruction = fun instruction ->
  match instruction with
  | Lir.Instruction.Call { dst; callee; arguments } -> rewrite_call dst callee arguments
  | Lir.Instruction.Label _
  | Lir.Instruction.Comment _
  | Lir.Instruction.Move _
  | Lir.Instruction.Store_global _
  | Lir.Instruction.Branch_if_zero _
  | Lir.Instruction.Jump _
  | Lir.Instruction.Return _ -> [ instruction ]

let rewrite_body = fun (procedure: Lir.Procedure.t) ->
  let body = List.concat_map rewrite_instruction procedure.body in
  match body with
  | Lir.Instruction.Label entry :: rest when String.equal entry procedure.name -> Lir.Instruction.Label entry
  :: parameter_moves procedure
  @ rest
  | _ -> parameter_moves procedure @ body

let rewrite_procedure = fun (procedure: Lir.Procedure.t) ->
  { procedure with body = rewrite_body procedure }

let program = fun ~ctx (program: Lir.Program.t) ->
  if supports_aarch64_apple_darwin (Compilation_context.target ctx) then
    { program with procedures = List.map rewrite_procedure program.procedures }
  else
    program
