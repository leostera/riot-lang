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
module Lir = Types
module Target_profile = Target_profile

let register_home = fun name -> Lir.Home.Register name

let register_destination = fun name -> Lir.Destination.Home (register_home name)

let register_operand = fun name -> Lir.Operand.Home (register_home name)

let home_of_name = fun (frame: Lir.Frame.t) name ->
  frame.homes
  |> List.find ~fn:(fun (binding: Lir.Home_binding.t) -> String.equal binding.name name)
  |> Option.map ~fn:(fun (binding: Lir.Home_binding.t) -> binding.home)
  |> Option.expect ~msg:(format Format.[ str "missing home for "; str name ])

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

let rec zip_prefix left right =
  match (left, right) with
  | (left :: left_rest, right :: right_rest) -> (left, right) :: zip_prefix left_rest right_rest
  | _ -> []

let parameter_moves = fun profile (procedure: Lir.Procedure.t) ->
  zip_prefix profile.Target_profile.argument_registers procedure.params
  |> List.flat_map
    ~fn:(fun (register, name) ->
      move_if_needed
        ~dst:(Lir.Destination.Home (home_of_name procedure.frame name))
        ~src:(register_operand register))

let rewrite_call = fun profile dst callee arguments ->
  let argument_pairs = zip_prefix profile.Target_profile.argument_registers arguments in
  let argument_moves = argument_pairs
  |> List.flat_map
    ~fn:(fun (register, argument) -> move_if_needed ~dst:(register_destination register) ~src:argument) in
  let rewritten_call = Lir.Instruction.Call {
    dst = None;
    callee;
    arguments = List.map argument_pairs ~fn:(fun (register, _) -> register_operand register)
  } in
  let result_moves =
    match dst with
    | None -> []
    | Some dst -> move_if_needed ~dst ~src:(register_operand profile.Target_profile.return_register)
  in
  argument_moves @ [ rewritten_call ] @ result_moves

let rewrite_instruction = fun profile instruction ->
  match instruction with
  | Lir.Instruction.Call { dst; callee; arguments } -> rewrite_call profile dst callee arguments
  | Lir.Instruction.Label _
  | Lir.Instruction.Comment _
  | Lir.Instruction.Move _
  | Lir.Instruction.Store_global _
  | Lir.Instruction.Branch_if_zero _
  | Lir.Instruction.Jump _
  | Lir.Instruction.Return _ -> [ instruction ]

let rewrite_body = fun profile (procedure: Lir.Procedure.t) ->
  let body = List.flat_map procedure.body ~fn:(rewrite_instruction profile) in
  match body with
  | Lir.Instruction.Label entry :: rest when String.equal entry procedure.name -> Lir.Instruction.Label entry
  :: parameter_moves profile procedure
  @ rest
  | _ -> parameter_moves profile procedure @ body

let rewrite_procedure = fun profile (procedure: Lir.Procedure.t) ->
  { procedure with body = rewrite_body profile procedure }

let program = fun ~ctx (program: Lir.Program.t) ->
  match Target_profile.from_context ctx with
  | None -> program
  | Some profile -> {
    program
    with procedures = List.map program.procedures ~fn:(rewrite_procedure profile)
  }
