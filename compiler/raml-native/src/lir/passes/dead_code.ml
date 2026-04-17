(** This pass uses [LIR] liveness to drop local writes whose values are never
    observed.

    It walks the linear instruction stream with the per-instruction live-after
    sets from [Liveness], removes [Move] instructions whose destination is dead
    after the instruction, and rewrites [Call] instructions to discard their
    result when the destination is dead but the call itself must still happen.

    The effect is that later passes and the emitter stop carrying obviously
    dead result traffic, especially pointless stores of call results and local
    copies that never feed another instruction.

    The rationale is the same as [asmcomp]'s early dead-code cleanup: do the
    cheap liveness-driven trimming before frame analysis and home assignment, so
    later passes do less work and frame layout does not count dead values. *)
open Std
module HashSet = Collections.HashSet
module Lir = Types

let destination_name = fun destination ->
  match destination with
  | Lir.Destination.Register name -> Some name
  | Lir.Destination.Home _ -> None

let rewrite_point = fun (point: Liveness.point) ->
  match point.instruction with
  | Lir.Instruction.Move { dst; _ } -> (
      match destination_name dst with
      | Some name when not (HashSet.contains point.live_after ~value:name) -> None
      | Some _
      | None -> Some point.instruction
    )
  | Lir.Instruction.Call { dst=Some dst; callee; arguments } -> (
      match destination_name dst with
      | Some name when not (HashSet.contains point.live_after ~value:name) -> Some (Lir.Instruction.Call {
        dst = None;
        callee;
        arguments
      })
      | Some _
      | None -> Some point.instruction
    )
  | _ ->
      Some point.instruction

let rewrite_procedure = fun (procedure: Lir.Procedure.t) ->
  let body = Liveness.points_of_procedure procedure |> List.filter_map ~fn:rewrite_point in
  { procedure with body }

let program = fun (program: Lir.Program.t) ->
  { program with procedures = List.map program.procedures ~fn:rewrite_procedure }
