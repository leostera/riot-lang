(** This pass assigns homes to virtual values after frame layout is already
    known.

    The algorithm reuses the per-procedure analysis gathered during
    [layout_frames], zips the ordered virtual names with the ordered stack
    slots, and records that mapping as explicit frame homes.

    The effect is that the compiler has a real seam between “what does the
    frame look like?” and “where does each virtual value live?”.

    The rationale is to keep location assignment separate from frame layout so
    later work can replace this simple stack-only mapping with a richer
    allocator without rewriting frame construction again. *)
open Std
module HashMap = Collections.HashMap
module Lir = Types

type analysis = Layout_frames.analysis

let names_for_procedure = fun analysis procedure_name ->
  HashMap.get analysis procedure_name
  |> Option.map (fun (result: Frame_analysis.result) -> result.virtual_names)
  |> Option.expect
    ~msg:(format Format.[ str "missing frame analysis for procedure "; str procedure_name ])

let rec zip_homes = fun names slots ->
  match (names, slots) with
  | [], [] -> []
  | name :: rest_names, slot :: rest_slots -> Lir.Home_binding.{
    name;
    home = Lir.Home.Stack_slot slot
  }
  :: zip_homes rest_names rest_slots
  | _ -> panic "allocate_homes: frame slot count does not match analyzed virtual count"

let rewrite_procedure = fun analysis (procedure: Lir.Procedure.t) ->
  let virtual_names = names_for_procedure analysis procedure.name in
  let homes = zip_homes virtual_names procedure.frame.slots in
  { procedure with frame = { procedure.frame with homes } }

let program = fun ~analysis (program: Lir.Program.t) ->
  { program with procedures = List.map (rewrite_procedure analysis) program.procedures }
