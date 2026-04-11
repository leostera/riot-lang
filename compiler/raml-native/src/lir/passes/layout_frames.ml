(** This pass computes the frame facts that exist before home allocation. It
    asks [Frame_analysis] whether a procedure performs calls, records the
    ordered virtual names that later allocation will care about, and attaches a
    frame skeleton to the procedure.

    The effect is that later passes do not need to rediscover "does this
    procedure contain calls?" or "which virtual names exist?" from the raw
    instruction stream.

    The rationale is to keep frame analysis separate from location assignment.
    Final slot counts and frame size should be decided after home allocation,
    not baked in before any allocator has run. *)
open Std
module HashMap = Collections.HashMap
module Lir = Types

type analysis = (string, Frame_analysis.result) HashMap.t

let layout_of_procedure = fun (analysis: Frame_analysis.result) ->
  Lir.Frame.{
    contains_calls = analysis.contains_calls;
    frame_required = analysis.contains_calls;
    slots = [];
    homes = [];
    saved_registers = [];
    frame_size = 0;
  }

let analyze_program = fun (program: Lir.Program.t) ->
  let analysis = HashMap.with_capacity (List.length program.procedures) in
  List.iter
    (fun (procedure: Lir.Procedure.t) ->
      let _ = HashMap.insert analysis procedure.name (Frame_analysis.analyze_procedure procedure) in
      ())
    program.procedures;
  analysis

let frame_for_procedure = fun analysis (procedure: Lir.Procedure.t) ->
  HashMap.get analysis procedure.name
  |> Option.map layout_of_procedure
  |> Option.expect
    ~msg:(format Format.[ str "missing frame analysis for procedure "; str procedure.name ])

let virtual_names_for_procedure = fun analysis ~procedure_name ->
  HashMap.get analysis procedure_name
  |> Option.map (fun (result: Frame_analysis.result) -> result.virtual_names)
  |> Option.expect
    ~msg:(format Format.[ str "missing frame analysis for procedure "; str procedure_name ])

let program_with_analysis = fun (program: Lir.Program.t) ->
  let analysis = analyze_program program in
  (
    {
      program
      with procedures = List.map
        (fun (procedure: Lir.Procedure.t) ->
          { procedure with frame = frame_for_procedure analysis procedure })
        program.procedures
    },
    analysis
  )

let program = fun program -> fst (program_with_analysis program)
