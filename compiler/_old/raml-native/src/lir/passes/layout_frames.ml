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
  let analysis = HashMap.with_capacity ~size:(List.length program.procedures) in
  List.for_each
    program.procedures
    ~fn:(fun (procedure: Lir.Procedure.t) ->
      let _ =
        HashMap.insert
          analysis
          ~key:procedure.name
          ~value:(Frame_analysis.analyze_procedure procedure)
      in
      ());
  analysis

let frame_for_procedure = fun analysis (procedure: Lir.Procedure.t) ->
  HashMap.get analysis ~key:procedure.name
  |> Option.map ~fn:layout_of_procedure
  |> Option.expect
    ~msg:(format Format.[ str "missing frame analysis for procedure "; str procedure.name ])

let virtual_names_for_procedure = fun analysis ~procedure_name ->
  HashMap.get analysis ~key:procedure_name
  |> Option.map ~fn:(fun (result: Frame_analysis.result) -> result.virtual_names)
  |> Option.expect
    ~msg:(format Format.[ str "missing frame analysis for procedure "; str procedure_name ])

let program_with_analysis = fun (program: Lir.Program.t) ->
  let analysis = analyze_program program in
  (
    {
      program
      with procedures = List.map
        program.procedures
        ~fn:(fun (procedure: Lir.Procedure.t) ->
          { procedure with frame = frame_for_procedure analysis procedure })
    },
    analysis
  )

let program = fun program ->
  let program, _analysis = program_with_analysis program in
  program
