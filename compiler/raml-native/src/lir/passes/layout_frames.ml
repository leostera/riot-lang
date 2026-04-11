(** This pass computes explicit frame metadata for each [LIR] procedure. It
    asks [Frame_analysis] whether the procedure needs a frame and whether it
    performs calls, assigns stable stack-slot offsets for the virtual values
    that need storage, and finishes by computing an aligned frame size. That
    leaves later passes and emitters with concrete frame metadata to consume
    instead of forcing them to rediscover layout from the instruction stream. *)
open Std
module HashMap = Collections.HashMap
module Lir = Types

type analysis = (string, Frame_analysis.result) HashMap.t

let pointer_width = 8

let align_to = fun value ~alignment ->
  if value mod alignment = 0 then
    value
  else
    value + (alignment - (value mod alignment))

let layout_of_procedure = fun (analysis: Frame_analysis.result) ->
  let slots =
    List.mapi (fun index _name -> Lir.Slot.{ index; offset = index * pointer_width }) analysis.virtual_names
  in
  let frame_size = align_to (List.length slots * pointer_width) ~alignment:16 in
  Lir.Frame.{
    contains_calls = analysis.contains_calls;
    frame_required = analysis.frame_required;
    slots;
    homes = [];
    frame_size;
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
