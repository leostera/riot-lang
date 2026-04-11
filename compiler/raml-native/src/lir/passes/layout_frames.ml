(** This pass computes explicit frame metadata for each [LIR] procedure. It
    asks [Frame_analysis] whether the procedure needs a frame and whether it
    performs calls, assigns stable stack-slot offsets for the pseudo-registers
    that need homes, and finishes by computing an aligned frame size. That
    leaves emitters with concrete frame metadata to consume instead of forcing
    them to rediscover stack layout from the instruction stream. *)
open Std
module Lir = Types

let pointer_width = 8

let align_to = fun value ~alignment ->
  if value mod alignment = 0 then
    value
  else
    value + (alignment - (value mod alignment))

let layout_of_procedure = fun (procedure: Lir.Procedure.t) ->
  let analysis = Frame_analysis.analyze_procedure procedure in
  let slots =
    List.mapi (fun index name -> Lir.Slot.{ name; offset = index * pointer_width }) analysis.slot_names
  in
  let frame_size = align_to (List.length slots * pointer_width) ~alignment:16 in
  Lir.Frame.{
    contains_calls = analysis.contains_calls;
    frame_required = analysis.frame_required;
    slots;
    frame_size
  }

let program = fun (program: Lir.Program.t) ->
  {
    program
    with procedures = List.map
      (fun (procedure: Lir.Procedure.t) -> { procedure with frame = layout_of_procedure procedure })
      program.procedures
  }
