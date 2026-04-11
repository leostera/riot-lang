(** This pass assigns concrete homes to [LIR] virtual values after frame
    analysis has already run.

    The algorithm computes live intervals, uses a small caller-saved register
    pool for short-lived values, uses a small callee-saved pool for values
    that remain live across calls, and spills the rest to stack slots with
    stable offsets.

    The effect is that [LIR] leaves this pass with explicit register or stack
    homes, plus a frame whose spill slots and callee-saved save set match the
    values that actually needed them.

    The rationale is to make home allocation a real compiler pass instead of an
    emitter convention, while keeping the first allocator simple and honest:
    caller-saved registers for cheap temporaries, callee-saved registers for
    call-live values, and stack for overflow. *)
open Std
module HashMap = Collections.HashMap
module HashSet = Collections.HashSet
module Lir = Types

type analysis = Layout_frames.analysis

type allocated_home =
  | Register of string
  | Stack

type active_interval = {
  finish: int;
  register: string;
}

let pointer_width = 8

let caller_saved_registers = [ "x11"; "x12"; "x13"; "x14"; "x15"; "x17" ]

let callee_saved_registers = [
  "x19";
  "x20";
  "x21";
  "x22";
  "x23";
  "x24";
  "x25";
  "x26";
  "x27";
  "x28"
]

let callee_saved_register_set =
  let set = HashSet.create () in
  List.iter
    (fun register ->
      let _ = HashSet.insert set register in
      ())
    callee_saved_registers;
  set

let align_to = fun value ~alignment ->
  if value mod alignment = 0 then
    value
  else
    value + (alignment - (value mod alignment))

let names_for_procedure = fun analysis procedure_name ->
  Layout_frames.virtual_names_for_procedure analysis ~procedure_name

let expire_finished = fun active ~before ->
  active |> List.filter (fun interval -> interval.finish >= before)

let available_registers = fun register_pool active ->
  register_pool |> List.filter
    (fun register ->
      not
        (
          List.exists
            (fun active_interval ->
              String.equal active_interval.register register)
            active
        ))

let allocation_for_procedure = fun analysis (procedure: Lir.Procedure.t) ->
  let virtual_names = names_for_procedure analysis procedure.name in
  let intervals_by_name =
    Liveness.intervals_of_procedure procedure
    |> List.fold_left
      (fun intervals (interval: Liveness.interval) ->
        let _ = HashMap.insert intervals interval.name interval in
        intervals)
      (HashMap.create ())
  in
  let sorted_intervals =
    virtual_names
    |> List.filter_map (HashMap.get intervals_by_name)
    |> List.sort
      (fun (left: Liveness.interval) (right: Liveness.interval) ->
        match Int.compare left.start right.start with
        | 0 -> String.compare left.name right.name
        | order -> order)
  in
  let assignments = HashMap.create () in
  let _, assignments =
    List.fold_left
      (fun (active, assignments) (interval: Liveness.interval) ->
        let active = expire_finished active ~before:interval.start in
        let register_pool =
          if interval.live_across_call then
            callee_saved_registers
          else
            caller_saved_registers
        in
        match available_registers register_pool active with
        | register :: _ ->
            let _ = HashMap.insert assignments interval.name (Register register) in
            ({ finish = interval.finish; register } :: active, assignments)
        | [] ->
            let _ = HashMap.insert assignments interval.name Stack in
            (active, assignments))
      ([], assignments)
      sorted_intervals
  in
  assignments

let homes_for_procedure = fun analysis (procedure: Lir.Procedure.t) ->
  let virtual_names = names_for_procedure analysis procedure.name in
  let assignments = allocation_for_procedure analysis procedure in
  let used_callee_saved = HashSet.create () in
  let next_slot_index = ref 0 in
  let stack_slots_rev = ref [] in
  let homes =
    virtual_names
    |> List.map
      (fun name ->
        let home =
          match HashMap.get assignments name with
          | Some (Register register) ->
              let () =
                if HashSet.contains callee_saved_register_set register then
                  let _ = HashSet.insert used_callee_saved register in
                  ()
              in
              Lir.Home.Register register
          | Some Stack
          | None ->
              let index = !next_slot_index in
              let slot = Lir.Slot.{ index; offset = index * pointer_width } in
              next_slot_index := index + 1;
              stack_slots_rev := slot :: !stack_slots_rev;
              Lir.Home.Stack_slot slot
        in
        Lir.Home_binding.{ name; home })
  in
  let saved_registers = List.filter (HashSet.contains used_callee_saved) callee_saved_registers in
  (homes, List.rev !stack_slots_rev, saved_registers)

let rewrite_procedure = fun analysis (procedure: Lir.Procedure.t) ->
  let homes, slots, saved_registers = homes_for_procedure analysis procedure in
  let frame_bytes = (List.length slots + List.length saved_registers) * pointer_width in
  let frame_size = align_to frame_bytes ~alignment:16 in
  let frame_required = procedure.frame.contains_calls || slots <> [] || saved_registers <> [] in
  {
    procedure
    with frame =
      {
        procedure.frame
        with homes;
        slots;
        saved_registers;
        frame_size;
        frame_required;
      };
  }

let program: analysis:analysis -> Lir.Program.t -> Lir.Program.t = fun ~analysis (
  program: Lir.Program.t
) ->
  { program with procedures = List.map (rewrite_procedure analysis) program.procedures }
