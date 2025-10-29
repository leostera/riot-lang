open Kernel

(* Simple timing wheel implementation
   
   Uses a single level with 256 slots at the configured resolution.
   Timers that don't fit in the wheel are kept in an overflow list.
   
   Key insight: We only check slots that have elapsed since last tick,
   not iterate through every tick that passed.
*)

type t = {
  config : Config.t;
  slots : Timer.t list array;
  overflow : Timer.t list ref;
  num_slots : int;
  slot_duration : int64;
  mutable current_time : int64;
  mutable current_slot : int;
  timers_by_id : (Timer_id.t, Timer.t) Hashtbl.t;
}

let create ~config =
  let now = Time.monotonic_time_nanos () in
  let slot_duration =
    Config.resolution_to_nanos config.Config.timer_resolution
  in
  let num_slots = 256 in

  {
    config;
    slots = Array.make num_slots [];
    overflow = ref [];
    num_slots;
    slot_duration;
    current_time = now;
    current_slot = 0;
    timers_by_id = Hashtbl.create 128;
  }

let calculate_slot t expires_at =
  let delta = Int64.sub expires_at t.current_time in
  let ticks = Int64.div delta t.slot_duration in

  if Int64.compare ticks (Int64.of_int t.num_slots) < 0 then
    (* Timer fits in the wheel *)
    let slot =
      Int64.to_int
        (Int64.rem
           (Int64.add (Int64.of_int t.current_slot) ticks)
           (Int64.of_int t.num_slots))
    in
    Some slot
  else
    (* Timer is too far in the future - put in overflow *)
    None

let add_timer t ~now ~duration_nanos ~mode ~action =
  let timer = Timer.make ~now ~duration_nanos ~mode ~action in
  Hashtbl.add t.timers_by_id timer.id timer;

  (match calculate_slot t timer.Timer.expires_at with
  | Some slot -> t.slots.(slot) <- timer :: t.slots.(slot)
  | None -> t.overflow := timer :: !(t.overflow));

  timer.id

let cancel_timer t timer_id =
  match Hashtbl.find_opt t.timers_by_id timer_id with
  | Some timer ->
      Timer.cancel timer;
      Hashtbl.remove t.timers_by_id timer_id
  | None -> ()

let tick t ~now =
  if Int64.compare now t.current_time < 0 then
    (* Time went backwards? Should never happen with monotonic clock *)
    []
  else
    let expired = ref [] in

    (* Calculate which slot corresponds to 'now' *)
    let ticks_elapsed =
      Int64.div (Int64.sub now t.current_time) t.slot_duration
    in
    let ticks_to_process = min (Int64.to_int ticks_elapsed) t.num_slots in

    (* Process slots from current_slot to new slot *)
    for _i = 1 to ticks_to_process do
      t.current_slot <- (t.current_slot + 1) mod t.num_slots;

      let timers = t.slots.(t.current_slot) in
      t.slots.(t.current_slot) <- [];

      (* Check each timer in this slot *)
      List.iter
        (fun timer ->
          if Timer.is_cancelled timer then
            Hashtbl.remove t.timers_by_id timer.id
          else if Timer.should_fire timer ~now then (
            expired := timer :: !expired;
            Hashtbl.remove t.timers_by_id timer.id)
          else
            (* Timer not yet expired - re-insert *)
            match calculate_slot t timer.Timer.expires_at with
            | Some slot -> t.slots.(slot) <- timer :: t.slots.(slot)
            | None -> t.overflow := timer :: !(t.overflow))
        timers
    done;

    (* Check overflow list for timers that now fit in the wheel *)
    let still_overflow = ref [] in
    List.iter
      (fun timer ->
        if Timer.is_cancelled timer then Hashtbl.remove t.timers_by_id timer.id
        else if Timer.should_fire timer ~now then (
          expired := timer :: !expired;
          Hashtbl.remove t.timers_by_id timer.id)
        else
          match calculate_slot t timer.Timer.expires_at with
          | Some slot -> t.slots.(slot) <- timer :: t.slots.(slot)
          | None -> still_overflow := timer :: !still_overflow)
      !(t.overflow);
    t.overflow := !still_overflow;

    (* Update current time *)
    t.current_time <- now;

    !expired

let next_expiration t ~now =
  let _unused = now in
  let min_expiration = ref None in

  Hashtbl.iter
    (fun _id timer ->
      if not (Timer.is_cancelled timer) then
        match !min_expiration with
        | None -> min_expiration := Some timer.Timer.expires_at
        | Some current_min ->
            if Int64.compare timer.Timer.expires_at current_min < 0 then
              min_expiration := Some timer.Timer.expires_at)
    t.timers_by_id;

  !min_expiration

let size t = Hashtbl.length t.timers_by_id
