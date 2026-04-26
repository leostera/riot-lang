open Kernel
open Collections

module Cell = struct
  type 'a t = { mutable value: 'a }

  let create = fun value -> { value }

  let get = fun cell -> cell.value

  let set = fun cell value -> cell.value <- value

  let ( ! ) = get

  let ( := ) = set
end

open Cell

(* Simple timing wheel implementation

   Uses a single level with 256 slots at the configured resolution.
   Timers that don't fit in the wheel are kept in an overflow list.

   Key insight: We only check slots that have elapsed since last tick,
   not iterate through every tick that passed.
*)

type t = {
  config: Config.t;
  slots: Timer.t list array;
  overflow: Timer.t list Cell.t;
  num_slots: int;
  slot_duration: int64;
  mutable current_time: int64;
  mutable current_slot: int;
  timers_by_id: (Timer_id.t, Timer.t) HashMap.t;
}

let get_slot = fun wheel ~at -> Array.get_unchecked wheel.slots ~at

let set_slot = fun wheel ~at ~value -> Array.set_unchecked wheel.slots ~at ~value

let monotonic_time_nanos = fun () ->
  match Kernel.Time.Monotonic.now () with
  | Ok time ->
      let (secs, nanos) = Kernel.Time.Monotonic.to_parts time in
      Int64.add (Int64.mul (Int64.from_int secs) 1_000_000_000L) (Int64.from_int nanos)
  | Error err -> Kernel.SystemError.panic (Kernel.Time.Monotonic.error_to_string err)

let create = fun ~config ->
  let now = monotonic_time_nanos () in
  let slot_duration = Config.resolution_to_nanos config.Config.timer_resolution in
  let num_slots = 256 in
  {
    config;
    slots = Array.make ~count:num_slots ~value:[];
    overflow = Cell.create [];
    num_slots;
    slot_duration;
    current_time = now;
    current_slot = 0;
    timers_by_id = HashMap.create ();
  }

let calculate_slot = fun t expires_at ->
  let delta = Int64.sub expires_at t.current_time in
  let ticks = Int64.div delta t.slot_duration in
  if (
    match Int64.compare ticks (Int64.from_int t.num_slots) with
    | Order.LT -> true
    | Order.EQ
    | Order.GT -> false
  ) then
    let slot =
      Int64.to_int
        (Int64.rem (Int64.add (Int64.from_int t.current_slot) ticks) (Int64.from_int t.num_slots))
    in
    Some slot
  else
    (* Timer is too far in the future - put in overflow *)
    None

let schedule_timer = fun t timer ->
  let _ = HashMap.insert t.timers_by_id ~key:timer.Timer.id ~value:timer in
  match calculate_slot t timer.Timer.expires_at with
  | Some slot -> set_slot t ~at:slot ~value:(timer :: get_slot t ~at:slot)
  | None -> t.overflow := timer :: !(t.overflow)

let add_timer = fun t ~now ~duration_nanos ~mode ~action ->
  let timer = Timer.make ~now ~duration_nanos ~mode ~action in
  schedule_timer t timer;
  timer.id

let reschedule_timer = fun t ~now timer ->
  Timer.reschedule timer ~now;
  schedule_timer t timer

let cancel_timer = fun t timer_id ->
  match HashMap.get t.timers_by_id ~key:timer_id with
  | Some timer ->
      Timer.cancel timer;
      let _ = HashMap.remove t.timers_by_id ~key:timer_id in
      ()
  | None -> ()

let tick = fun t ~now ->
  if (
    match Int64.compare now t.current_time with
    | Order.LT -> true
    | Order.EQ
    | Order.GT -> false
  ) then
    []
  else
    let expired = Cell.create [] in
    (* Calculate which slot corresponds to 'now' *)
    let ticks_elapsed = Int64.div (Int64.sub now t.current_time) t.slot_duration in
    let ticks_to_process = Int.min (Int64.to_int ticks_elapsed) t.num_slots in
    (* Process slots from current_slot to new slot *)
    for _i = 1 to ticks_to_process do
      t.current_slot <- (t.current_slot + 1) mod t.num_slots;
      let timers = get_slot t ~at:t.current_slot in
      set_slot t ~at:t.current_slot ~value:[];
      (* Check each timer in this slot *)
      List.for_each
        timers
        ~fn:(fun timer ->
          if Timer.is_cancelled timer then
            let _ = HashMap.remove t.timers_by_id ~key:timer.Timer.id in
            ()
          else if Timer.should_fire timer ~now then (
            expired := timer :: !expired;
            let _ = HashMap.remove t.timers_by_id ~key:timer.Timer.id in
            ()
          ) else
            (* Timer not yet expired - re-insert *)
            match calculate_slot t timer.Timer.expires_at with
            | Some slot -> set_slot t ~at:slot ~value:(timer :: get_slot t ~at:slot)
            | None -> t.overflow := timer :: !(t.overflow))
    done;
  (* Check overflow list for timers that now fit in the wheel *)
  let still_overflow = Cell.create [] in
  List.for_each
    !(t.overflow)
    ~fn:(fun timer ->
      if Timer.is_cancelled timer then
        let _ = HashMap.remove t.timers_by_id ~key:timer.Timer.id in
        ()
      else if Timer.should_fire timer ~now then (
        expired := timer :: !expired;
        let _ = HashMap.remove t.timers_by_id ~key:timer.Timer.id in
        ()
      ) else
        match calculate_slot t timer.Timer.expires_at with
        | Some slot -> set_slot t ~at:slot ~value:(timer :: get_slot t ~at:slot)
        | None -> still_overflow := timer :: !still_overflow);
  t.overflow := !still_overflow;
  (* Update current time *)
  t.current_time <- now;
  !expired

let next_expiration = fun t ~now ->
  let _unused = now in
  let min_expiration = Cell.create None in
  HashMap.for_each
    t.timers_by_id
    ~fn:(fun _id timer ->
      if not (Timer.is_cancelled timer) then
        match !min_expiration with
        | None -> min_expiration := Some timer.Timer.expires_at
        | Some current_min ->
            if (
              match Int64.compare timer.Timer.expires_at current_min with
              | Order.LT -> true
              | Order.EQ
              | Order.GT -> false
            ) then
              min_expiration := Some timer.Timer.expires_at);
  !min_expiration

let size = fun t -> HashMap.length t.timers_by_id
