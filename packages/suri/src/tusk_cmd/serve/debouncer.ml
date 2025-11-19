open Std

type 'a t = {
  wait : Time.Duration.t;
  callback : 'a list -> unit;
  mutable pending : 'a list;
  mutable timer : Time.Instant.t;
}

let create ~wait callback = {
  wait;
  callback;
  pending = [];
  timer = Time.Instant.now ();
}

let push t event =
  (* Add to pending *)
  t.pending <- event :: t.pending;
  (* Update timer *)
  t.timer <- Time.Instant.now ()

let flush t =
  let events = t.pending in
  if List.length events > 0 then begin
    t.pending <- [];
    t.callback (List.rev events)
  end

let should_flush t =
  let last_event = t.timer in
  let elapsed = Time.Instant.elapsed last_event in
  elapsed >= t.wait && List.length t.pending > 0

let tick t =
  if should_flush t then flush t
