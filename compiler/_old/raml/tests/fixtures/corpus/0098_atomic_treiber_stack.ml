(* Lock-free Treiber stack using atomics. *)
type 'a stack = 'a list Atomic.t

let rec push stack elt =
  let cur = Atomic.get stack in
  if not (Atomic.compare_and_set stack cur (elt :: cur)) then
    push stack elt

let rec pop stack =
  let cur = Atomic.get stack in
  match cur with
  | [] -> None
  | x :: tl ->
      if Atomic.compare_and_set stack cur tl then Some x
      else pop stack

let () =
  let st = Atomic.make [] in
  push st 1;
  push st 2;
  push st 3;
  let a = pop st in
  let b = pop st in
  let c = pop st in
  let show = function None -> "none" | Some x -> string_of_int x in
  Printf.printf "%s %s %s\n" (show a) (show b) (show c)
