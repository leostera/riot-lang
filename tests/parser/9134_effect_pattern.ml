let handle_match thunk =
  match thunk () with
  | value -> value
  | effect (Yield yielded) k -> continue k yielded

let handle_try thunk =
  try thunk () with
  | effect (Choose (left, right)) k -> continue k left
