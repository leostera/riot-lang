type t = ..
type envelope = { msg : t; uid : int }

let uid_counter = ref 0

let envelope msg =
  incr uid_counter;
  { msg; uid = !uid_counter }