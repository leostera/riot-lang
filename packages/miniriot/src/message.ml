open Kernel

type t = ..
type envelope = { msg : t; uid : int }

let uid_counter = Sync.Cell.create 0

let envelope msg =
  let uid = Sync.Cell.get uid_counter + 1 in
  Sync.Cell.set uid_counter uid;
  { msg; uid }
