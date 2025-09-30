type t = int

let of_mode mode = mode
let to_mode t = t

(* Check if any write bit is set *)
let has_write_bit mode = mode land 0o222 <> 0
let readonly t = not (has_write_bit t)

let set_readonly t readonly =
  if readonly then
    (* Clear all write bits *)
    t land lnot 0o222
  else
    (* Set all write bits (world-writable!) *)
    t lor 0o222

(* User permissions *)
let user_read t = t land 0o400 <> 0
let user_write t = t land 0o200 <> 0
let user_execute t = t land 0o100 <> 0

(* Group permissions *)
let group_read t = t land 0o040 <> 0
let group_write t = t land 0o020 <> 0
let group_execute t = t land 0o010 <> 0

(* Other permissions *)
let other_read t = t land 0o004 <> 0
let other_write t = t land 0o002 <> 0
let other_execute t = t land 0o001 <> 0

(* Common modes *)
let read_write = 0o644
let executable = 0o755
let private_read_write = 0o600
let private_executable = 0o700
