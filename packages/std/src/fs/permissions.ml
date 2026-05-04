open Global

type t = int

let from_mode = fun mode -> mode

let to_mode = fun t -> t

(* Check if any write bit is set *)

let has_write_bit = fun mode -> mode land 0o222 != 0

let readonly = fun t -> not (has_write_bit t)

let set_readonly = fun t readonly ->
  if readonly then
    t land lnot 0o222
  else
    (* Set all write bits (world-writable!) *)
    t lor 0o222

(* User permissions *)

let user_read = fun t -> t land 0o400 != 0

let user_write = fun t -> t land 0o200 != 0

let user_execute = fun t -> t land 0o100 != 0

(* Group permissions *)

let group_read = fun t -> t land 0o040 != 0

let group_write = fun t -> t land 0o020 != 0

let group_execute = fun t -> t land 0o010 != 0

(* Other permissions *)

let other_read = fun t -> t land 0o004 != 0

let other_write = fun t -> t land 0o002 != 0

let other_execute = fun t -> t land 0o001 != 0

(* Common modes *)

let read_write = 0o644

let executable = 0o755

let private_read_write = 0o600

let private_executable = 0o700
