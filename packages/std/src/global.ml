module Format = Format

type format = Format.t

type ('value, 'error) result = ('value, 'error) Kernel.result =
  | Ok of 'value
  | Error of 'error

let format = Format.format

let ( = ) = Kernel.( = )

let compare = Kernel.compare

let min = Kernel.min

let max = Kernel.max

let ( != ) = Kernel.( != )

let ( < ) = Kernel.( < )

let ( > ) = Kernel.( > )

let ( <= ) = Kernel.( <= )

let ( >= ) = Kernel.( >= )

let ( ~- ) = Kernel.( ~- )

let ( + ) = Kernel.( + )

let ( - ) = Kernel.( - )

let ( * ) = Kernel.( * )

let ( / ) = Kernel.( / )

let ( mod ) = Kernel.( mod )

let ( land ) = Kernel.( land )

let ( lor ) = Kernel.( lor )

let ( lxor ) = Kernel.( lxor )

let lnot = Kernel.lnot

let ( lsl ) = Kernel.( lsl )

let ( lsr ) = Kernel.( lsr )

let ( asr ) = Kernel.( asr )

let ( ~-. ) = Kernel.( ~-. )

let ( +. ) = Kernel.( +. )

let ( -. ) = Kernel.( -. )

let ( *. ) = Kernel.( *. )

let ( /. ) = Kernel.( /. )

let ( @@ ) = Kernel.( @@ )

let ( |> ) = Kernel.( |> )

let ( ^ ) = Kernel.( ^ )

let ( @ ) = Kernel.( @ )

let ( ** ) = Kernel.( ** )

let float_of_int = Float.from_int

let int_of_float = Float.to_int

let float = Float.from_int

let string_of_int = Int.to_string

let string_of_float = Float.to_string

let abs = Int.abs

let mod_float = Float.rem

let sqrt = Float.sqrt

let floor = Float.floor

let ceil = Float.ceil

let not = Kernel.not

let ( && ) = Kernel.( && )

let ( || ) = Kernel.( || )

let raise = Kernel.raise

let raise_notrace = Kernel.Exception.raise_notrace

let ignore = fun _ -> ()

(** Process management globals *)
include Runtime.Exception

type 'msg selector = 'msg Runtime.selector

let self = Runtime.self

let spawn = Runtime.spawn

let spawn_link = Runtime.spawn_link

let send = Runtime.send

let receive = fun ~selector ?timeout () ->
  let timeout = Option.map timeout ~fn:Time.Duration.to_secs_float in
  Runtime.receive ~selector ?timeout ()

let receive_any = fun ?timeout () ->
  let timeout = Option.map timeout ~fn:Time.Duration.to_secs_float in
  Runtime.receive_any ?timeout ()

let sleep = fun timeout ->
  let selector _msg = `skip in
  try receive ~selector ~timeout () with
  | Receive_timeout -> ()

let yield = Runtime.yield

let shutdown = Runtime.shutdown

let panic = Kernel.SystemError.panic

let ( ! ) = fun cell -> Sync.Cell.get cell

let ( := ) = fun cell value ->
  Sync.Cell.set cell value

let ref = fun value -> Sync.Cell.create value

let cell = fun value -> Sync.Cell.create value

external bytes_unsafe_of_string: string -> bytes = "%bytes_of_string"

let newline = bytes_unsafe_of_string "\n"

external fd_write_all_raw_int:
  int -> bytes -> int -> int -> int
  = "kernel_new_fs_file_write_all_raw"

external fd_write_pair_all_raw_int:
  int -> bytes -> int -> int -> bytes -> int -> int -> int
  = "kernel_new_fs_file_write_pair_all_raw_bytecode" "kernel_new_fs_file_write_pair_all_raw"

let panic_write_error = fun code -> panic (Kernel.SystemError.to_string (Kernel.SystemError.from_code code))

let write_stdout_bytes = fun bytes ~len ->
  let written = fd_write_all_raw_int 1 bytes 0 len in
  if written = len then
    ()
  else if written = 0 then
    panic "stdout write returned 0 bytes"
  else
    panic_write_error (-written)

let write_stdout_pair = fun left ~left_len right ~right_len ->
  let total_len = left_len + right_len in
  let written = fd_write_pair_all_raw_int 1 left 0 left_len right 0 right_len in
  if written = total_len then
    ()
  else if written = 0 then
    panic "stdout write_pair returned 0 bytes"
  else
    panic_write_error (-written)

let write_stderr_bytes = fun bytes ~len ->
  let written = fd_write_all_raw_int 2 bytes 0 len in
  if written = len then
    ()
  else if written = 0 then
    panic "stderr write returned 0 bytes"
  else
    panic_write_error (-written)

let write_stderr_pair = fun left ~left_len right ~right_len ->
  let total_len = left_len + right_len in
  let written = fd_write_pair_all_raw_int 2 left 0 left_len right 0 right_len in
  if written = total_len then
    ()
  else if written = 0 then
    panic "stderr write_pair returned 0 bytes"
  else
    panic_write_error (-written)

let print = fun message ->
  let bytes = bytes_unsafe_of_string message in
  let len = String.length message in
  write_stdout_bytes bytes ~len

let eprint = fun message ->
  let bytes = bytes_unsafe_of_string message in
  let len = String.length message in
  write_stderr_bytes bytes ~len

let println = fun message ->
  let bytes = bytes_unsafe_of_string message in
  let len = String.length message in
  (* Human-mode renderers call this per line, so keep it on the raw native path and
     avoid the richer iovec-building IO surface here. *)
  write_stdout_pair bytes ~left_len:len newline ~right_len:1

let eprintln = fun message ->
  let bytes = bytes_unsafe_of_string message in
  let len = String.length message in
  write_stderr_pair bytes ~left_len:len newline ~right_len:1

let todo = fun msg -> panic ("TODO: " ^ msg)

let unimplemented = fun () -> panic "unimplemented"
