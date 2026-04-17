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

let write_stdout_bytes = fun bytes ~len ->
  let rec loop pos remaining =
    if remaining > 0 then
      match Kernel.IO.Stdout.write ~pos ~len:remaining bytes with
      | Result.Ok written ->
          if written <= 0 then
            panic "stdout write returned 0 bytes"
          else
            loop (pos + written) (remaining - written)
      | Result.Error error -> panic (Kernel.IO.Stdout.error_to_string error)
  in
  loop 0 len

let write_stdout_pair = fun left ~left_len right ~right_len ->
  let rec loop left_pos left_remaining right_pos right_remaining =
    let remaining = left_remaining + right_remaining in
    if remaining > 0 then
      match
        Kernel.IO.Stdout.write_pair
          ~left_pos
          ~left_len:left_remaining
          left
          ~right_pos
          ~right_len:right_remaining
          right
      with
      | Result.Ok written ->
          if written <= 0 then
            panic "stdout write_pair returned 0 bytes"
          else
            let left_written =
              if written < left_remaining then
                written
              else
                left_remaining
            in
            let right_written = written - left_written in
            loop
              (left_pos + left_written)
              (left_remaining - left_written)
              (right_pos + right_written)
              (right_remaining - right_written)
      | Result.Error error -> panic (Kernel.IO.Stdout.error_to_string error)
  in
  loop 0 left_len 0 right_len

let write_stderr_bytes = fun bytes ~len ->
  let rec loop pos remaining =
    if remaining > 0 then
      match Kernel.IO.Stderr.write ~pos ~len:remaining bytes with
      | Result.Ok written ->
          if written <= 0 then
            panic "stderr write returned 0 bytes"
          else
            loop (pos + written) (remaining - written)
      | Result.Error error -> panic (Kernel.IO.Stderr.error_to_string error)
  in
  loop 0 len

let write_stderr_pair = fun left ~left_len right ~right_len ->
  let rec loop left_pos left_remaining right_pos right_remaining =
    let remaining = left_remaining + right_remaining in
    if remaining > 0 then
      match
        Kernel.IO.Stderr.write_pair
          ~left_pos
          ~left_len:left_remaining
          left
          ~right_pos
          ~right_len:right_remaining
          right
      with
      | Result.Ok written ->
          if written <= 0 then
            panic "stderr write_pair returned 0 bytes"
          else
            let left_written =
              if written < left_remaining then
                written
              else
                left_remaining
            in
            let right_written = written - left_written in
            loop
              (left_pos + left_written)
              (left_remaining - left_written)
              (right_pos + right_written)
              (right_remaining - right_written)
      | Result.Error error -> panic (Kernel.IO.Stderr.error_to_string error)
  in
  loop 0 left_len 0 right_len

let print = fun message ->
  let bytes = bytes_unsafe_of_string message in
  write_stdout_bytes bytes ~len:(String.length message)

let eprint = fun message ->
  let bytes = bytes_unsafe_of_string message in
  write_stderr_bytes bytes ~len:(String.length message)

let println = fun message ->
  let bytes = bytes_unsafe_of_string message in
  write_stdout_pair bytes ~left_len:(String.length message) newline ~right_len:1

let eprintln = fun message ->
  let bytes = bytes_unsafe_of_string message in
  write_stderr_pair bytes ~left_len:(String.length message) newline ~right_len:1

let todo = fun msg -> panic ("TODO: " ^ msg)

let unimplemented = fun () -> panic "unimplemented"
