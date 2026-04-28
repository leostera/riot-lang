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

type 'msg selection = 'msg Runtime.selection =
  | Select of 'msg
  | Skip

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
  let selector _msg = Skip in
  try receive ~selector ~timeout () with
  | Receive_timeout -> ()

let yield = Runtime.yield

let shutdown = Runtime.shutdown

let panic = Kernel.SystemError.panic

let ( ! ) = fun cell -> Sync.Cell.get cell

let ( := ) = fun cell value -> Sync.Cell.set cell value

let ref = fun value -> Sync.Cell.create value

let cell = fun value -> Sync.Cell.create value

let print = fun message ->
  match Kernel.IO.print message with
  | Ok () -> ()
  | Error error -> panic (Kernel.IO.Stdout.error_to_string error)

let eprint = fun message ->
  match Kernel.IO.eprint message with
  | Ok () -> ()
  | Error error -> panic (Kernel.IO.Stderr.error_to_string error)

let println = fun message ->
  match Kernel.IO.println message with
  | Ok () -> ()
  | Error error -> panic (Kernel.IO.Stdout.error_to_string error)

let eprintln = fun message ->
  match Kernel.IO.eprintln message with
  | Ok () -> ()
  | Error error -> panic (Kernel.IO.Stderr.error_to_string error)

let todo = fun msg -> panic ("TODO: " ^ msg)

let unimplemented = fun () -> panic "unimplemented"
