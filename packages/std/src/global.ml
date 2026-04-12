module Format = Format

type format = Format.t

type ('value, 'error) result = ('value, 'error) Kernel.result =
  | Ok of 'value
  | Error of 'error

let format = Format.format

let max_int = Kernel.max_int

let min_int = Kernel.min_int

let ( = ) = Kernel.( = )

let compare = Kernel.compare

let min = Kernel.min

let max = Kernel.max

let ( != ) = Kernel.( != )

let ( <> ) = Kernel.( <> )

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

let float_of_int = Kernel.float_of_int

let int_of_float = Kernel.int_of_float

let float = Kernel.float

let string_of_int = Kernel.string_of_int

let string_of_float = Kernel.string_of_float

let abs = Kernel.abs

let mod_float = Kernel.mod_float

let sqrt = Kernel.sqrt

let floor = Kernel.floor

let ceil = Kernel.ceil

let not = Kernel.not

let ( && ) = Kernel.( && )

let ( || ) = Kernel.( || )

let raise = Kernel.raise

let raise_notrace = Kernel.raise_notrace

let ignore = Kernel.ignore

(** Process management globals *)
include Runtime.Exception

type 'msg selector = 'msg Runtime.selector

let self = Runtime.self

let spawn = Runtime.spawn

let spawn_link = Runtime.spawn_link

let send = Runtime.send

let receive = fun ~selector ?timeout () ->
  let timeout = Option.map Time.Duration.to_secs_float timeout in
  Runtime.receive ~selector ?timeout ()

let receive_any = fun ?timeout () ->
  let timeout = Option.map Time.Duration.to_secs_float timeout in
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

let print = fun message ->
  Stdlib.print_string message;
  Stdlib.flush Stdlib.stdout

let println = fun message ->
  Stdlib.print_endline message;
  Stdlib.flush Stdlib.stdout

let eprint = fun message ->
  Stdlib.prerr_string message;
  Stdlib.flush Stdlib.stderr

let eprintln = fun message ->
  Stdlib.prerr_endline message;
  Stdlib.flush Stdlib.stderr

let todo = fun msg -> panic (format Format.[ str "TODO: "; str msg ])

let unimplemented = fun () -> panic "unimplemented"
