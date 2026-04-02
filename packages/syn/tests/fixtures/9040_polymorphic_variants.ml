(* Polymorphic variant type definitions *)

type color =
[
  `Red
  | `Green
  | `Blue
]

type extended_color = [
  color
  | `Yellow
  | `Orange
]

(* Open and closed variants *)

type io_error =
[
  `Eof
  | `Timeout
  | `Connection_closed
]

type ('ok, 'err) io_result =
  ('ok, ([>
    io_error
  ] as 'err)) result

(* Polymorphic variant patterns *)

let test = function
  | `Red -> "red"
  | `Green -> "green"
  | `Blue -> "blue"

(* Constructor with polymorphic variant argument *)

let handle_result = function
  | Error `Would_block -> "blocked"
  | Error `Eof -> "eof"
  | Ok x -> "ok"

(* Nested polymorphic variants *)

let complex = function
  | Some `Foo -> 1
  | Some `Bar -> 2
  | None -> 0
