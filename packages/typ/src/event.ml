open Std

type t = |

let unreachable = fun (_event: t) ->
  failwith "typ events are not defined yet"

let to_json = fun event -> unreachable event

let to_stream = fun event -> unreachable event
