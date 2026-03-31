open Global

type t =
  Http09
  | Http10
  | Http11
  | Http2
  | Http3

let of_string =
  function
  | "HTTP/0.9" -> Ok Http09
  | "HTTP/1.0" -> Ok Http10
  | "HTTP/1.1" -> Ok Http11
  | "HTTP/2"
  | "HTTP/2.0" -> Ok Http2
  | "HTTP/3"
  | "HTTP/3.0" -> Ok Http3
  | _ -> Error `InvalidVersion

let to_string =
  function
  | Http09 -> "HTTP/0.9"
  | Http10 -> "HTTP/1.0"
  | Http11 -> "HTTP/1.1"
  | Http2 -> "HTTP/2"
  | Http3 -> "HTTP/3"

let compare = fun v1 v2 ->
  let version_num =
    function
    | Http09 -> 0
    | Http10 -> 1
    | Http11 -> 2
    | Http2 -> 3
    | Http3 -> 4
  in
  Int.compare (version_num v1) (version_num v2)

let equal = fun v1 v2 -> compare v1 v2 = 0

let is_supported =
  function
  | Http09
  | Http10
  | Http11 -> true
  | Http2
  | Http3 -> false
