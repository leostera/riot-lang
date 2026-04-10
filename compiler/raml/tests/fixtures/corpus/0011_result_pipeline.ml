(* Result-based parsing and validation. *)
let parse_int s =
  try Ok (int_of_string s) with
  | Failure _ -> Error ("bad int:" ^ s)

let nonnegative n =
  if n >= 0 then Ok n else Error "negative"

let outcome =
  match parse_int "42" with
  | Error _ as e -> e
  | Ok x -> nonnegative (x - 10)

let () =
  match outcome with
  | Ok n -> Printf.printf "ok:%d\n" n
  | Error msg -> Printf.printf "error:%s\n" msg
