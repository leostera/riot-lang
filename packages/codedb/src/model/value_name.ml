open Std

type t = string

(** Value names must start with lowercase letter and contain only alphanumeric and underscores
    Valid: "hello_world", "helloWorld", "map2"
    Invalid: "HelloWorld", "_hello", "0hello"
*)
let from_string name =
  if name = "" then Error "Value name cannot be empty"
  else
    (* Must start with lowercase letter *)
    let first = name.[0] in
    if not (first >= 'a' && first <= 'z') then
      Error "Value name must start with a lowercase letter"
    else
      (* Check rest are alphanumeric or underscore *)
      let is_valid_char c =
        (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || 
        (c >= '0' && c <= '9') || c = '_'
      in
      let rec check i =
        if i >= String.length name then Ok name
        else if is_valid_char name.[i] then check (i + 1)
        else Error (String.concat "" ["Invalid character '"; String.make 1 name.[i]; "' in value name"])
      in
      check 1

let to_string t = t

let to_json t = Data.Json.String t

let from_json json =
  match json with
  | Data.Json.String s -> from_string s
  | _ -> Error "Expected string for value name"
