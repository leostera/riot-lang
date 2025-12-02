open Std

type t = string

(** Package names must be lowercase with hyphens (kebab-case)
    Valid: "hello-world", "tusk-server", "std"
    Invalid: "hello_world", "helloWorld", "HelloWorld"
*)
let from_string name =
  if name = "" then Error "Package name cannot be empty"
  else if String.contains name "_" then 
    Error "Package name cannot contain underscores, use hyphens instead"
  else if String.contains name " " then
    Error "Package name cannot contain spaces"
  else
    (* Check all characters are lowercase, digits, or hyphens *)
    let is_valid_char c = 
      (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') || c = '-'
    in
    let rec check i =
      if i >= String.length name then Ok name
      else if is_valid_char name.[i] then check (i + 1)
      else Error (String.concat "" ["Invalid character '"; String.make 1 name.[i]; "' in package name"])
    in
    check 0

let to_string t = t

(** Create from string, panicking if invalid (for internal use when string is known valid) *)
let of_string_exn name =
  match from_string name with
  | Ok t -> t
  | Error msg -> panic msg

let to_json t = Data.Json.String t

let from_json json =
  match json with
  | Data.Json.String s -> from_string s
  | _ -> Error "Expected string for package name"
