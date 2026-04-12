open Prelude

let ( let* ) value fn = Result.and_then value ~fn

type error =
  | InvalidVarName of { name: string }
  | System of System_error.t

let args = Caml_runtime.argv

let executable_name =
  if Array.length args = 0 then
    None
  else
    Array.get args ~at:0

module FFI = struct
  external get: string -> string option = "kernel_new_env_get"

  external set_var: string -> string -> (unit, int) Result.t = "kernel_new_env_set_var"

  external remove_var: string -> (unit, int) Result.t = "kernel_new_env_remove_var"

  external vars: unit -> (string * string) array = "kernel_new_env_vars"

  external current_dir: unit -> (string, int) Result.t = "kernel_new_env_current_dir"

  external set_current_dir: string -> (unit, int) Result.t = "kernel_new_env_set_current_dir"
end

let get = fun ~var -> FFI.get var

let error_to_string = fun value ->
  match value with
  | InvalidVarName { name } -> String.concat "" [ "invalid environment variable name: "; name ]
  | System error -> System_error.to_string error

let validate_var_name = fun name ->
  let length = String.length name in
  let rec loop index =
    if index >= length then
      Result.Ok ()
    else if String.get_unchecked name ~at:index = '=' then
      Result.Error (InvalidVarName { name })
    else
      loop (index + 1)
  in
  if length = 0 then
    Result.Error (InvalidVarName { name })
  else
    loop 0

let set = fun ~var ~value ->
  let name = var in
  let* () = validate_var_name name in
  FFI.set_var name value |> Result.map_err ~fn:(fun code -> System (System_error.from_code code))

let remove = fun ~var ->
  let name = var in
  let* () = validate_var_name name in
  FFI.remove_var name |> Result.map_err ~fn:(fun code -> System (System_error.from_code code))

let vars = FFI.vars

let current_dir = fun () ->
  FFI.current_dir ()
  |> Result.map ~fn:Path.from_string
  |> Result.map_err ~fn:(fun code -> System (System_error.from_code code))

let set_current_dir = fun path ->
  FFI.set_current_dir (Path.to_string path)
  |> Result.map_err ~fn:(fun code -> System (System_error.from_code code))

let home_dir = fun () -> get ~var:"HOME" |> Option.map ~fn:Path.from_string
