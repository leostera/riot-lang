open Prelude

let ( let* ) = Result.and_then

type error =
  | InvalidVarName of { name: string }
  | System of System_error.t

let args = Caml_runtime.argv

module FFI = struct
  external get: string -> string option = "kernel_new_env_get"

  external set_var: string -> string -> (unit, int) Result.t = "kernel_new_env_set_var"

  external remove_var: string -> (unit, int) Result.t = "kernel_new_env_remove_var"

  external vars: unit -> (string * string) array = "kernel_new_env_vars"

  external current_dir: unit -> (string, int) Result.t = "kernel_new_env_current_dir"

  external set_current_dir: string -> (unit, int) Result.t = "kernel_new_env_set_current_dir"
end

let get = FFI.get

let error_to_string = fun value ->
  match value with
  | InvalidVarName { name } -> String.concat "" [ "invalid environment variable name: "; name ]
  | System error -> System_error.to_string error

let validate_var_name = fun name ->
  let length = String.length name in
  let rec loop index =
    if index >= length then
      Result.Ok ()
    else if String.get name index = '=' then
      Result.Error (InvalidVarName { name })
    else
      loop (index + 1)
  in
  if length = 0 then
    Result.Error (InvalidVarName { name })
  else
    loop 0

let set_var = fun ~name ~value ->
  let* () = validate_var_name name in
  Result.map_error (fun code -> System (System_error.of_code code)) (FFI.set_var name value)

let remove_var = fun ~name ->
  let* () = validate_var_name name in
  Result.map_error (fun code -> System (System_error.of_code code)) (FFI.remove_var name)

let vars = FFI.vars

let current_dir = fun () ->
  Result.map_error
    (fun code -> System (System_error.of_code code))
    (Result.map Path.of_string (FFI.current_dir ()))

let set_current_dir = fun path ->
  Result.map_error
    (fun code -> System (System_error.of_code code))
    (FFI.set_current_dir (Path.to_string path))

let home_dir = fun () ->
  Option.map Path.of_string (get "HOME")
