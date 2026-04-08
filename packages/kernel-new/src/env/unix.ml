open Prelude

type error = Error.t

external args: string array = "%sys_argv"

module FFI = struct
  external get:
    string -> string option
    = "kernel_new_env_get"

  external set_var:
    string -> string -> (unit, int) Result.t
    = "kernel_new_env_set_var"

  external remove_var:
    string -> (unit, int) Result.t
    = "kernel_new_env_remove_var"

  external vars:
    unit -> (string * string) array
    = "kernel_new_env_vars"

  external current_dir:
    unit -> (string, int) Result.t
    = "kernel_new_env_current_dir"

  external set_current_dir:
    string -> (unit, int) Result.t
    = "kernel_new_env_set_current_dir"
end

let get = FFI.get

let set_var = fun ~name ~value ->
  Result.map_error Error.of_code (FFI.set_var name value)

let remove_var = fun ~name ->
  Result.map_error Error.of_code (FFI.remove_var name)

let vars = FFI.vars

let current_dir = fun () ->
  Result.map_error Error.of_code (Result.map Path.v (FFI.current_dir ()))

let set_current_dir = fun path ->
  Result.map_error Error.of_code (FFI.set_current_dir (Path.to_string path))

let home_dir = fun () ->
  Option.map Path.v (get "HOME")
