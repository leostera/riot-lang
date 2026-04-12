(** Environment utilities *)
open Global
open Collections

let args = Array.to_list Kernel.Env.args

let current_dir = fun () ->
  match Kernel.Env.current_dir () with
  | Ok cwd -> Path.of_string cwd
  | Error error -> Error (Path.SystemError (Kernel.Env.error_to_string error))

let set_current_dir = fun path ->
  match Kernel.Env.set_current_dir (Path.to_string path) with
  | Ok () -> Ok ()
  | Error error -> Error (Path.SystemError (Kernel.Env.error_to_string error))

let home_dir = fun () ->
  match Kernel.Env.home_dir () with
  | Some home -> Some (Path.v home)
  | None -> None

type 't var_type =
  | String: string var_type
  | Int: int var_type
  | Float: float var_type
  | Bool: bool var_type
  | Char: char var_type

let get = fun name -> Kernel.Env.get name

let var: type t. t var_type -> name:string -> t option = fun var_type ~name ->
  match get name with
  | None -> None
  | Some value ->
      match var_type with
      | String ->
          Some value
      | Int ->
          Int.parse value
      | Float ->
          Float.parse value
      | Bool -> (
          match String.lowercase_ascii value with
          | "true"
          | "1"
          | "yes"
          | "on" -> Some true
          | "false"
          | "0"
          | "no"
          | "off" -> Some false
          | _ -> None
        )
      | Char ->
          if String.length value = 1 then
            Some value.[0]
          else
            None

let set_var = fun ~name ~value ->
  let previous = get name in
  let _ = Kernel.Env.set_var ~name ~value in
  previous

let vars = fun () -> Kernel.Env.vars () |> Array.to_list
