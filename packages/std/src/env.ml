(** Environment utilities *)
open Global
open Collections

let args = Array.to_list (Kernel.System.argv ())

let current_dir = fun () ->
  match Kernel.Fs.File.getcwd () with
  | Ok cwd -> Path.of_string cwd
  | Error e -> Error (Path.SystemError (Kernel.IO.error_message e))

let set_current_dir = fun path ->
  match Kernel.Fs.File.chdir (Path.to_string path) with
  | Ok () -> Ok ()
  | Error e -> Error (Path.SystemError (Kernel.IO.error_message e))

let home_dir = fun () ->
  try
    let home = Kernel.Env.getenv_exn "HOME" |> Path.of_string |> Result.expect ~msg:"HOME path was an invalid UTF-8 path" in
    Some home
  with
  | Not_found -> None

type 't var_type =
  | String: string var_type
  | Int: int var_type
  | Float: float var_type
  | Bool: bool var_type
  | Char: char var_type

let var : type t. t var_type -> name:string -> t option = fun var_type ~name ->
  try
    let value = Kernel.Env.getenv_exn name in
    match var_type with
    | String ->
        Some value
    | Int -> (
        try Some (int_of_string value) with
        | _ -> None
      )
    | Float -> (
        try Some (float_of_string value) with
        | _ -> None
      )
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
  with
  | Not_found -> None

let set_var = fun ~name ~value ->
  Kernel.Env.putenv name value;
  None

let vars = fun () ->
  Kernel.Env.environment () |> Array.to_list |> List.filter_map
    (fun s ->
      match String.index_opt s '=' with
      | None -> None
      | Some idx ->
          let name = String.sub s 0 idx in
          let value = String.sub s (idx + 1) (String.length s - idx - 1) in
          Some (name, value))
