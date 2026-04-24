(** Environment utilities *)
open Global
open Collections

let args = Kernel.Array.fold_left Kernel.Env.args ~acc:[] ~fn:(fun acc value -> value :: acc)
|> List.reverse

let current_dir = fun () ->
  match Kernel.Env.current_dir () with
  | Ok cwd -> Path.from_string cwd
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

let get_raw = fun name -> Kernel.Env.get ~var:name

let get: type t. t var_type -> var:string -> t option = fun var_type ~var ->
  match get_raw var with
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
            Some (String.get_unchecked value ~at:0)
          else
            None

let var = fun kind ~name -> get kind ~var:name

let set = fun ~var ~value ->
  let previous = get_raw var in
  let _ = Kernel.Env.set ~var ~value in
  previous

let remove = fun ~var ->
  let previous = get_raw var in
  let _ = Kernel.Env.remove ~var in
  previous

let vars = fun () ->
  Kernel.Env.vars () |> Kernel.Array.fold_left ~acc:[] ~fn:(fun acc value -> value :: acc) |> List.reverse
