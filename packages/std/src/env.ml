(** Environment utilities *)

let args = Array.to_list (Kernel.System.argv ()) |> List.tl

let current_dir () =
  match Kernel.Fs.File.getcwd () with
  | Ok cwd -> Path.of_string cwd
  | Error e ->
      Error
        (Path.SystemError
           (Kernel.Async.pp_err Format.str_formatter e;
            Format.flush_str_formatter ()))

let set_current_dir path =
  match Kernel.Fs.File.chdir (Path.to_string path) with
  | Ok () -> Ok ()
  | Error e ->
      Error
        (Path.SystemError
           (Kernel.Async.pp_err Format.str_formatter e;
            Format.flush_str_formatter ()))

let home_dir () =
  try
    let home =
      Kernel.Env.getenv_exn "HOME"
      |> Path.of_string
      |> Result.expect ~msg:"HOME path was an invalid UTF-8 path"
    in
    Some home
  with Not_found -> None

type 't var_type =
  | String : string var_type
  | Int : int var_type
  | Float : float var_type
  | Bool : bool var_type
  | Char : char var_type

let var : type t. t var_type -> name:string -> t option =
 fun var_type ~name ->
  try
    let value = Kernel.Env.getenv_exn name in
    match var_type with
    | String -> Some value
    | Int -> ( try Some (int_of_string value) with _ -> None)
    | Float -> ( try Some (float_of_string value) with _ -> None)
    | Bool -> (
        match String.lowercase_ascii value with
        | "true" | "1" | "yes" | "on" -> Some true
        | "false" | "0" | "no" | "off" -> Some false
        | _ -> None)
    | Char -> if String.length value = 1 then Some value.[0] else None
  with Not_found -> None

let set_var ~name ~value =
  Kernel.Env.putenv name value;
  None

let vars () =
  Kernel.Env.environment () |> Array.to_list
  |> List.filter_map (fun s ->
      match String.index_opt s '=' with
      | None -> None
      | Some idx ->
          let name = String.sub s 0 idx in
          let value = String.sub s (idx + 1) (String.length s - idx - 1) in
          Some (name, value))

(* Legacy functions for compatibility *)
let home () = home_dir ()

let getenv key =
  try Ok (Kernel.Env.getenv_exn key)
  with Not_found -> Error (`EnvVarNotFound key)

let putenv key value =
  Kernel.Env.putenv key value;
  Ok ()

let get_home () = home_dir ()
