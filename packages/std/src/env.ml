(** Environment utilities *)

let args = Array.to_list Sys.argv |> List.tl

let current_dir () =
  try Sys.getcwd () |> Path.of_string
  with Sys_error msg -> Error (Path.SystemError msg)

let set_current_dir path =
  try
    Sys.chdir (Path.to_string path);
    Ok ()
  with Sys_error msg -> Error (Path.SystemError msg)

let home_dir () =
  try
    let home =
      Sys.getenv "HOME" |> Path.of_string
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
    let value = Sys.getenv name in
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
  Unix.putenv name value;
  None

let vars () =
  Unix.environment () |> Array.to_list
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
  try Ok (Sys.getenv key) with Not_found -> Error (`EnvVarNotFound key)

let putenv key value =
  Unix.putenv key value;
  Ok ()

let get_home () = home_dir ()
