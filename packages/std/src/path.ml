open Global
open Collections

(** Path manipulation module - Type-safe filesystem paths *)
type t = string

(* Internal representation - always valid UTF-8 *)

type error =
  | InvalidUtf8 of { path: string }
  | SystemInvalidUtf8 of { syscall: string; path: string }
  | SystemError of string

let is_absolute = fun path -> String.length path > 0 && path.[0] = '/'

let is_valid_utf8 = fun s ->
  try
    let len = String.length s in
    let rec check i =
      if i >= len then
        true
      else
        let c = Char.code s.[i] in
        if c < 0x80 then
          check (i + 1)
          (* ASCII *)
        else if c < 0xc0 then
          false
          (* Invalid start byte *)
        else if c < 0xe0 then
          if i + 1 >= len then
            false
          else if Char.code s.[i + 1] land 0xc0 != 0x80 then
            false
          else
            check (i + 2)
        else if c < 0xf0 then
          if i + 2 >= len then
            false
          else if Char.code s.[i + 1] land 0xc0 != 0x80 then
            false
          else if Char.code s.[i + 2] land 0xc0 != 0x80 then
            false
          else
            check (i + 3)
        else if c < 0xf8 then
          if i + 3 >= len then
            false
          else if Char.code s.[i + 1] land 0xc0 != 0x80 then
            false
          else if Char.code s.[i + 2] land 0xc0 != 0x80 then
            false
          else if Char.code s.[i + 3] land 0xc0 != 0x80 then
            false
          else
            check (i + 4)
        else
          false
    in
    check 0
  with
  | _ -> false

let of_string = fun s ->
  if is_valid_utf8 s then
    Result.ok s
  else
    Result.err (InvalidUtf8 { path = s })

let v = fun path -> of_string path |> Result.expect ~msg:("Invalid string path " ^ path)

let to_string = fun t -> t

let join = fun base path ->
  if is_absolute path then
    path
  else if base = "" then
    path
  else if base.[String.length base - 1] = '/' then
    base ^ path
  else
    base ^ "/" ^ path

let ( / ) = join

let dirname = fun path ->
  if path = "" then
    "."
  else if path = "/" then
    "/"
  else
    (* Split by separator, drop last element, rejoin by separator *)
    let parts = String.split_on_char '/' path in
    let without_last =
      match List.rev parts with
      | [] -> []
      | _ :: rest -> List.rev rest
    in
    let result = String.concat "/" without_last in
    if result = "" then
      "."
    else
      result

let parent = fun path ->
  match dirname path with
  | "." when path = "." -> None
  | ".." -> Some "../.."
  | "/" when path = "/" -> None
  | dir -> Some dir

let basename = fun path ->
  if path = "" then
    ""
  else if path = "/" then
    "/"
  else
    (* Split by separator, return last element *)
    let parts = String.split_on_char '/' path in
    match List.rev parts with
    | [] -> ""
    | last :: _ ->
        if last = "" then
          "/"
        else
          last

let extension = fun path ->
  let base = basename path in
  let len = String.length base in
  let rec find_dot i =
    if i < 1 then
      None
      (* Don't consider leading dot *)
    else if base.[i] = '.' then
      Some i
    else
      find_dot (i - 1)
  in
  match find_dot (len - 1) with
  | None -> None
  | Some i -> Some (String.sub base i (len - i))

let remove_extension = fun path ->
  let base = basename path in
  let dir = dirname path in
  let len = String.length base in
  let rec find_dot i =
    if i < 1 then
      len
      (* Don't consider leading dot *)
    else if base.[i] = '.' then
      i
    else
      find_dot (i - 1)
  in
  let base_without_ext = String.sub base 0 (find_dot (len - 1)) in
  if dir = "." then
    base_without_ext
  else if dir = "/" then
    "/" ^ base_without_ext
  else
    dir ^ "/" ^ base_without_ext

let add_extension = fun path ~ext ->
  let ext =
    if String.length ext > 0 && ext.[0] != '.' then
      "." ^ ext
    else
      ext
  in
  path ^ ext

let replace_extension = fun path ~ext -> add_extension (remove_extension path) ~ext

let is_relative = fun path -> not (is_absolute path)

let components = fun t ->
  if t = "" then
    []
  else if t = "/" then
    [ "/" ]
  else
    let parts = String.split_on_char '/' t in
    let rec build_components acc parts =
      match parts with
      | [] -> List.rev acc
      | "" :: rest when acc = [] ->
          (* Leading slash means absolute path *)
          build_components [ "/" ] rest
      | "" :: rest ->
          (* Empty component from // in path, skip it *)
          build_components acc rest
      | part :: rest -> build_components (part :: acc) rest
    in
    build_components [] parts

let rec normalize = fun path ->
  let parts = String.split_on_char '/' path in
  let rec process = fun acc ->
    function
    | [] ->
        List.rev acc
    | "." :: rest ->
        process acc rest
    | ".." :: rest -> (
        match acc with
        | [] -> process [ ".." ] rest
        | ".." :: _ -> process (".." :: acc) rest
        | _ :: acc' -> process acc' rest
      )
    | part :: rest ->
        process (part :: acc) rest
  in
  let normalized = process [] parts in
  let result = String.concat "/" normalized in
  if is_absolute path && result != "" && not (String.starts_with ~prefix:"/" result) then
    "/" ^ result
  else if result = "" then
    "."
  else
    result

let exists = fun path ->
  match Kernel.Fs.File.exists path with
  | Ok exists -> exists
  | Error _ -> false

let is_directory = fun path ->
  match Kernel.Fs.File.is_directory path with
  | Ok is_dir -> is_dir
  | Error _ -> false

let is_file = fun path -> exists path && not (is_directory path)

let equal = fun p1 p2 -> normalize p1 = normalize p2

let compare = fun p1 p2 ->
  String.compare (normalize p1) (normalize p2)

let strip_prefix = fun path ~prefix ->
  let path_components = components path in
  let prefix_components = components prefix in
  (* Recursively consume matching prefix components *)
  let rec consume path_parts prefix_parts =
    match (path_parts, prefix_parts) with
    | _, [] ->
        (* Prefix fully consumed, return remaining path components *)
        Result.ok path_parts
    | [], _ :: _ ->
        (* Ran out of path before consuming prefix *)
        Result.err
          (SystemError ("Path " ^ to_string path ^ " does not start with prefix " ^ to_string prefix))
    | p :: path_rest, pre :: prefix_rest ->
        if to_string p = to_string pre then
          consume path_rest prefix_rest
        else
          Result.err
            (SystemError ("Path " ^ to_string path ^ " does not start with prefix " ^ to_string prefix))
  in
  match consume path_components prefix_components with
  | Ok [] ->
      Result.ok (v "")
  | Ok remaining ->
      (* Join remaining components back into a path *)
      let result = List.fold_left join (List.hd remaining) (List.tl remaining) in
      Result.ok result
  | Error e ->
      Result.err e
