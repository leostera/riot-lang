(** Path manipulation module - Type-safe filesystem paths *)

type t = string (* Internal representation - always valid UTF-8 *)

type error =
  | InvalidUtf8 of { path : string }
  | SystemInvalidUtf8 of { syscall : string; path : string }
  | SystemError of string

let is_absolute path = not (Filename.is_relative path)

let is_valid_utf8 s =
  try
    (* OCaml strings are byte sequences, we need to validate UTF-8 *)
    let len = String.length s in
    let rec check i =
      if i >= len then true
      else
        let c = Char.code s.[i] in
        if c < 0x80 then check (i + 1) (* ASCII *)
        else if c < 0xC0 then false (* Invalid start byte *)
        else if c < 0xE0 then (* 2-byte sequence *)
          if i + 1 >= len then false
          else if Char.code s.[i + 1] land 0xC0 <> 0x80 then false
          else check (i + 2)
        else if c < 0xF0 then (* 3-byte sequence *)
          if i + 2 >= len then false
          else if Char.code s.[i + 1] land 0xC0 <> 0x80 then false
          else if Char.code s.[i + 2] land 0xC0 <> 0x80 then false
          else check (i + 3)
        else if c < 0xF8 then (* 4-byte sequence *)
          if i + 3 >= len then false
          else if Char.code s.[i + 1] land 0xC0 <> 0x80 then false
          else if Char.code s.[i + 2] land 0xC0 <> 0x80 then false
          else if Char.code s.[i + 3] land 0xC0 <> 0x80 then false
          else check (i + 4)
        else false (* Invalid UTF-8 *)
    in
    check 0
  with _ -> false

let of_string s =
  if is_valid_utf8 s then Result.ok s else Result.err (InvalidUtf8 { path = s })

let v path =
  of_string path
  |> Result.expect ~msg:(Format.sprintf "Invalid string path %S" path)

let to_string t = t

let join base path =
  if is_absolute path then path
  else if base = "" then path
  else if base.[String.length base - 1] = '/' then base ^ path
  else base ^ "/" ^ path

let ( / ) = join

let parent path =
  match Filename.dirname path with
  | "." when path = "." -> None
  | ".." -> Some "../.."
  | "/" when path = "/" -> None
  | dir -> Some dir

let basename = Filename.basename
let dirname = Filename.dirname

let extension path =
  match Filename.extension path with "" -> None | ext -> Some ext

let remove_extension = Filename.remove_extension

let add_extension path ~ext =
  let ext =
    if String.length ext > 0 && ext.[0] <> '.' then "." ^ ext else ext
  in
  remove_extension path ^ ext

let is_relative = Filename.is_relative

let components t =
  if t = "" then []
  else if t = "/" then [ "/" ]
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

let rec normalize path =
  let parts = String.split_on_char '/' path in
  let rec process acc = function
    | [] -> List.rev acc
    | "." :: rest -> process acc rest
    | ".." :: rest -> (
        match acc with
        | [] -> process [ ".." ] rest
        | ".." :: _ -> process (".." :: acc) rest
        | _ :: acc' -> process acc' rest)
    | part :: rest -> process (part :: acc) rest
  in
  let normalized = process [] parts in
  let result = String.concat "/" normalized in
  if is_absolute path && result <> "" then "/" ^ result
  else if result = "" then "."
  else result

let exists path =
  match Kernel.Fs.File.file_exists path with
  | Ok exists -> exists
  | Error _ -> false

let is_directory path =
  match Kernel.Fs.File.is_directory path with
  | Ok is_dir -> is_dir
  | Error _ -> false

let is_file path = exists path && not (is_directory path)
let equal p1 p2 = normalize p1 = normalize p2

let strip_prefix path ~prefix =
  let path_str = to_string path in
  let prefix_str = to_string prefix in

  (* Normalize to ensure trailing slashes don't affect comparison *)
  let prefix_str =
    if prefix_str <> "" && prefix_str.[String.length prefix_str - 1] <> '/' then
      prefix_str ^ "/"
    else prefix_str
  in

  let prefix_len = String.length prefix_str in
  let path_len = String.length path_str in

  if path_len >= prefix_len && String.sub path_str 0 prefix_len = prefix_str then
    (* Path starts with prefix, return the remaining part *)
    let remaining = String.sub path_str prefix_len (path_len - prefix_len) in
    Result.ok (v remaining)
  else
    Result.err (SystemError (Printf.sprintf "Path %s does not start with prefix %s" path_str prefix_str))

let pp fmt path = Format.pp_print_string fmt (to_string path)
