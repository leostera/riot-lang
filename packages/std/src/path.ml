open Global
open Collections

type t = string

type error =
  | InvalidUtf8 of { path: string }
  | SystemInvalidUtf8 of { syscall: string; path: string }
  | SystemError of string

let is_absolute = fun path -> String.length path > 0 && String.get_unchecked path ~at:0 = '/'

let is_valid_utf8 = fun s ->
  try
    let len = String.length s in
    let rec check i =
      if i >= len then
        true
      else
        let c = Char.code (String.get_unchecked s ~at:i) in
        if c < 0x80 then
          check (i + 1)
        else
          if c < 0xc0 then
            false
          else
            if c < 0xe0 then
              if i + 1 >= len then
                false
              else
                if Char.code (String.get_unchecked s ~at:(i + 1)) land 0xc0 != 0x80 then
                  false
                else check (i + 2)
            else
              if c < 0xf0 then
                if i + 2 >= len then
                  false
                else
                  if Char.code (String.get_unchecked s ~at:(i + 1)) land 0xc0 != 0x80 then
                    false
                  else
                    if Char.code (String.get_unchecked s ~at:(i + 2)) land 0xc0 != 0x80 then
                      false
                    else check (i + 3)
              else
                if c < 0xf8 then
                  if i + 3 >= len then
                    false
                  else
                    if Char.code (String.get_unchecked s ~at:(i + 1)) land 0xc0 != 0x80 then
                      false
                    else
                      if Char.code (String.get_unchecked s ~at:(i + 2)) land 0xc0 != 0x80 then
                        false
                      else
                        if Char.code (String.get_unchecked s ~at:(i + 3)) land 0xc0 != 0x80 then
                          false
                        else check (i + 4)
                else false
    in
    check 0
  with
  | _ -> false

let from_string = fun s ->
  if is_valid_utf8 s then
    Result.ok s
  else Result.err (InvalidUtf8 { path = s })

let from_string_unchecked = fun s -> s

let v = fun path -> from_string path |> Result.expect ~msg:("Invalid string path " ^ path)

let to_string = fun t -> t

let join = fun base path ->
  if is_absolute path then
    path
  else
    if base = "" then
      path
    else
      if String.get_unchecked base ~at:(String.length base - 1) = '/' then
        base ^ path
      else base ^ "/" ^ path

let ( / ) = join

let dirname = fun path ->
  if path = "" then
    "."
  else
    if path = "/" then
      "/"
    else
      let parts = String.split ~by:"/" path in
      let without_last =
        match List.reverse parts with
        | [] -> []
        | _ :: rest -> List.reverse rest
      in
      let result = String.concat "/" without_last in
      if result = "" then
        "."
      else result

let parent = fun path ->
  match dirname path with
  | "." when path = "." -> None
  | ".." -> Some "../.."
  | "/" when path = "/" -> None
  | dir -> Some dir

let basename = fun path ->
  if path = "" then
    ""
  else
    if path = "/" then
      "/"
    else
      let parts = String.split ~by:"/" path in
      match List.reverse parts with
      | [] -> ""
      | last :: _ ->
          if last = "" then
            "/"
          else last

let extension = fun path ->
  let base = basename path in
  let len = String.length base in
  let rec find_dot i =
    if i < 1 then
      None
    else
      if String.get_unchecked base ~at:i = '.' then
        Some i
      else find_dot (i - 1)
  in
  match find_dot (len - 1) with
  | None -> None
  | Some i -> Some (String.sub base ~offset:i ~len:(len - i))

let remove_extension = fun path ->
  let base = basename path in
  let dir = dirname path in
  let len = String.length base in
  let rec find_dot i =
    if i < 1 then
      len
    else
      if String.get_unchecked base ~at:i = '.' then
        i
      else find_dot (i - 1)
  in
  let base_without_ext = String.sub base ~offset:0 ~len:(find_dot (len - 1)) in
  if dir = "." then
    base_without_ext
  else
    if dir = "/" then
      "/" ^ base_without_ext
    else dir ^ "/" ^ base_without_ext

let add_extension = fun path ~ext ->
  let ext =
    if String.length ext > 0 && String.get_unchecked ext ~at:0 != '.' then
      "." ^ ext
    else ext
  in
  path ^ ext

let replace_extension = fun path ~ext -> add_extension (remove_extension path) ~ext

let is_relative = fun path -> not (is_absolute path)

let components = fun t ->
  if t = "" then
    []
  else
    if t = "/" then
      [ "/" ]
    else
      let parts = String.split ~by:"/" t in
      let rec build_components acc parts =
        match parts with
        | [] -> List.reverse acc
        | "" :: rest when acc = [] -> build_components [ "/" ] rest
        | "" :: rest -> build_components acc rest
        | part :: rest -> build_components (part :: acc) rest
      in
      build_components [] parts

let rec normalize = fun path ->
  let parts = String.split ~by:"/" path in
  let rec process acc = function
    | [] -> List.reverse acc
    | "." :: rest -> process acc rest
    | ".." :: rest -> (
      match acc with
      | [] -> process [ ".." ] rest
      | ".." :: _ -> process (".." :: acc) rest
      | _ :: acc' -> process acc' rest
    )
    | part :: rest -> process (part :: acc) rest
  in
  let normalized = process [] parts in
  let result = String.concat "/" normalized in
  if is_absolute path && result != "" && not (String.starts_with ~prefix:"/" result) then
    "/" ^ result
  else
    if result = "" then
      "."
    else result

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

let compare = fun p1 p2 -> String.compare (normalize p1) (normalize p2)

let strip_prefix = fun path ~prefix ->
  let path_components = components path in
  let prefix_components = components prefix in
  let rec consume path_parts prefix_parts =
    match path_parts, prefix_parts with
    | _, [] -> Result.ok path_parts
    | [], _ :: _ -> Result.err (SystemError ("Path " ^ to_string path ^ " does not start with prefix " ^ to_string prefix))
    | p :: path_rest, pre :: prefix_rest ->
        if to_string p = to_string pre then
          consume path_rest prefix_rest
        else Result.err (SystemError ("Path " ^ to_string path ^ " does not start with prefix " ^ to_string prefix))
  in
  match consume path_components prefix_components with
  | Ok [] -> Result.ok (v "")
  | Ok remaining ->
      let result =
        match remaining with
        | first :: rest -> List.fold_left rest ~init:first ~fn:join
        | [] -> ""
      in
      Result.ok result
  | Error e -> Result.err e
