open Global

(** Metadata attached to log events *)
type t = {
  module_name: string option;
  function_name: string option;
  file: string option;
  line: int option;
  pid: Pid.t option;
  custom: (string * string) list;
}

let empty = {
  module_name = None;
  function_name = None;
  file = None;
  line = None;
  pid = None;
  custom = [];

}

let make = fun ?module_name ?function_name ?file ?line ?pid ?(custom = []) () -> {
  module_name;
  function_name;
  file;
  line;
  pid;
  custom
}

let merge = fun t1 t2 -> {
  module_name = Option.or_ t2.module_name t1.module_name;
  function_name = Option.or_ t2.function_name t1.function_name;
  file = Option.or_ t2.file t1.file;
  line = Option.or_ t2.line t1.line;
  pid = Option.or_ t2.pid t1.pid;
  custom = t2.custom @ t1.custom;

}

let to_string = fun t ->
  let parts = [] in
  let parts =
    match t.module_name with
    | None -> parts
    | Some m -> ("module=" ^ m) :: parts
  in
  let parts =
    match t.function_name with
    | None -> parts
    | Some f -> ("function=" ^ f) :: parts
  in
  let parts =
    match t.file with
    | None -> parts
    | Some f -> ("file=" ^ f) :: parts
  in
  let parts =
    match t.line with
    | None -> parts
    | Some l -> ("line=" ^ string_of_int l) :: parts
  in
  let parts =
    match t.pid with
    | None -> parts
    | Some p -> ("pid=" ^ Pid.to_string p) :: parts
  in
  (* Add custom fields *)
  let rec add_custom = fun acc ->
    function
    | [] -> acc
    | (k, v) :: rest -> add_custom ((k ^ "=" ^ v) :: acc) rest
  in
  let parts = add_custom parts t.custom in
  (* Reverse to get correct order *)
  let rec rev = fun acc ->
    function
    | [] -> acc
    | x :: xs -> rev (x :: acc) xs
  in
  let parts = rev [] parts in
  if parts = [] then
    ""
  else
    String.concat " " parts
