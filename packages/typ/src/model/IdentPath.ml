open Std

type module_name = string

type t =
  | Bare of string
  | Qualified of module_name * t

let empty = Bare ""

let is_empty = function
  | Bare "" -> true
  | _ -> false

let of_name = fun name -> Bare name

let is_uppercase_ascii = fun ch -> ch >= 'A' && ch <= 'Z'

let is_module_segment = fun segment ->
  String.length segment > 0 && is_uppercase_ascii segment.[0]

let of_segments = fun segments ->
  let rec loop = function
    | [] -> empty
    | [ name ] -> Bare name
    | module_name :: rest -> Qualified (module_name, loop rest)
  in
  loop segments

let of_string = fun text ->
  if String.equal text "" then
    empty
  else
    let segments = String.split_on_char '.' text in
    if List.exists String.is_empty segments then
      Bare text
    else
      match segments with
      | [] -> empty
      | [ name ] -> Bare name
      | prefix :: _ when is_module_segment prefix -> of_segments segments
      | _ -> Bare text

let to_segments =
  let rec loop acc = function
    | Bare "" -> List.rev acc
    | Bare name -> List.rev (name :: acc)
    | Qualified (module_name, tail) -> loop (module_name :: acc) tail
  in
  loop []

let to_string = fun path ->
  match to_segments path with
  | [] -> ""
  | segments -> String.concat "." segments

let rec equal = fun left right ->
  match (left, right) with
  | (Bare left_name, Bare right_name) -> String.equal left_name right_name
  | (Qualified (left_module, left_tail), Qualified (right_module, right_tail)) ->
      String.equal left_module right_module && equal left_tail right_tail
  | _ -> false

let rec compare = fun left right ->
  match (left, right) with
  | (Bare left_name, Bare right_name) -> String.compare left_name right_name
  | (Bare _, Qualified _) -> -1
  | (Qualified _, Bare _) -> 1
  | (Qualified (left_module, left_tail), Qualified (right_module, right_tail)) -> (
      match String.compare left_module right_module with
      | 0 -> compare left_tail right_tail
      | order -> order
    )

let rec append_name = fun path name ->
  match path with
  | Bare "" -> Bare name
  | Bare module_name -> Qualified (module_name, Bare name)
  | Qualified (module_name, tail) -> Qualified (module_name, append_name tail name)

let prepend_name = fun name path ->
  if is_empty path then
    Bare name
  else
    Qualified (name, path)

let append_path = fun left right ->
  match (left, right) with
  | (path, other) when is_empty path -> other
  | (path, other) when is_empty other -> path
  | _ ->
      of_segments (to_segments left @ to_segments right)

let last_name = fun path ->
  match List.rev (to_segments path) with
  | last :: _ -> Some last
  | [] -> None

let strip_prefix = fun ~prefix path ->
  let rec loop prefix_segments path_segments =
    match (prefix_segments, path_segments) with
    | ([], rest) -> Some (of_segments rest)
    | (prefix_segment :: prefix_rest, segment :: rest)
      when String.equal prefix_segment segment -> loop prefix_rest rest
    | _ -> None
  in
  loop (to_segments prefix) (to_segments path)

let prefixes = fun path ->
  let rec loop acc current = function
    | [] -> List.rev (empty :: acc)
    | segment :: rest ->
        let current = append_name current segment in
        loop (current :: acc) current rest
  in
  loop [] empty (to_segments path)
