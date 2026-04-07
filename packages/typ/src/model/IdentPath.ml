open Std

type module_name = string

type t =
  | Bare of string
  | Qualified of module_name * t

let empty = Bare ""

let is_empty = function
  | Bare "" -> true
  | _ -> false

let is_bare = function
  | Bare name when not (String.equal name "") -> true
  | _ -> false

let bare_name = function
  | Bare name when not (String.equal name "") -> Some name
  | _ -> None

let of_name = fun name -> Bare name

let is_uppercase_ascii = fun ch -> ch >= 'A' && ch <= 'Z'

let is_module_segment = fun segment -> String.length segment > 0 && is_uppercase_ascii segment.[0]

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
  | (Qualified (left_module, left_tail), Qualified (right_module, right_tail)) -> String.equal
    left_module
    right_module
  && equal left_tail right_tail
  | _ -> false

let rec compare = fun left right ->
  match (left, right) with
  | (Bare left_name, Bare right_name) ->
      String.compare left_name right_name
  | (Bare _, Qualified _) ->
      (-1)
  | (Qualified _, Bare _) ->
      1
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

let rec append_path = fun left right ->
  match (left, right) with
  | (path, other) when is_empty path -> other
  | (path, other) when is_empty other -> path
  | (Bare name, other) -> Qualified (name, other)
  | (Qualified (module_name, tail), other) -> Qualified (module_name, append_path tail other)

let rec last_name = function
  | Bare "" -> None
  | Bare name -> Some name
  | Qualified (_, tail) -> last_name tail

let uncons = function
  | Bare "" -> None
  | Bare name -> Some (name, empty)
  | Qualified (module_name, tail) -> Some (module_name, tail)

let rec split_last = function
  | Bare "" -> None
  | Bare _ -> None
  | Qualified (module_name, Bare name) -> Some (Bare module_name, name)
  | Qualified (module_name, tail) -> split_last tail
  |> Option.map (fun (prefix, name) -> (Qualified (module_name, prefix), name))

let rec strip_prefix = fun ~prefix path ->
  match (prefix, path) with
  | (Bare "", path) -> Some path
  | (Bare prefix_name, Bare path_name) ->
      if String.equal prefix_name path_name then
        Some empty
      else
        None
  | (Bare prefix_name, Qualified (module_name, tail)) ->
      if String.equal prefix_name module_name then
        Some tail
      else
        None
  | (Qualified (prefix_name, prefix_tail), Qualified (module_name, tail)) when String.equal
    prefix_name
    module_name -> strip_prefix ~prefix:prefix_tail tail
  | _ -> None

let prefixes = fun path ->
  let rec nonempty = function
    | Bare "" ->
        []
    | Bare name ->
        [ Bare name ]
    | Qualified (module_name, tail) ->
        let rest = nonempty tail in
        Bare module_name :: List.map (prepend_name module_name) rest
  in
  empty :: nonempty path
