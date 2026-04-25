open Std

type t =
  | Empty
  | Text of string
  | Space
  | Spaces of int
  | Line
  | Break of string
  | Group of t
  | Concat of t list
  | Indent of int * t

let empty = Empty

let str = fun value ->
  if value = "" then
    Empty
  else Text value

let space = Space

let spaces = fun count ->
  if count <= 0 then
    Empty
  else
    if count = 1 then
      Space
    else Spaces count

let brk = Break " "

let break = fun ?(flat = " ") () -> Break flat

let softline = Break ""

let line = Line

let hardline = Line

let concat = fun docs ->
  let add_spaces count acc =
    if count <= 0 then
      acc
    else
      match acc with
      | (Spaces current) :: rest -> Spaces (current + count) :: rest
      | Space :: rest -> Spaces (count + 1) :: rest
      | _ when count = 1 -> Space :: acc
      | _ -> Spaces count :: acc
  in
  let rec flatten acc = function
    | [] -> List.reverse acc
    | Empty :: rest -> flatten acc rest
    | Space :: rest -> flatten (add_spaces 1 acc) rest
    | (Spaces count) :: rest -> flatten (add_spaces count acc) rest
    | (Break flat) :: rest -> (
      match acc with
      | (Break current) :: _ when current = flat -> flatten acc rest
      | _ -> flatten (Break flat :: acc) rest
    )
    | (Group doc) :: rest -> flatten (Group doc :: acc) rest
    | (Concat nested) :: rest -> flatten acc (nested @ rest)
    | doc :: rest -> flatten (doc :: acc) rest
  in
  match flatten [] docs with
  | [] -> Empty
  | [ doc ] -> doc
  | flattened -> Concat flattened

let of_list = concat

let group = fun docs -> Group (concat docs)

let nest = fun amount docs ->
  let doc = concat docs in
  if amount <= 0 then
    doc
  else Indent (amount, doc)

let join = fun separator docs ->
  match docs with
  | [] -> Empty
  | first :: rest -> concat (first :: (rest |> List.map ~fn:(
    fun doc -> [ separator; doc ]
  ) |> List.concat))

type mode =
  | Flat
  | Broken

type frame = int * mode * t

let display_width = fun text -> String.width text

let last_line_width = fun text ->
  match List.reverse (String.split ~by:"\n" text) with
  | [] -> 0
  | last :: _ -> display_width last

let solve = fun ~width doc ->
  let rec push_many indent mode docs rest =
    match List.reverse docs with
    | [] -> rest
    | reversed -> reversed |> List.fold_left ~init:rest ~fn:(
      fun acc item -> (indent, mode, item) :: acc
    )
  in
  let rec fits remaining = function
    | _ when remaining < 0 -> false
    | [] -> true
    | (_, _, Empty) :: rest -> fits remaining rest
    | (_, _, Text value) :: rest ->
        if String.contains value "\n" then
          fits (width - last_line_width value) rest
        else fits (remaining - display_width value) rest
    | (_, _, Space) :: rest -> fits (remaining - 1) rest
    | (_, _, Spaces count) :: rest -> fits (remaining - count) rest
    | (_, _, Line) :: _ -> true
    | (_, Flat, Break flat) :: rest -> fits (remaining - display_width flat) rest
    | (_, Broken, Break _) :: _ -> true
    | (indent, _, Group child) :: rest -> fits remaining ((indent, Flat, child) :: rest)
    | (indent, mode, Concat docs) :: rest -> fits remaining (push_many indent mode docs rest)
    | (indent, mode, Indent (extra, child)) :: rest -> fits remaining ((indent + extra, mode, child) :: rest)
  in
  let rec solve_many ~column ~indent ~mode = function
    | [] -> empty, column
    | doc :: rest ->
        let solved_doc, column = solve_doc ~column ~indent ~mode doc in
        let solved_rest, column = solve_many ~column ~indent ~mode rest in (concat [ solved_doc; solved_rest ], column)
  and solve_doc ~column ~indent ~mode = function
    | Empty -> empty, column
    | Text value ->
        if String.contains value "\n" then
          (str value, last_line_width value)
        else (str value, column + display_width value)
    | Space -> space, column + 1
    | Spaces count -> spaces count, column + count
    | Line -> line, indent
    | Break flat -> (
      match mode with
      | Flat -> (str flat, column + display_width flat)
      | Broken -> (line, indent)
    )
    | Concat docs -> solve_many ~column ~indent ~mode docs
    | Indent (extra, child) ->
        let child_indent = indent + extra in
        let child_column =
          if column = indent then
            child_indent
          else column
        in
        let solved, column = solve_doc ~column:child_column ~indent:child_indent ~mode child in (Indent (extra, solved), column)
    | Group child ->
        let next_mode =
          if fits (width - column)
            [
              indent, Flat, child;
            ] then
            Flat
          else Broken
        in
        solve_doc ~column ~indent ~mode:next_mode child
  in
  let solved_doc, _column = solve_doc ~column:0 ~indent:0 ~mode:Broken doc in solved_doc

let to_string = fun doc ->
  let buffer = IO.Buffer.create ~size:256 in
  let rec write ~line_start ~indent = function
    | Empty -> line_start
    | Text value -> write_text ~line_start ~indent value
    | Space ->
        if line_start then
          line_start
        else
          (
            IO.Buffer.add_char buffer ' ';
            false
          )
    | Spaces count ->
        if line_start then
          line_start
        else
          (
            for _ = 1 to count do IO.Buffer.add_char buffer ' ' done;
            false
          )
    | Line ->
        IO.Buffer.add_char buffer '\n';
        true
    | Break flat -> write ~line_start ~indent (str flat)
    | Group child -> write ~line_start ~indent child
    | Concat docs -> List.fold_left docs ~init:line_start ~fn:(
      fun current_line_start child -> write ~line_start:current_line_start ~indent child
    )
    | Indent (extra, child) -> write ~line_start ~indent:(indent + extra) child
  and write_text ~line_start ~indent value =
    let rec write_lines line_start is_first = function
      | [] -> line_start
      | [ current ] ->
          if is_first && line_start && String.length current > 0 then
            IO.Buffer.add_string buffer (String.make ~len:indent ~char:' ');
          IO.Buffer.add_string buffer current;
          line_start && String.length current = 0
      | current :: rest ->
          if is_first && line_start && String.length current > 0 then
            IO.Buffer.add_string buffer (String.make ~len:indent ~char:' ');
          IO.Buffer.add_string buffer current;
          IO.Buffer.add_char buffer '\n';
          write_lines true false rest
    in
    write_lines line_start true (String.split ~by:"\n" value)
  in
  let _ = write ~line_start:true ~indent:0 doc in IO.Buffer.contents buffer

let normalize_width = fun width -> Int.max 0 width

let layout_doc = fun ?(width = 80) doc ->
  doc |> fun root -> solve ~width:(normalize_width width) (Group root) |> to_string

let layout = fun ?(width = 80) docs -> layout_doc ~width (concat docs)

let format_doc = layout_doc

let format = layout
