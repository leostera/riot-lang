open Std
module Slice = IO.IoVec.IoSlice

type mode =
  | Flat
  | Break

type frame = int * mode * Doc.t

let last_line_width = fun text ->
  match List.reverse (String.split text ~by:"\n") with
  | [] -> 0
  | last :: _ -> String.length last

let last_slice_line_width = fun value ->
  let slice = value.Doc.value in
  let rec find_last_newline index last_newline =
    if Int.(index >= Slice.length slice) then
      last_newline
    else if Char.equal (Slice.get_unchecked slice ~at:index) '\n' then
      find_last_newline Int.(index + 1) index
    else
      find_last_newline Int.(index + 1) last_newline
  in
  let last_newline = find_last_newline 0 (-1) in
  if Int.(last_newline < 0) then
    Slice.length slice
  else
    Int.(Slice.length slice - last_newline - 1)

let solve = fun ~width doc ->
  let rec push_many indent mode docs rest =
    match docs with
    | [] -> rest
    | doc :: tail -> (indent, mode, doc) :: push_many indent mode tail rest
  in
  let rec fits = fun remaining ->
    function
    | [] -> true
    | _ when remaining < 0 -> false
    | (_, _, Doc.Empty) :: rest -> fits remaining rest
    | (_, _, Doc.Text value) :: rest ->
        if String.contains value "\n" then
          fits (width - last_line_width value) rest
        else
          fits (remaining - String.length value) rest
    | (_, _, Doc.RawText value) :: rest ->
        if String.contains value "\n" then
          fits (width - last_line_width value) rest
        else
          fits (remaining - String.length value) rest
    | (_, _, Doc.Slice value) :: rest ->
        if value.Doc.has_newline then
          fits (width - last_slice_line_width value) rest
        else
          fits (remaining - Slice.length value.Doc.value) rest
    | (_, _, Doc.Space) :: rest -> fits (remaining - 1) rest
    | (_, _, Doc.Spaces count) :: rest -> fits (remaining - count) rest
    | (_, _, Doc.Line) :: _ -> true
    | (_, Flat, Doc.Break flat) :: rest -> fits (remaining - String.length flat) rest
    | (_, Break, Doc.Break _) :: _ -> true
    | (indent, _, Doc.Group doc) :: rest -> fits remaining ((indent, Flat, doc) :: rest)
    | (indent, mode, Doc.Concat docs) :: rest -> fits remaining (push_many indent mode docs rest)
    | (indent, mode, Doc.Indent (extra, doc)) :: rest -> fits
      remaining
      ((indent + extra, mode, doc) :: rest)
  in
  let rec solve_many ~column ~indent ~mode docs =
    let rec loop acc column = function
      | [] -> (Doc.concat (List.reverse acc), column)
      | doc :: rest ->
          let solved_doc, column = solve_doc ~column ~indent ~mode doc in
          loop (solved_doc :: acc) column rest
    in
    loop [] column docs
  and solve_doc = fun ~column ~indent ~mode ->
    function
    | Doc.Empty ->
        (Doc.empty, column)
    | Doc.Text value ->
        if String.contains value "\n" then
          (Doc.text value, last_line_width value)
        else
          (Doc.text value, column + String.length value)
    | Doc.RawText value ->
        if String.contains value "\n" then
          (Doc.raw_text value, last_line_width value)
        else
          (Doc.raw_text value, column + String.length value)
    | Doc.Slice value ->
        if value.Doc.has_newline then
          (Doc.slice ~has_newline:true value.Doc.value, last_slice_line_width value)
        else
          (Doc.slice ~has_newline:false value.Doc.value, column + Slice.length value.Doc.value)
    | Doc.Space ->
        (Doc.space, column + 1)
    | Doc.Spaces count ->
        (Doc.spaces count, column + count)
    | Doc.Line ->
        (Doc.line, indent)
    | Doc.Break flat -> (
        match mode with
        | Flat -> (Doc.text flat, column + String.length flat)
        | Break -> (Doc.line, indent)
      )
    | Doc.Concat docs ->
        solve_many ~column ~indent ~mode docs
    | Doc.Indent (extra, doc) ->
        let child_indent = indent + extra in
        let child_column =
          if column = indent then
            child_indent
          else
            column
        in
        let solved, column = solve_doc ~column:child_column ~indent:child_indent ~mode doc in
        (Doc.indent extra solved, column)
    | Doc.Group doc ->
        let mode =
          if fits (width - column) [ (indent, Flat, doc) ] then
            Flat
          else
            Break
        in
        solve_doc ~column ~indent ~mode doc
  in
  solve_doc ~column:0 ~indent:0 ~mode:Break doc |> fun (solved, _) -> solved
