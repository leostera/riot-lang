open Std

type mode =
  | Flat
  | Break

type frame = int * mode * Doc.t

let last_line_width = fun text ->
  match List.rev (String.split_on_char '\n' text) with
  | [] -> 0
  | last :: _ -> String.length last

let solve = fun ~width doc ->
  let rec push_many indent mode docs rest =
    match List.rev docs with
    | [] -> rest
    | docs -> docs |> List.fold_left (fun acc doc -> (indent, mode, doc) :: acc) rest
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
  let rec solve_many = fun ~column ~indent ~mode ->
    function
    | [] -> (Doc.empty, column)
    | doc :: rest ->
        let solved_doc, column = solve_doc ~column ~indent ~mode doc in
        let solved_rest, column = solve_many ~column ~indent ~mode rest in
        (Doc.concat [ solved_doc; solved_rest ], column)
  and solve_doc = fun ~column ~indent ~mode ->
    function
    | Doc.Empty ->
        (Doc.empty, column)
    | Doc.Text value ->
        if String.contains value "\n" then
          (Doc.text value, last_line_width value)
        else
          (Doc.text value, column + String.length value)
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
  solve_doc ~column:0 ~indent:0 ~mode:Break doc |> fst
