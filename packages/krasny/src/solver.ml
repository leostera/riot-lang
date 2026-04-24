open Std
open Std.Collections
module Slice = IO.IoVec.IoSlice

type mode =
  | Flat
  | Break

type frame = int * mode * Doc.t

let append_subslice_unchecked = fun buffer slice ~off ~len ->
  match IO.Buffer.append_subslice buffer slice ~off ~len with
  | Ok () -> ()
  | Error error -> panic ("Solver.append_subslice: " ^ Kernel.IO.Error.message error)

let last_line_width = fun text ->
  let length = String.length text in
  let rec loop index =
    if Int.compare index 0 < 0 then
      length
    else if Char.equal (String.get_unchecked text ~at:index) '\n' then
      Int.sub (Int.sub length index) 1
    else
      loop (Int.sub index 1)
  in
  loop (Int.sub length 1)

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

let rec push_many indent mode docs rest =
  let rec loop index acc =
    if Int.compare index 0 < 0 then
      acc
    else
      loop (Int.sub index 1) ((indent, mode, Vector.get_unchecked docs ~at:index) :: acc)
  in
  loop (Int.sub (Vector.length docs) 1) rest

let rec fits = fun ~width remaining ->
  function
  | [] -> true
  | _ when remaining < 0 -> false
  | (_, _, Doc.Empty) :: rest -> fits ~width remaining rest
  | (_, _, Doc.Text value) :: rest ->
      if String.contains value "\n" then
        fits ~width (width - last_line_width value) rest
      else
        fits ~width (remaining - String.length value) rest
  | (_, _, Doc.RawText value) :: rest ->
      if String.contains value "\n" then
        fits ~width (width - last_line_width value) rest
      else
        fits ~width (remaining - String.length value) rest
  | (_, _, Doc.Slice value) :: rest ->
      if value.Doc.has_newline then
        fits ~width (width - last_slice_line_width value) rest
      else
        fits ~width (remaining - Slice.length value.Doc.value) rest
  | (_, _, Doc.Space) :: rest -> fits ~width (remaining - 1) rest
  | (_, _, Doc.Spaces count) :: rest -> fits ~width (remaining - count) rest
  | (_, _, Doc.Line) :: _ -> true
  | (_, Flat, Doc.Break flat) :: rest -> fits ~width (remaining - String.length flat) rest
  | (_, Break, Doc.Break _) :: _ -> true
  | (indent, _, Doc.Group group) :: rest -> fits
    ~width
    remaining
    ((indent, Flat, group.Doc.doc) :: rest)
  | (indent, mode, Doc.Concat docs) :: rest -> fits
    ~width
    remaining
    (push_many indent mode docs rest)
  | (indent, mode, Doc.Indent (extra, doc)) :: rest -> fits
    ~width
    remaining
    ((indent + extra, mode, doc) :: rest)

let group_mode = fun ~width ~column ~indent group ->
  match group.Doc.flat_measure with
  | Some measure ->
      if Int.compare measure.Doc.flat_width (width - column) <= 0 then
        Flat
      else if fits ~width (width - column) [ (indent, Flat, group.Doc.doc) ] then
        Flat
      else
        Break
  | None ->
      if fits ~width (width - column) [ (indent, Flat, group.Doc.doc) ] then
        Flat
      else
        Break

let solve = fun ~width doc ->
  let rec solve_many ~column ~indent ~mode docs =
    let solved_docs = Vector.with_capacity ~size:(Vector.length docs) in
    let rec loop column index =
      if Int.compare index (Vector.length docs) >= 0 then
        (Doc.fast_concat_vector solved_docs, column)
      else
        let solved_doc, column = solve_doc
          ~column
          ~indent
          ~mode
          (Vector.get_unchecked docs ~at:index) in
        Vector.push solved_docs ~value:solved_doc;
        loop column (Int.add index 1)
    in
    loop column 0
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
    | Doc.Group group ->
        let mode = group_mode ~width ~column ~indent group in
        solve_doc ~column ~indent ~mode group.Doc.doc
  in
  solve_doc ~column:0 ~indent:0 ~mode:Break doc |> fun (solved, _) -> solved

let to_string = fun ~width ?(size_hint = 1_024) ?(final_newline = false) doc ->
  let buffer = IO.Buffer.create ~size:(Int.max 0 size_hint) in
  let rec write_indent indent =
    if indent > 0 then
      (
        IO.Buffer.add_char buffer ' ';
        write_indent (indent - 1)
      )
  in
  let write_string_segment ~line_start ~indent value ~off ~len =
    if line_start && len > 0 then
      write_indent indent;
    IO.Buffer.add_substring buffer value off len;
    line_start && len = 0
  in
  let write_slice_segment ~line_start ~indent value ~off ~len =
    if line_start && len > 0 then
      write_indent indent;
    append_subslice_unchecked buffer value ~off ~len;
    line_start && len = 0
  in
  let write_line indent =
    IO.Buffer.add_char buffer '\n';
    (true, indent)
  in
  let rec render_many ~line_start ~column ~indent ~mode docs =
    let length = Vector.length docs in
    let rec loop line_start column index =
      if Int.compare index length >= 0 then
        (line_start, column)
      else
        let line_start, column = render_doc
          ~line_start
          ~column
          ~indent
          ~mode
          (Vector.get_unchecked docs ~at:index) in
        loop line_start column (Int.add index 1)
    in
    loop line_start column 0
  and render_doc = fun ~line_start ~column ~indent ~mode ->
    function
    | Doc.Empty ->
        (line_start, column)
    | Doc.Text value ->
        render_text ~line_start ~column ~indent value
    | Doc.RawText value ->
        render_raw_text ~line_start ~column ~indent value
    | Doc.Slice value ->
        render_slice ~line_start ~column ~indent value
    | Doc.Space ->
        if line_start then
          (line_start, column + 1)
        else (
          IO.Buffer.add_char buffer ' ';
          (false, column + 1)
        )
    | Doc.Spaces count ->
        if line_start then
          (line_start, column + count)
        else (
          for _ = 1 to count do
            IO.Buffer.add_char buffer ' '
          done;
          (false, column + count)
        )
    | Doc.Line ->
        write_line indent
    | Doc.Break flat -> (
        match mode with
        | Flat -> render_text ~line_start ~column ~indent flat
        | Break -> write_line indent
      )
    | Doc.Group group ->
        let mode = group_mode ~width ~column ~indent group in
        render_doc ~line_start ~column ~indent ~mode group.Doc.doc
    | Doc.Concat docs ->
        render_many ~line_start ~column ~indent ~mode docs
    | Doc.Indent (extra, doc) ->
        let child_indent = indent + extra in
        let child_column =
          if column = indent then
            child_indent
          else
            column
        in
        render_doc ~line_start ~column:child_column ~indent:child_indent ~mode doc
  and render_text ~line_start ~column ~indent value =
    let length = String.length value in
    let rec loop line_start segment_start index saw_newline =
      if Int.(index >= length) then
        let line_start = write_string_segment
          ~line_start
          ~indent
          value
          ~off:segment_start
          ~len:Int.(length - segment_start) in
        let column =
          if saw_newline then
            Int.sub length segment_start
          else
            column + length
        in
        (line_start, column)
      else if Char.equal (String.get_unchecked value ~at:index) '\n' then
        (
          let _ = write_string_segment
            ~line_start
            ~indent
            value
            ~off:segment_start
            ~len:Int.(index - segment_start) in
          IO.Buffer.add_char buffer '\n';
          loop true Int.(index + 1) Int.(index + 1) true
        )
      else
        loop line_start segment_start Int.(index + 1) saw_newline
    in
    loop line_start 0 0 false
  and render_raw_text ~line_start ~column ~indent value =
    let length = String.length value in
    if length = 0 then
      (line_start, column)
    else (
      if line_start then
        write_indent indent;
      IO.Buffer.add_string buffer value;
      let line_start = Char.equal (String.get_unchecked value ~at:Int.(length - 1)) '\n' in
      let column =
        if String.contains value "\n" then
          last_line_width value
        else
          column + length
      in
      (line_start, column)
    )
  and render_slice ~line_start ~column ~indent (slice: Doc.slice) =
    if not slice.Doc.has_newline then
      let line_start = write_slice_segment
        ~line_start
        ~indent
        slice.Doc.value
        ~off:0
        ~len:(Slice.length slice.Doc.value) in
      (line_start, column + Slice.length slice.Doc.value)
    else
      let length = Slice.length slice.Doc.value in
      if length = 0 then
        (line_start, column)
      else (
        if line_start then
          write_indent indent;
        append_subslice_unchecked buffer slice.Doc.value ~off:0 ~len:length;
        let line_start = Char.equal (Slice.get_unchecked slice.Doc.value ~at:Int.(length - 1)) '\n' in
        (line_start, last_slice_line_width slice)
      )
  in
  let line_start, _ = render_doc ~line_start:true ~column:0 ~indent:0 ~mode:Break doc in
  if final_newline && IO.Buffer.length buffer > 0 && not line_start then
    IO.Buffer.add_char buffer '\n';
  IO.Buffer.contents buffer
