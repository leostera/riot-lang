open Std
open Std.Collections

module Slice = IO.IoVec.IoSlice

let append_subslice_unchecked = fun buffer slice ~off ~len ->
  match IO.Buffer.append_subslice buffer slice ~off ~len with
  | Ok () -> ()
  | Error error -> panic ("Printer.append_subslice: " ^ IO.IoVec.error_message error)

let to_string = fun ?(size_hint = 1_024) ?(final_newline = false) doc ->
  let buffer = IO.Buffer.create ~size:(Int.max 0 size_hint) in
  let rec write_indent indent =
    if indent > 0 then (
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
  let rec write = fun ~line_start ~indent ->
    function
    | Doc.Empty -> line_start
    | Doc.Text value -> write_text ~line_start ~indent value
    | Doc.RawText value -> write_raw_text ~line_start ~indent value
    | Doc.Slice value -> write_slice ~line_start ~indent value
    | Doc.Space ->
        if line_start then
          line_start
        else (
          IO.Buffer.add_char buffer ' ';
          false
        )
    | Doc.Spaces count ->
        if line_start then
          line_start
        else (
          for _ = 1 to count do
            IO.Buffer.add_char buffer ' '
          done;
          false
        )
    | Doc.Line ->
        IO.Buffer.add_char buffer '\n';
        true
    | Doc.Break flat -> write ~line_start ~indent (Doc.text flat)
    | Doc.Group group -> write ~line_start ~indent group.Doc.doc
    | Doc.Concat docs ->
        let rec loop line_start index =
          if Int.(index >= Vector.length docs) then
            line_start
          else
            loop (write
              ~line_start
              ~indent
              (Vector.get_unchecked docs ~at:index)) (Int.add index 1)
        in
        loop line_start 0
    | Doc.Indent (extra, doc) -> write ~line_start ~indent:(indent + extra) doc
  and write_text ~line_start ~indent value =
    let length = String.length value in
    let rec loop line_start segment_start index =
      if Int.(index >= length) then
        write_string_segment
          ~line_start
          ~indent
          value
          ~off:segment_start
          ~len:Int.(length - segment_start)
      else if Char.equal (String.get_unchecked value ~at:index) '\n' then
        (
          let _ =
            write_string_segment
              ~line_start
              ~indent
              value
              ~off:segment_start
              ~len:Int.(index - segment_start)
          in
          IO.Buffer.add_char buffer '\n';
          loop true Int.(index + 1) Int.(index + 1)
        )
      else
        loop line_start segment_start Int.(index + 1)
    in
    loop line_start 0 0
  and write_raw_text ~line_start ~indent value =
    let length = String.length value in
    if length = 0 then
      line_start
    else (
      if line_start then
        write_indent indent;
      IO.Buffer.add_string buffer value;
      Char.equal (String.get_unchecked value ~at:Int.(length - 1)) '\n'
    )
  and write_slice ~line_start ~indent ({ Doc.value; has_newline }: Doc.slice) =
    if not has_newline then
      write_slice_segment ~line_start ~indent value ~off:0 ~len:(Slice.length value)
    else
      let length = Slice.length value in
      if length = 0 then
        line_start
      else (
        if line_start then
          write_indent indent;
        append_subslice_unchecked buffer value ~off:0 ~len:length;
        Char.equal (Slice.get_unchecked value ~at:Int.(length - 1)) '\n'
      )
  in
  let line_start = write ~line_start:true ~indent:0 doc in
  if final_newline && IO.Buffer.length buffer > 0 && not line_start then
    IO.Buffer.add_char buffer '\n';
  IO.Buffer.contents buffer
