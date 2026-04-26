open Std
open Std.Collections
module Slice = IO.IoVec.IoSlice

type mode =
  | Flat
  | Break

type writer = {
  buffer: IO.Buffer.t;
  sink: sink option;
  width: int;
  mutable line_start: bool;
  mutable column: int;
  mutable indent: int;
  mutable pending_spaces: int;
  mutable mode: mode;
}

and sink = {
  writer: IO.Writer.t;
  flush_threshold: int;
  mutable error: IO.error option;
}

type shape =
  | Atom
  | Text_shape of string
  | Raw_text_shape of string
  | Slice_shape of slice
  | Space_shape of int
  | Line_shape
  | Break_shape of string
  | Concat_shape of t Vector.t
  | Group_shape of t
  | Indent_shape of int * t

and t =
  | Empty
  | Node of node

and slice = {
  value: Slice.t;
  has_newline: bool;
}

and node = {
  flat_measure: Doc.flat_measure option;
  multiline: bool;
  shape: shape;
  emit: writer -> unit;
}

let empty = Empty

let is_empty = function
  | Empty -> true
  | _ -> false

let append_subslice_unchecked = fun buffer slice ~off ~len ->
  match IO.Buffer.append_subslice buffer slice ~off ~len with
  | Ok () -> ()
  | Error error -> panic ("Stream_doc.append_subslice: " ^ IO.IoVec.error_message error)

let flush = fun writer ->
  match writer.sink with
  | None -> Ok ()
  | Some sink -> (
      match sink.error with
      | Some error -> Error error
      | None ->
          if IO.Buffer.length writer.buffer = 0 then
            Ok ()
          else
            match IO.write_all sink.writer ~from:writer.buffer with
            | Ok () ->
                IO.Buffer.clear writer.buffer;
                Ok ()
            | Error error ->
                sink.error <- Some error;
                Error error
    )

let flush_if_needed = fun writer ->
  match writer.sink with
  | Some sink when IO.Buffer.length writer.buffer >= sink.flush_threshold -> ignore (flush writer)
  | _ -> ()

let last_line_width = fun text ->
  let length = String.length text in
  let rec loop index =
    if Int.(index < 0) then
      length
    else if Char.equal (String.get_unchecked text ~at:index) '\n' then
      Int.sub (Int.sub length index) 1
    else
      loop (Int.sub index 1)
  in
  loop (Int.sub length 1)

let last_slice_line_width = fun slice ->
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

let write_indent = fun writer ->
  let rec loop remaining =
    if remaining > 0 then
      (
        IO.Buffer.add_char writer.buffer ' ';
        loop (remaining - 1)
      )
  in
  loop writer.indent

let write_pending_spaces = fun writer ->
  let count = writer.pending_spaces in
  if count > 0 then
    (
      for _ = 1 to count do
        IO.Buffer.add_char writer.buffer ' '
      done;
      writer.pending_spaces <- 0;
      flush_if_needed writer
    )

let write_string_segment = fun writer value ~off ~len ->
  if len > 0 then
    (
      if writer.line_start then
        (
          writer.pending_spaces <- 0;
          write_indent writer
        )
      else
        write_pending_spaces writer;
      IO.Buffer.add_substring writer.buffer value off len;
      flush_if_needed writer;
      writer.line_start <- false
    )

let write_slice_segment = fun writer value ~off ~len ->
  if len > 0 then
    (
      if writer.line_start then
        (
          writer.pending_spaces <- 0;
          write_indent writer
        )
      else
        write_pending_spaces writer;
      append_subslice_unchecked writer.buffer value ~off ~len;
      flush_if_needed writer;
      writer.line_start <- false
    )

let emit_line = fun writer ->
  writer.pending_spaces <- 0;
  IO.Buffer.add_char writer.buffer '\n';
  flush_if_needed writer;
  writer.line_start <- true;
  writer.column <- writer.indent

let emit_text = fun writer value ->
  let length = String.length value in
  let rec loop segment_start index saw_newline =
    if Int.(index >= length) then
      (
        write_string_segment writer value ~off:segment_start ~len:Int.(length - segment_start);
        writer.column <- if saw_newline then
          Int.sub length segment_start
        else
          writer.column + length
      )
    else if Char.equal (String.get_unchecked value ~at:index) '\n' then
      (
        write_string_segment writer value ~off:segment_start ~len:Int.(index - segment_start);
        writer.pending_spaces <- 0;
        IO.Buffer.add_char writer.buffer '\n';
        writer.line_start <- true;
        loop Int.(index + 1) Int.(index + 1) true
      )
    else
      loop segment_start Int.(index + 1) saw_newline
  in
  loop 0 0 false

let is_horizontal_whitespace = fun char -> Char.equal char ' ' || Char.equal char '\t'

let raw_breaks_before_text = fun value ->
  let length = String.length value in
  let rec loop index =
    if Int.(index >= length) then
      false
    else
      let char = String.get_unchecked value ~at:index in
      if Char.equal char '\n' then
        true
      else if is_horizontal_whitespace char then
        loop (Int.add index 1)
      else
        false
  in
  loop 0

let trim_whitespace_only_segment = fun value ~start ~stop ->
  let rec loop index =
    if Int.(index >= stop) then
      start
    else if is_horizontal_whitespace (String.get_unchecked value ~at:index) then
      loop (Int.add index 1)
    else
      stop
  in
  loop start

let emit_raw_text = fun writer value ->
  let length = String.length value in
  if Int.(length > 0) then
    (
      let starts_with_break = raw_breaks_before_text value in
      if writer.line_start then
        (
          writer.pending_spaces <- 0;
          if not starts_with_break then
            write_indent writer
        )
      else if starts_with_break then
        writer.pending_spaces <- 0
      else
        write_pending_spaces writer;
      let rec loop segment_start index saw_newline =
        if Int.(index >= length) then
          (
            let segment_length = Int.sub length segment_start in
            if Int.(segment_length > 0) then
              IO.Buffer.add_substring writer.buffer value segment_start segment_length;
            saw_newline
          )
        else if Char.equal (String.get_unchecked value ~at:index) '\n' then
          (
            let segment_end = trim_whitespace_only_segment value ~start:segment_start ~stop:index in
            let segment_length = Int.sub segment_end segment_start in
            if Int.(segment_length > 0) then
              IO.Buffer.add_substring writer.buffer value segment_start segment_length;
            writer.pending_spaces <- 0;
            IO.Buffer.add_char writer.buffer '\n';
            flush_if_needed writer;
            loop Int.(index + 1) Int.(index + 1) true
          )
        else
          loop segment_start Int.(index + 1) saw_newline
      in
      let saw_newline = loop 0 0 false in
      flush_if_needed writer;
      writer.line_start <- Char.equal (String.get_unchecked value ~at:Int.(length - 1)) '\n';
      writer.column <- if saw_newline then
        last_line_width value
      else
        writer.column + length
    )

let emit_slice = fun writer ~has_newline value ->
  let length = Slice.length value in
  if Int.(length > 0) then
    if has_newline then
      (
        if writer.line_start then
          (
            writer.pending_spaces <- 0;
            write_indent writer
          )
        else
          write_pending_spaces writer;
        append_subslice_unchecked writer.buffer value ~off:0 ~len:length;
        writer.line_start <- Char.equal (Slice.get_unchecked value ~at:Int.(length - 1)) '\n';
        writer.column <- last_slice_line_width value
      )
    else (
      write_slice_segment writer value ~off:0 ~len:length;
      writer.column <- writer.column + length
    )

let emit_doc = fun writer ->
  function
  | Empty -> ()
  | Node node -> node.emit writer

let flat_measure_of_text = fun value ->
  if String.contains value "\n" then
    None
  else
    Some { Doc.flat_width = String.length value; stops_at_line = false }

let flat_measure_of_slice = fun ~has_newline value ->
  if has_newline then
    None
  else
    Some { Doc.flat_width = Slice.length value; stops_at_line = false }

let add_flat_measure = fun left right ->
  {
    Doc.flat_width = Int.add left.Doc.flat_width right.Doc.flat_width;
    stops_at_line = right.Doc.stops_at_line
  }

let flat_measure = function
  | Empty -> Some { Doc.flat_width = 0; stops_at_line = false }
  | Node node -> node.flat_measure

let flat_measure_vector = fun docs ->
  let length = Vector.length docs in
  let rec loop index measure =
    if measure.Doc.stops_at_line || Int.(index >= length) then
      Some measure
    else
      match flat_measure (Vector.get_unchecked docs ~at:index) with
      | None -> None
      | Some next -> loop (Int.add index 1) (add_flat_measure measure next)
  in
  loop 0 { Doc.flat_width = 0; stops_at_line = false }

let is_multiline = function
  | Empty -> false
  | Node node -> node.multiline

let is_multiline_vector = fun docs ->
  let length = Vector.length docs in
  let rec loop index =
    if Int.(index >= length) then
      false
    else if is_multiline (Vector.get_unchecked docs ~at:index) then
      true
    else
      loop (Int.add index 1)
  in
  loop 0

let node = fun ~flat_measure ~multiline ~shape ~emit -> Node { flat_measure; multiline; shape; emit }

let text = fun value ->
  if Int.(String.length value = 0) then
    Empty
  else
    node
      ~flat_measure:(flat_measure_of_text value)
      ~multiline:(String.contains value "\n")
      ~shape:(Text_shape value)
      ~emit:(fun writer -> emit_text writer value)

let raw_text = fun value ->
  if Int.(String.length value = 0) then
    Empty
  else
    node
      ~flat_measure:(flat_measure_of_text value)
      ~multiline:(String.contains value "\n")
      ~shape:(Raw_text_shape value)
      ~emit:(fun writer -> emit_raw_text writer value)

let slice = fun ~has_newline value ->
  if Int.(Slice.length value = 0) then
    Empty
  else
    node
      ~flat_measure:(flat_measure_of_slice ~has_newline value)
      ~multiline:has_newline
      ~shape:(Slice_shape { value; has_newline })
      ~emit:(fun writer -> emit_slice writer ~has_newline value)

let space =
  node ~flat_measure:(Some { Doc.flat_width = 1; stops_at_line = false }) ~multiline:false ~shape:(Space_shape 1)
    ~emit:(fun writer ->
      if writer.line_start then
        writer.column <- writer.column + 1
      else (
        writer.pending_spaces <- writer.pending_spaces + 1;
        writer.column <- writer.column + 1
      ))

let spaces = fun count ->
  if Int.(count <= 0) then
    Empty
  else if Int.(count = 1) then
    space
  else
    node ~flat_measure:(Some { Doc.flat_width = count; stops_at_line = false }) ~multiline:false ~shape:(Space_shape count)
      ~emit:(fun writer ->
        if writer.line_start then
          writer.column <- writer.column + count
        else (
          writer.pending_spaces <- writer.pending_spaces + count;
          writer.column <- writer.column + count
        ))

let line = node
  ~flat_measure:(Some { Doc.flat_width = 0; stops_at_line = true })
  ~multiline:true
  ~shape:Line_shape
  ~emit:emit_line

let break = fun ?(flat = " ") () ->
  node ~flat_measure:(Some { Doc.flat_width = String.length flat; stops_at_line = false }) ~multiline:false ~shape:(Break_shape flat)
    ~emit:(fun writer ->
      match writer.mode with
      | Flat -> emit_text writer flat
      | Break -> emit_line writer)

let softline = break ~flat:"" ()

let doc_vector = fun docs ->
  match Vector.length docs with
  | 0 -> Empty
  | 1 -> Vector.get_unchecked docs ~at:0
  | _ ->
      node ~flat_measure:(flat_measure_vector docs) ~multiline:(is_multiline_vector docs) ~shape:(Concat_shape docs)
        ~emit:(fun writer ->
          let length = Vector.length docs in
          let rec loop index =
            if Int.(index < length) then
              (
                emit_doc writer (Vector.get_unchecked docs ~at:index);
                loop (Int.add index 1)
              )
          in
          loop 0)

let fast_concat = function
  | [] -> Empty
  | [ doc ] -> doc
  | docs -> doc_vector (Vector.from_list docs)

let fast_concat_vector = doc_vector

let concat_with = fun ~iter ->
  let output = Vector.with_capacity ~size:8 in
  let push doc = Vector.push output ~value:doc in
  let add_spaces count =
    if Int.(count > 0) then
      let current_length = Vector.length output in
      if Int.equal current_length 0 then
        push (spaces count)
      else
        let last_index = Int.sub current_length 1 in
        match Vector.get_unchecked output ~at:last_index with
        | Node { shape=Space_shape current; _ } -> Vector.set_unchecked
          output
          ~at:last_index
          ~value:(spaces (Int.add current count))
        | _ -> push (spaces count)
  in
  let rec append_doc = function
    | Empty ->
        ()
    | Node { shape=Space_shape count; _ } ->
        add_spaces count
    | Node { shape=Break_shape flat; _ } as doc ->
        let current_length = Vector.length output in
        if Int.equal current_length 0 then
          push doc
        else
          let last_index = Int.sub current_length 1 in
          (
            match Vector.get_unchecked output ~at:last_index with
            | Node { shape=Break_shape current; _ } when String.equal current flat -> ()
            | _ -> push doc
          )
    | Node { shape=Concat_shape docs; _ } ->
        append_vector docs
    | doc ->
        push doc
  and append_vector docs =
    let length = Vector.length docs in
    let rec loop index =
      if Int.(index < length) then
        (
          append_doc (Vector.get_unchecked docs ~at:index);
          loop (Int.add index 1)
        )
    in
    loop 0
  in
  iter append_doc;
  doc_vector output

let concat = fun docs -> concat_with ~iter:(fun append_doc -> List.for_each docs ~fn:append_doc)

let concat_vector = fun docs ->
  concat_with ~iter:(fun append_doc -> Vector.for_each docs ~fn:append_doc)

let join = fun separator docs ->
  match docs with
  | [] -> Empty
  | [ doc ] -> doc
  | docs ->
      concat_with
        ~iter:(fun append_doc ->
          let rec loop first = function
            | [] -> ()
            | doc :: rest ->
                if not first then
                  append_doc separator;
                append_doc doc;
                loop false rest
          in
          loop true docs)

let join_vector = fun separator docs ->
  match Vector.length docs with
  | 0 -> Empty
  | 1 -> Vector.get_unchecked docs ~at:0
  | length ->
      concat_with
        ~iter:(fun append_doc ->
          let rec loop index =
            if Int.(index < length) then
              (
                if Int.(index > 0) then
                  append_doc separator;
                append_doc (Vector.get_unchecked docs ~at:index);
                loop (Int.add index 1)
              )
          in
          loop 0)

let fast_join = fun separator docs ->
  match docs with
  | [] ->
      Empty
  | [ doc ] ->
      doc
  | docs ->
      let output = Vector.with_capacity ~size:(Int.sub (Int.mul (List.length docs) 2) 1) in
      let rec loop first = function
        | [] -> ()
        | doc :: rest ->
            if not first then
              Vector.push output ~value:separator;
            Vector.push output ~value:doc;
            loop false rest
      in
      loop true docs;
      doc_vector output

let words = fun docs -> join space docs

let lines = fun docs -> join line docs

let rec push_many indent mode docs rest =
  let rec loop index acc =
    if Int.(index < 0) then
      acc
    else
      loop (Int.sub index 1) ((indent, mode, Vector.get_unchecked docs ~at:index) :: acc)
  in
  loop (Int.sub (Vector.length docs) 1) rest

let rec fits = fun ~width remaining ->
  function
  | [] -> true
  | _ when remaining < 0 -> false
  | (_, _, Empty) :: rest -> fits ~width remaining rest
  | (_, _, Node { shape=Text_shape value; _ }) :: rest ->
      if String.contains value "\n" then
        fits ~width (width - last_line_width value) rest
      else
        fits ~width (remaining - String.length value) rest
  | (_, _, Node { shape=Raw_text_shape value; _ }) :: rest ->
      if String.contains value "\n" then
        fits ~width (width - last_line_width value) rest
      else
        fits ~width (remaining - String.length value) rest
  | (_, _, Node { shape=Slice_shape slice; _ }) :: rest ->
      if slice.has_newline then
        fits ~width (width - last_slice_line_width slice.value) rest
      else
        fits ~width (remaining - Slice.length slice.value) rest
  | (_, _, Node { shape=Space_shape count; _ }) :: rest -> fits ~width (remaining - count) rest
  | (_, _, Node { shape=Line_shape; _ }) :: _ -> true
  | (_, Flat, Node { shape=Break_shape flat; _ }) :: rest -> fits
    ~width
    (remaining - String.length flat)
    rest
  | (_, Break, Node { shape=Break_shape _; _ }) :: _ -> true
  | (indent, _, Node { shape=Group_shape doc; _ }) :: rest -> fits
    ~width
    remaining
    ((indent, Flat, doc) :: rest)
  | (indent, mode, Node { shape=Concat_shape docs; _ }) :: rest -> fits
    ~width
    remaining
    (push_many indent mode docs rest)
  | (indent, mode, Node { shape=Indent_shape (extra, doc); _ }) :: rest -> fits
    ~width
    remaining
    ((indent + extra, mode, doc) :: rest)
  | (_, _, Node { shape=Atom; _ }) :: rest -> fits ~width remaining rest

let group_mode = fun writer doc flat_measure ->
  match flat_measure with
  | Some measure when Int.(measure.Doc.flat_width <= writer.width - writer.column) -> Flat
  | _ when fits ~width:writer.width (writer.width - writer.column) [ (writer.indent, Flat, doc); ] -> Flat
  | _ -> Break

let group = fun doc ->
  let flat_measure = flat_measure doc in
  node ~flat_measure ~multiline:(is_multiline doc) ~shape:(Group_shape doc)
    ~emit:(fun writer ->
      let previous_mode = writer.mode in
      writer.mode <- group_mode writer doc flat_measure;
      emit_doc writer doc;
      writer.mode <- previous_mode)

let indent = fun spaces doc ->
  if Int.(spaces <= 0) then
    doc
  else
    node ~flat_measure:(flat_measure doc) ~multiline:(is_multiline doc) ~shape:(Indent_shape (
      spaces,
      doc
    ))
      ~emit:(fun writer ->
        let previous_indent = writer.indent in
        let previous_column = writer.column in
        let child_indent = previous_indent + spaces in
        writer.indent <- child_indent;
        if Int.equal previous_column previous_indent then
          writer.column <- child_indent;
        emit_doc writer doc;
        writer.indent <- previous_indent)

let equal = text "="

let arrow = text "->"

let bar = text "|"

let colon = text ":"

let semi = text ";"

let comma = text ","

let lparen = text "("

let rparen = text ")"

let lbrace = text "{"

let rbrace = text "}"

let lbracket = text "["

let rbracket = text "]"

let to_string = fun ~width ?(size_hint = 1_024) ?(final_newline = false) doc ->
  let writer = {
    buffer = IO.Buffer.create ~size:(Int.max 0 size_hint);
    sink = None;
    width;
    line_start = true;
    column = 0;
    indent = 0;
    pending_spaces = 0;
    mode = Break;
  }
  in
  emit_doc writer doc;
  if final_newline && IO.Buffer.length writer.buffer > 0 && not writer.line_start then
    IO.Buffer.add_char writer.buffer '\n';
  IO.Buffer.contents writer.buffer

let write = fun ~writer ?(buffer_size = 4_096) ~width ?(final_newline = false) doc ->
  let sink = { writer; flush_threshold = Int.max 1 buffer_size; error = None } in
  let state = {
    buffer = IO.Buffer.create ~size:(Int.max 0 buffer_size);
    sink = Some sink;
    width;
    line_start = true;
    column = 0;
    indent = 0;
    pending_spaces = 0;
    mode = Break;
  }
  in
  emit_doc state doc;
  if final_newline && not state.line_start then
    IO.Buffer.add_char state.buffer '\n';
  match flush state with
  | Ok () -> (
      match sink.error with
      | Some error -> Error error
      | None -> IO.flush writer
    )
  | Error _ as error -> error
