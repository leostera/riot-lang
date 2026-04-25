open Std
open Std.Collections

type format_error =
  | Cannot_parse of Syn.Diagnostic.t Vector.t
  | Cannot_lower of string

type write_error =
  | Format_failed of format_error
  | Write_failed of IO.error

let parse2_diagnostics_to_string = fun diagnostics ->
  let count = Vector.length diagnostics in
  if count = 0 then
    "parse2 diagnostics prevented formatting"
  else
    let first = Vector.get_unchecked diagnostics ~at:0 |> Syn.Diagnostic.to_string in
    if count = 1 then
      first
    else first ^ " (+" ^ Int.to_string (count - 1) ^ " more)"

let format_error_to_string = function
  | Cannot_parse diagnostics -> parse2_diagnostics_to_string diagnostics
  | Cannot_lower err -> err

let output_size_hint = fun (result: Syn.Parser.parse_result) -> IO.IoVec.IoSlice.length result.source + 1

let source_slice = fun source ->
  match IO.IoVec.IoSlice.from_string source with
  | Ok slice -> slice
  | Error error -> panic ("failed to create parser source slice: " ^ IO.IoSlice.error_message error)

let parse_source = fun ~filename source -> Syn.parse ~filename (source_slice source)

let buffer_writer = fun buffer ->
  let append_slice buffer slice =
    match IO.Buffer.append_slice buffer slice with
    | Ok () -> ()
    | Error error -> panic ("Format_core.buffer_writer: " ^ IO.IoVec.error_message error)
  in
  let module Write = struct
    type t = IO.Buffer.t

    let write = fun buffer ~from ->
      let len = IO.Buffer.readable_bytes from in
      append_slice buffer (IO.Buffer.readable from);
      Ok len

    let write_vectored = fun buffer ~from ->
      let written = ref 0 in
      IO.IoVec.for_each from ~fn:(
        fun segment ->
          append_slice buffer segment;
          written := !written + IO.IoSlice.length segment
      );
      Ok !written

    let flush = fun _buffer -> Ok ()
  end in
  IO.Writer.from_sink (module Write) buffer

let finalize_rendered_output = fun rendered ->
  if String.length rendered = 0 || String.ends_with ~suffix:"\n" rendered then
    rendered
  else rendered ^ "\n"

let stream_format = fun (result: Syn.Parser.parse_result) ~writer ~width ->
  yield ();
  let diagnostics = result.Syn.Parser.diagnostics in
  if Vector.length diagnostics > 0 then
    Error (Format_failed (Cannot_parse diagnostics))
  else
    let source_file = Syn.Ast.SourceFile.make result.Syn.Parser.tree in
    match Streaming_lower.write ~writer ~width source_file with
    | Error (Streaming_lower.Cannot_format err) -> Error (Format_failed (Cannot_lower (Streaming_lower.error_to_string err)))
    | Error (Streaming_lower.Cannot_write err) -> Error (Write_failed err)
    | Ok () ->
        yield ();
        Ok ()

let stream_format_to_string = fun (result: Syn.Parser.parse_result) ~width ->
  let buffer = IO.Buffer.create ~size:(output_size_hint result) in
  let writer = buffer_writer buffer in
  match stream_format result ~writer ~width with
  | Ok () -> Ok (IO.Buffer.contents buffer)
  | Error (Format_failed err) -> Error err
  | Error (Write_failed err) -> Error (Cannot_lower ("buffer write failed: " ^ IO.error_message err))

let write = fun ~writer result -> stream_format result ~writer ~width:100

let format = fun result -> stream_format_to_string result ~width:100

let format_source = fun ~filename source -> parse_source ~filename source |> stream_format_to_string ~width:100
