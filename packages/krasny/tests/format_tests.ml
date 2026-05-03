open Std

let sample_ml = Path.v "sample.ml"

let workspace_files = [
  Path.v "packages/syn/src/cursor.mli";
  Path.v "packages/std/src/int.ml";
  Path.v "packages/std/src/bool.ml";
  Path.v "packages/std/src/option.ml";
  Path.v "packages/std/src/result.ml";
]

let parse_ml = fun source -> Krasny.parse_source ~filename:sample_ml source

let parse_mli = fun source -> Krasny.parse_source ~filename:(Path.v "sample.mli") source

let parse_file = fun path ->
  let source =
    Fs.read path
    |> Result.expect ~msg:"fixture file should exist"
  in
  Krasny.parse_source ~filename:path source

let with_tempdir = fun prefix fn ->
  match Fs.with_tempdir ~prefix fn with
  | Ok result -> result
  | Error err -> Error (IO.error_message err)

let buffer_writer = fun buffer ->
  let module Write = struct
    type t = IO.Buffer.t

    let write = fun buffer ~from ->
      let len = IO.Buffer.readable_bytes from in
      let _ =
        IO.Buffer.append_slice buffer (IO.Buffer.readable from)
        |> Result.expect ~msg:"failed to append writer buffer"
      in
      Ok len

    let write_vectored = fun buffer ~from ->
      let written = ref 0 in
      IO.IoVec.for_each
        from
        ~fn:(fun segment ->
          written := !written + IO.IoSlice.length segment;
          let _ =
            IO.Buffer.append_slice buffer segment
            |> Result.expect ~msg:"failed to append writer iovec segment"
          in
          ());
      Ok !written

    let flush = fun _buffer -> Ok ()
  end in
  IO.Writer.from_sink (module Write) buffer

let capture_write = fun result ->
  let buffer = IO.Buffer.create ~size:128 in
  let writer = buffer_writer buffer in
  match Krasny.stream_format result ~writer ~width:100 with
  | Ok () -> IO.Buffer.contents buffer
  | Error (Krasny.Format_failed err) ->
      panic
        ("stream_format should render into the supplied writer: "
        ^ Krasny.format_error_to_string err)
  | Error (Krasny.Write_failed err) ->
      panic ("stream_format should write into the supplied writer: " ^ IO.error_message err)

let has_trailing_horizontal_whitespace = fun text ->
  let length = String.length text in
  let rec loop index =
    if Int.(index >= length) then
      false
    else if Char.equal (String.get_unchecked text ~at:index) '\n' && Int.(index > 0) then
      let previous = String.get_unchecked text ~at:(Int.sub index 1) in
      if Char.equal previous ' ' || Char.equal previous '\t' then
        true
      else
        loop (Int.add index 1)
    else
      loop (Int.add index 1)
  in
  loop 0

let capture_json_event = fun ~root event ->
  let buffer = IO.Buffer.create ~size:128 in
  let writer = buffer_writer buffer in
  Krasny.Report.write_json_event ~writer ~root event
  |> Result.expect ~msg:"failed to serialize json event";
  IO.Buffer.contents buffer
  |> String.trim

let assert_json_timestamp_field = fun json ->
  match Data.Json.get_field "timestamp" json with
  | Some (Data.Json.String timestamp) ->
      Test.assert_true (String.contains timestamp "T");
      Test.assert_true (String.ends_with ~suffix:"Z" timestamp)
  | Some _ -> panic "timestamp field should be a JSON string"
  | None -> panic "timestamp field missing"

let assert_json_duration_ms_field = fun json ->
  match Data.Json.get_field "duration_ms" json with
  | Some (Data.Json.Int duration_ms) -> Test.assert_true (duration_ms >= 0)
  | Some _ -> panic "duration_ms field should be a JSON int"
  | None -> panic "duration_ms field missing"

let assert_idempotent = fun ~source ~msg ->
  let first =
    parse_ml source
    |> Krasny.format
    |> Result.expect ~msg
  in
  let second =
    parse_ml first
    |> Krasny.format
    |> Result.expect ~msg:"formatted output should reformat"
  in
  Test.assert_equal ~expected:first ~actual:second

let assert_formatted_ml_snapshot = fun ~ctx ~msg source ->
  let actual =
    parse_ml source
    |> Krasny.format
    |> Result.expect ~msg
  in
  Test.Snapshot.assert_text ~ctx ~actual

let assert_formatted_mli_snapshot = fun ~ctx ~msg source ->
  let actual =
    parse_mli source
    |> Krasny.format
    |> Result.expect ~msg
  in
  Test.Snapshot.assert_text ~ctx ~actual

let assert_format_ml_fails = fun ~msg source ->
  match parse_ml source
  |> Krasny.format with
  | Ok _ -> panic msg
  | Error _ -> Ok ()

let assert_format_mli_fails = fun ~msg source ->
  match parse_mli source
  |> Krasny.format with
  | Ok _ -> panic msg
  | Error _ -> Ok ()

let assert_roundtrip_hash = fun path ->
  let parsed = parse_file path in
  let original_hash = Krasny.syntax_hash parsed in
  let formatted =
    Krasny.format parsed
    |> Result.expect ~msg:"selected repo files should format"
  in
  let reparsed = Krasny.parse_source ~filename:path formatted in
  let reparsed_hash = Krasny.syntax_hash reparsed in
  Test.assert_equal ~expected:original_hash ~actual:reparsed_hash

let test_format_returns_the_original_source_for_a_simple_implementation = fun _ctx ->
  let source = "let x = 1 + 2\n" in
  let actual =
    parse_ml source
    |> Krasny.format
    |> Result.expect ~msg:"simple implementations should format"
  in
  Test.assert_equal ~expected:source ~actual;
  Ok ()

let test_format_source_uses_the_public_krasny_parse_facade = fun _ctx ->
  let source = "let x = 1 + 2\n" in
  let formatted =
    Krasny.format_source ~filename:sample_ml source
    |> Result.expect ~msg:"format_source should format"
  in
  let expected_hash =
    parse_ml source
    |> Krasny.syntax_hash
  in
  let actual_hash = Krasny.syntax_hash_source ~filename:sample_ml source in
  Test.assert_equal ~expected:source ~actual:formatted;
  Test.assert_equal ~expected:expected_hash ~actual:actual_hash;
  Ok ()

let test_syntax_hash_normalizes_formatter_safe_punctuation = fun _ctx ->
  let expected =
    parse_ml {ocaml|type row={x:int;y:int}
let check=pos+pattern_len>String.length str
|ocaml}
    |> Krasny.syntax_hash
  in
  let actual =
    parse_ml {ocaml|type row={x:int;y:int;}
let check=(pos+pattern_len)>(String.length str)
|ocaml}
    |> Krasny.syntax_hash
  in
  Test.assert_equal ~expected ~actual;
  Ok ()

let test_format_adds_a_final_newline_to_non_empty_output = fun _ctx ->
  let source = "let x = 1 + 2" in
  let actual =
    parse_ml source
    |> Krasny.format
    |> Result.expect ~msg:"formatted output should end with a final newline"
  in
  Test.assert_equal ~expected:"let x = 1 + 2\n" ~actual;
  Ok ()

let test_format_keeps_empty_files_empty = fun _ctx ->
  let actual =
    parse_ml ""
    |> Krasny.format
    |> Result.expect ~msg:"empty files should still format"
  in
  Test.assert_equal ~expected:"" ~actual;
  Ok ()

let test_write_renders_formatted_output_into_the_supplied_writer = fun _ctx ->
  let source = "let x = 1 + 2\n" in
  let parsed = parse_ml source in
  let expected =
    Krasny.format parsed
    |> Result.expect ~msg:"format should render the same source"
  in
  let actual = capture_write parsed in
  Test.assert_equal ~expected ~actual;
  Ok ()

let test_write_renders_simple_interfaces = fun ctx ->
  let source =
    {ocaml|val id : 'a -> 'a
type 'a t = 'a list
module type S = sig
  val run : unit -> unit
end
|ocaml}
  in
  let parsed = parse_mli source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|val id: 'a -> 'a

type 'a t = 'a list

module type S = sig
  val run: unit -> unit
end
|ocaml}

let test_write_normalizes_multiline_docstring_indentation = fun ctx ->
  let source =
    {ocaml|module ANSI:sig
(** Convert an ANSI palette entry to RGB.

          Use this when you need a concrete RGB value for a terminal color.

          Indices outside `0..255` are clamped to the nearest valid palette entry.

          Example:
          ```ocaml
          ANSI.to_rgb (`ansi 9) = `rgb (255, 0, 0)
          ```
      *)
val to_rgb:int->rgb
end
|ocaml}
  in
  let parsed = parse_mli source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|module ANSI: sig
  (**
     Convert an ANSI palette entry to RGB.

     Use this when you need a concrete RGB value for a terminal color.

     Indices outside `0..255` are clamped to the nearest valid palette entry.

     Example:
     ```ocaml
     ANSI.to_rgb (`ansi 9) = `rgb (255, 0, 0)
     ```
  *)
  val to_rgb: int -> rgb
end
|ocaml}

let test_write_preserves_relative_indentation_inside_docstrings = fun ctx ->
  let source =
    {ocaml|module Blink:sig
(** # SSE

    ```ocaml
    Blink.SSE.await conn
    |> Iter.MutIterator.for_each (fun event ->
         Log.info event.data)
    ```
*)
val await:unit->unit
end
|ocaml}
  in
  let parsed = parse_mli source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|module Blink: sig
  (**
     # SSE

     ```ocaml
     Blink.SSE.await conn
     |> Iter.MutIterator.for_each (fun event ->
          Log.info event.data)
     ```
  *)

  val await: unit -> unit
end
|ocaml}

let test_write_collapses_blank_lines_before_leading_comments = fun ctx ->
  let source = {ocaml|let test=fun _ctx ->




  (* keep *)
  let value=1 in value
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  Test.assert_false (has_trailing_horizontal_whitespace actual);
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|let test = fun _ctx ->
  (* keep *)
  let value = 1 in
  value
|ocaml}

let test_format_keeps_signature_docstring_spacing_idempotent = fun ctx ->
  let source =
    {ocaml|open Std
(** Interface that package commands must implement *)
module type Command=sig
val name:string
(** Command name (must match TOML declaration) *)
val command:ArgParser.command
(** Full ArgParser command with subcommands, args, etc. *)
val run:args:ArgParser.matches->(unit,string)result
(** Execute the command with parsed arguments *)
end
(** Global registry for dynamically loaded commands *)
module Registry:sig
val register:(module Command)->unit
(** Register a command (called by plugin initialization) *)
val get:string->(module Command)option
(** Lookup a registered command by name *)
val list:unit->(string*(module Command))list
(** List all registered commands *)
end
|ocaml}
  in
  let first =
    parse_mli source
    |> Krasny.format
    |> Result.expect ~msg:"signature docstrings should format"
  in
  let second =
    parse_mli first
    |> Krasny.format
    |> Result.expect ~msg:"formatted signature docstrings should reformat"
  in
  Test.assert_equal ~expected:first ~actual:second;
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual:first
    ~expected:{ocaml|open Std

(** Interface that package commands must implement *)
module type Command = sig
  val name: string

  (** Command name (must match TOML declaration) *)
  val command: ArgParser.command

  (** Full ArgParser command with subcommands, args, etc. *)
  val run: args:ArgParser.matches -> (unit, string) result

  (** Execute the command with parsed arguments *)
end

(** Global registry for dynamically loaded commands *)
module Registry: sig
  val register: (module Command) -> unit

  (** Register a command (called by plugin initialization) *)
  val get: string -> (module Command) option

  (** Lookup a registered command by name *)
  val list: unit -> (string * (module Command)) list

  (** List all registered commands *)
end
|ocaml}

let test_write_preserves_terminal_docstrings_before_nested_signature_end = fun ctx ->
  let source = {ocaml|module S:sig
val x:int
(** keep me *)
end
|ocaml}
  in
  let parsed = parse_mli source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|module S: sig
  val x: int

  (** keep me *)
end
|ocaml}

let test_write_preserves_module_functor_parameters = fun ctx ->
  let source = {ocaml|module Make(Order:Std_order.Ordered)=struct
let empty=Empty
end
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|module Make (Order: Std_order.Ordered) = struct
  let empty = Empty
end
|ocaml}

let test_write_keeps_adjacent_module_structures_separated = fun ctx ->
  let source =
    {ocaml|module Tcp:Intf=struct
let name="tcp"
end
module Tls:Intf=struct
let name="tls"
end
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|module Tcp: Intf = struct
  let name = "tcp"
end

module Tls: Intf = struct
  let name = "tls"
end
|ocaml}

let test_write_preserves_variant_constructor_docstrings_in_interfaces = fun ctx ->
  let source =
    {ocaml|type 'a parse_result=|Done of{value:'a;remaining:string}
(** Successfully parsed + remaining input *)
|Need_more
(** Need more data to continue parsing *)
|Error of string
(** Parse error with message *)
val find_substring:needle:string->string->int option
|ocaml}
  in
  let parsed = parse_mli source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|type 'a parse_result =
  | Done of { value: 'a; remaining: string }
  (** Successfully parsed + remaining input *)
  | Need_more
  (** Need more data to continue parsing *)
  | Error of string

(** Parse error with message *)
val find_substring: needle:string -> string -> int option
|ocaml}

let test_write_renders_short_record_types_inline = fun ctx ->
  let source =
    {ocaml|type error=|OutOfBoundsSet of{length:int;at:int}
type row={left:int;right:string}
|ocaml}
  in
  let parsed = parse_mli source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|type error =
  | OutOfBoundsSet of { length: int; at: int }
type row = { left: int; right: string }
|ocaml}

let test_write_keeps_heavy_record_types_vertical = fun ctx ->
  let source =
    {ocaml|type constructor_description=unit and constructor_binding={description:constructor_description;ordinal:int}
|ocaml}
  in
  let parsed = parse_mli source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|type constructor_description = unit

and constructor_binding = {
  description: constructor_description;
  ordinal: int;
}
|ocaml}

let test_write_preserves_terminal_record_field_docstrings = fun ctx ->
  let source =
    {ocaml|type cookie={name:string;value:string;same_site:same_site option;
(** CSRF protection *)}
|ocaml}
  in
  let parsed = parse_mli source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|type cookie = {
  name: string;
  value: string;
  same_site: same_site option;
  (** CSRF protection *)
}
|ocaml}

let test_write_renders_type_alias_record_representations = fun ctx ->
  let source =
    {ocaml|type point=Base.point=private{x:int;y:string}
type export_entry=Manifest.export_entry={name:string;path:Std.Path.t;action_hash:string}
|ocaml}
  in
  let parsed = parse_mli source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|type point = Base.point = private { x: int; y: string }
type export_entry = Manifest.export_entry = {
  name: string;
  path: Std.Path.t;
  action_hash: string;
}
|ocaml}

let test_write_breaks_record_type_aliases_when_width_is_exceeded = fun ctx ->
  let source =
    {ocaml|type export_entry=Manifest.export_entry={name:string;path:Std.Path.t;action_hash:string}
|ocaml}
  in
  let actual =
    Krasny.stream_format_to_string (parse_mli source) ~width:60
    |> Result.expect ~msg:"record type alias should format"
  in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|type export_entry = Manifest.export_entry = {
  name: string;
  path: Std.Path.t;
  action_hash: string;
}
|ocaml}

let test_write_preserves_unit_parameter_return_annotation = fun ctx ->
  let source = {ocaml|let create_counters ():runtime_counters={steals=Atomic.make 0}
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|let create_counters (): runtime_counters = { steals = Atomic.make 0 }
|ocaml}

let test_write_preserves_function_return_annotation_before_record_body = fun ctx ->
  let source =
    {ocaml|let gc_delta ~(before:stat) ~(after_:stat):Bench_result.gc_stats={minor_collections=0}
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|let gc_delta ~(before:stat) ~(after_:stat) : Bench_result.gc_stats = { minor_collections = 0 }
|ocaml}

let test_write_preserves_local_function_return_annotation_before_record_body = fun ctx ->
  let source =
    {ocaml|let parse_dependency raw_name value=
let name=raw_name in
let make_dependency source:Package.dependency={name;source} in
make_dependency value
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|let parse_dependency raw_name value =
  let name = raw_name in
  let make_dependency source: Package.dependency = { name; source } in
  make_dependency value
|ocaml}

let test_write_parenthesizes_annotated_record_expressions = fun ctx ->
  let source = {ocaml|let to_kernel_tm=fun tm->({tm_sec=tm.tm_sec}:Kernel.Time.tm)
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|let to_kernel_tm = fun tm -> ({ tm_sec = tm.tm_sec }: Kernel.Time.tm)
|ocaml}

let test_write_parenthesizes_annotated_identifier_expressions = fun ctx ->
  let source = {ocaml|let as_node=fun (value:t)->(value:node)
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|let as_node = fun (value: t) -> (value: node)
|ocaml}

let test_write_preserves_coercion_expressions = fun ctx ->
  let source = {ocaml|let f rgb=to_string (rgb:>color)
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|let f rgb = to_string (rgb :> color)
|ocaml}

let test_write_keeps_nullary_constructor_pattern_payloads_atomic = fun ctx ->
  let source =
    {ocaml|let f=function|Ok Protocol.Event {handler_id;event_data}->event_data
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|let f = fun (Ok Protocol.Event { handler_id; event_data }) -> event_data
|ocaml}

let test_write_desugars_function_expressions = fun ctx ->
  let source = {ocaml|let classify=function|Some value->value|None->0
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|let classify = fun __tmp1 ->
  match __tmp1 with
  | Some value -> value
  | None -> 0
|ocaml}

let test_write_desugars_single_case_function_to_fun_pattern = fun ctx ->
  let source = {ocaml|let message=function|Error msg->msg
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|let message = fun (Error msg) -> msg
|ocaml}

let test_write_parenthesizes_single_case_poly_variant_payload_function = fun ctx ->
  let source =
    {ocaml|let rgb_tuple_of=function|`rgb(r,g,b)->(r,g,b)
let ansi_value=function|`ansi actual->actual
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|let rgb_tuple_of = fun (`rgb (r, g, b)) -> (r, g, b)

let ansi_value = fun (`ansi actual) -> actual
|ocaml}

let test_write_desugars_function_application_arguments = fun ctx ->
  let source = {ocaml|let values=List.map(function|Some value->value|None->0) options
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|let values =
  List.map
    (fun __tmp1 ->
      match __tmp1 with
      | Some value -> value
      | None -> 0)
    options
|ocaml}

let test_write_preserves_labeled_parameter_order_around_wildcards = fun ctx ->
  let source =
    {ocaml|let f=fun ~poll:_ ~server ~server_addr:_ ~client ~client_addr:_->server
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|let f = fun ~poll:_ ~server ~server_addr:_ ~client ~client_addr:_ -> server
|ocaml}

let test_write_preserves_labeled_binding_return_annotation = fun ctx ->
  let source =
    {ocaml|let make ~start ~finish ~steps : color array =
Array.make ~count:steps ~value:start
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|let make ~start ~finish ~steps : color array = Array.make ~count:steps ~value:start
|ocaml}

let test_write_breaks_long_let_binding_parameters = fun ctx ->
  let source =
    {ocaml|let inline_record_constructor_description name type_ (owner:Ast.type_declaration) (constructor:Ast.type_constructor) (field:Ast.record_field_declaration):Env.constructor_description={description;ordinal}
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|let inline_record_constructor_description
  name
  type_
  (owner: Ast.type_declaration)
  (constructor: Ast.type_constructor)
  (field: Ast.record_field_declaration)
  : Env.constructor_description = {
  description;
  ordinal;
}
|ocaml}

let test_write_preserves_assignment_operators = fun ctx ->
  let source =
    {ocaml|let update state remaining=state.buffer<-remaining
let assign r=r:=1
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|let update state remaining =
  state.buffer <- remaining

let assign r =
  r := 1
|ocaml}

let test_write_preserves_labeled_wildcard_function_parameters = fun ctx ->
  let source = {ocaml|let run=fun ~args:_->()
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text ~ctx ~actual ~expected:{ocaml|let run = fun ~args:_ -> ()
|ocaml}

let test_write_preserves_renamed_labeled_function_parameters = fun ctx ->
  let source =
    {ocaml|let index_of=fun value ~char:needle ~fn:predicate->if predicate needle then Some value else None
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|let index_of = fun value ~char:needle ~fn:predicate ->
  if predicate needle then
    Some value
  else
    None
|ocaml}

let test_write_preserves_let_binding_annotations_exactly_once = fun ctx ->
  let source =
    {ocaml|let make:reader:IO.Reader.t->writer:IO.Writer.t->of_io_error:(IO.error->Error.t)->uri:Net.Uri.t->t=fun ~reader ~writer ~of_io_error ~uri->Ok ()
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|let make:
  reader:IO.Reader.t ->
  writer:IO.Writer.t ->
  of_io_error:(IO.error -> Error.t) ->
  uri:Net.Uri.t ->
  t = fun ~reader ~writer ~of_io_error ~uri -> Ok ()
|ocaml}

let test_write_preserves_comments_before_else_tokens = fun ctx ->
  let source =
    {ocaml|let parse line=if line="" then()else if String.starts_with ~prefix:":" line then()(* keep comment *)else()
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|let parse line =
  if line = "" then
    ()
  else if String.starts_with ~prefix:":" line then
    ()
  (* keep comment *)
  else
    ()
|ocaml}

let test_write_preserves_eof_owned_trailing_comments = fun ctx ->
  let source =
    {ocaml|let close=fun _conn->()
(* Reader/writer don't need explicit close *)
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|let close = fun _conn -> ()
(* Reader/writer don't need explicit close *)
|ocaml}

let test_write_keeps_multiline_ordinary_comments_idempotent = fun ctx ->
  let source = {ocaml|module M=struct
(*
   first
     nested
*)
let value=1
end
|ocaml}
  in
  let first =
    parse_ml source
    |> Krasny.format
    |> Result.expect ~msg:"multiline ordinary comments should format"
  in
  let second =
    parse_ml first
    |> Krasny.format
    |> Result.expect ~msg:"formatted multiline ordinary comments should reformat"
  in
  Test.assert_equal ~expected:first ~actual:second;
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual:first
    ~expected:{ocaml|module M = struct
  (*
     first
       nested
  *)

  let value = 1
end
|ocaml}

let test_write_preserves_shallow_ordinary_comment_indentation = fun ctx ->
  let source = {ocaml|(* TODO:
  - keep this markdown indent
*)
let value=1
|ocaml}
  in
  let first =
    parse_ml source
    |> Krasny.format
    |> Result.expect ~msg:"ordinary comment should format"
  in
  let second =
    parse_ml first
    |> Krasny.format
    |> Result.expect ~msg:"formatted ordinary comment should reformat"
  in
  Test.assert_equal ~expected:first ~actual:second;
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual:first
    ~expected:{ocaml|(* TODO:
  - keep this markdown indent
*)

let value = 1
|ocaml}

let test_write_normalizes_multiline_ordinary_comment_continuation_indentation = fun ctx ->
  let source =
    {ocaml|let link=fun ()->
let target_platform=target() in
(* NOTE: Dependency ld_flags must be collected here during linking, not in the profile.
                                             The profile contains only the current package's target-specific flags (applied in|ocaml}
    ^ " "
    ^ {ocaml|
                                             package_planner). When linking, we need flags from ALL dependencies transitively,
                                             which can only be determined at link-time based on the depset. *)
let transitive_deps=deps() in
transitive_deps
|ocaml}
  in
  let first =
    parse_ml source
    |> Krasny.format
    |> Result.expect ~msg:"multiline ordinary comment should format"
  in
  let second =
    parse_ml first
    |> Krasny.format
    |> Result.expect ~msg:"formatted multiline ordinary comment should reformat"
  in
  Test.assert_equal ~expected:first ~actual:second;
  Test.assert_false (has_trailing_horizontal_whitespace first);
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual:first
    ~expected:{ocaml|let link = fun () ->
  let target_platform = target () in
  (* NOTE: Dependency ld_flags must be collected here during linking, not in the profile.
     The profile contains only the current package's target-specific flags (applied in
     package_planner). When linking, we need flags from ALL dependencies transitively,
     which can only be determined at link-time based on the depset.
  *)
  let transitive_deps = deps () in
  transitive_deps
|ocaml}

let test_format_keeps_constructor_record_update_arguments_idempotent = fun ctx ->
  let source =
    {ocaml|let parse_payload=fun frame payload_data->
match frame.frame_type with
|Frame.Headers->Ok {frame with payload=Frame.HeadersPayload {pad_length=None;stream_dependency=None;weight=None;exclusive=false;header_block_fragment=payload_data;}}
|Frame.Data->Ok {frame with payload=Frame.DataPayload {data=payload_data;pad_length=None}}
|ocaml}
  in
  let first =
    parse_ml source
    |> Krasny.format
    |> Result.expect ~msg:"constructor record update argument should format"
  in
  let second =
    parse_ml first
    |> Krasny.format
    |> Result.expect ~msg:"formatted constructor record update argument should reformat"
  in
  Test.assert_equal ~expected:first ~actual:second;
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual:first
    ~expected:{ocaml|let parse_payload = fun frame payload_data ->
  match frame.frame_type with
  | Frame.Headers ->
      Ok {
        frame with
        payload =
          Frame.HeadersPayload {
            pad_length = None;
            stream_dependency = None;
            weight = None;
            exclusive = false;
            header_block_fragment = payload_data;
          };
      }
  | Frame.Data ->
      Ok { frame with payload = Frame.DataPayload { data = payload_data; pad_length = None } }
|ocaml}

let test_write_renders_parenthesized_type_constructor_arguments = fun ctx ->
  let source =
    {ocaml|val set:'value t->at:int->value:'value->(unit,error) Kernel.result
val mapper:(int->string) option
val pair:('a*'b) box
val nested:('a,'e) result option
val triple:(int,string,bool) Graph_scheduler.node_result
type close=Close of int option*string
type timing={microseconds:int*int}
|ocaml}
  in
  let parsed = parse_mli source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|val set: 'value t -> at:int -> value:'value -> (unit, error) Kernel.result

val mapper: (int -> string) option

val pair: ('a * 'b) box

val nested: ('a, 'e) result option

val triple: (int, string, bool) Graph_scheduler.node_result

type close =
  | Close of int option * string
type timing = {
  microseconds: int * int;
}
|ocaml}

let test_write_renders_operator_value_declarations = fun ctx ->
  let source =
    {ocaml|val(=):'a->'a->bool
val( |> ):'a->('a->'b)->'b
val( * ):int->int->int
val( ** ):float->float->float
|ocaml}
  in
  let parsed = parse_mli source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|val ( = ): 'a -> 'a -> bool

val ( |> ): 'a -> ('a -> 'b) -> 'b

val ( * ): int -> int -> int

val ( ** ): float -> float -> float
|ocaml}

let test_write_renders_include_declarations = fun ctx ->
  let source = {ocaml|include   Kernel.Process
module type S=sig include  Map.S end
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|include Kernel.Process

module type S = sig
  include Map.S
end
|ocaml}

let test_write_renders_adjacent_opens_tightly = fun ctx ->
  let source = {ocaml|open Std
open Std.Collections
let value=1
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|open Std
open Std.Collections

let value = 1
|ocaml}

let test_write_renders_record_updates = fun ctx ->
  let source = {ocaml|let next={state with count=state.count+1;ready=true}
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|let next = { state with count = state.count + 1; ready = true }
|ocaml}

let test_write_renders_coercion_expressions = fun ctx ->
  let source =
    {ocaml|let color=((`rgb (1,2,3):>color))
let named=(value:>Service.t)
let cast=(value:source:>target)
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|let color = (`rgb (1, 2, 3) :> color)

let named = (value :> Service.t)

let cast = (value : source :> target)
|ocaml}

let test_write_renders_local_open_patterns = fun ctx ->
  let source = {ocaml|let fin=fun Frame.{fin;opcode}->fin
let value=fun M.(Some x)->x
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|let fin = fun Frame.{ fin; opcode } -> fin

let value = fun M.(Some x) -> x
|ocaml}

let test_write_renders_operator_bindings_and_local_open_operator_values = fun ctx ->
  let source =
    {ocaml|let(+)=add
let( let* )=bind
let ( .@() ) x y=get x y
let mul=Stdlib.( * )
let modulo=Stdlib.(mod)
let value=M.(Some x)
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|let ( + ) = add

let ( let* ) = bind

let ( .@() ) x y = get x y

let mul = Stdlib.( * )

let modulo = Stdlib.( mod )

let value = M.(Some x)
|ocaml}

let test_write_renders_polymorphic_variants_with_qualified_record_payloads = fun ctx ->
  let source =
    {ocaml|let item=Some(`Definition Cst.ClassDefinition.{syntax_node=node;class_body=body})
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|let item = Some (`Definition Cst.ClassDefinition.{ syntax_node = node; class_body = body })
|ocaml}

let test_write_renders_binding_operator_expressions = fun ctx ->
  let source =
    {ocaml|let one=let* item=read () in Ok item
let both=let* a=read_a () and* b=read_b () in Ok (a,b)
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|let one =
  let* item = read () in
  Ok item

let both =
  let* a = read_a ()
  and* b = read_b ()
  in
  Ok (a, b)
|ocaml}

let test_write_keeps_fun_applications_bare = fun ctx ->
  let source = {ocaml|let ()=start ~apps:[]@@fun ()->main ()
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|let () = start ~apps:[] @@ fun () -> main ()
|ocaml}

let test_write_keeps_pipeline_fun_operands_bare = fun ctx ->
  let source =
    {ocaml|let headers=headers|>fun h->Net.Http.Header.add h "host" "localhost"
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|let headers =
  headers
  |> fun h -> Net.Http.Header.add h "host" "localhost"
|ocaml}

let test_write_parenthesizes_tuple_function_bodies = fun ctx ->
  let source = {ocaml|let pair=fun a b->(a,b)
let triple=map3(fun a b c->(a,b,c))x y z
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|let pair = fun a b -> (a, b)

let triple = map3 (fun a b c -> (a, b, c)) x y z
|ocaml}

let test_write_parenthesizes_tuple_let_bodies_inside_function_arguments = fun ctx ->
  let source =
    {ocaml|let make=list_init count(fun index->let weight=if heavy&&index=0 then 99 else 1 in (weight,Generator.return index))
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|let make =
  list_init
    count
    (fun index ->
      let weight =
        if heavy && index = 0 then
          99
        else
          1
      in
      (weight, Generator.return index))
|ocaml}

let test_write_parenthesizes_tuple_if_branches = fun ctx ->
  let source =
    {ocaml|let make=fun res fastest->if res.name=fastest.name then res.name,1.0 else let ratio=calculate fastest.mean res.mean in(res.name,ratio)
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|let make = fun res fastest ->
  if res.name = fastest.name then
    (res.name, 1.0)
  else
    let ratio = calculate fastest.mean res.mean in
    (res.name, ratio)
|ocaml}

let test_write_keeps_infix_application_operands_bare_when_precedence_allows_it = fun ctx ->
  let source =
    {ocaml|let length=String.length b|>Int.to_string
let line=Net.Http.Method.to_string method_^" "^resource^"\r\n"
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|let length =
  String.length b
  |> Int.to_string

let line = Net.Http.Method.to_string method_ ^ " " ^ resource ^ "\r\n"
|ocaml}

let test_write_keeps_low_precedence_right_infix_operands_bare = fun ctx ->
  let source =
    {ocaml|let ok=response<>""&&let parts=String.split ~by:":" response in match parts with|[_;value]->String.trim value=expected|_->false
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|let ok =
  response <> "" && let parts = String.split ~by:":" response in
  match parts with
  | [ _; value ] -> String.trim value = expected
  | _ -> false
|ocaml}

let test_write_keeps_application_on_the_right_side_of_exponent_infix = fun ctx ->
  let source = {ocaml|let multiplier=10.0**Float.from_int precision
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|let multiplier = 10.0 ** Float.from_int precision
|ocaml}

let test_write_keeps_bare_polymorphic_variant_application_arguments_split = fun ctx ->
  let source =
    {ocaml|let seq=Color.to_escape_seq ~mode:`fg color
let ok=assert_relation `Satisfied (Pubgrub.Partial_solution.relation solution incompat)
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|let seq = Color.to_escape_seq ~mode:`fg color

let ok = assert_relation `Satisfied (Pubgrub.Partial_solution.relation solution incompat)
|ocaml}

let test_write_keeps_field_access_bound_to_polyvariant_payloads = fun ctx ->
  let source =
    {ocaml|let schedule=fun t timer->t.overflow:=timer::!(t.overflow)
let event=fun state->`Paste state.paste_buffer
let constrained=fun state->`Constrained state.ranges
let events=fun state events->(`Paste state.paste_buffer)::events
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|let schedule = fun t timer -> t.overflow := timer :: !(t.overflow)

let event = fun state -> `Paste state.paste_buffer

let constrained = fun state -> `Constrained state.ranges

let events = fun state events -> (`Paste state.paste_buffer) :: events
|ocaml}

let test_write_keeps_qualified_record_field_accesses = fun ctx ->
  let source =
    {ocaml|let root=tree.Syntax_tree.root
let body=leaf.Syntax_tree.body_raw
let span=first.Raw_token.span
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|let root = tree.Syntax_tree.root

let body = leaf.Syntax_tree.body_raw

let span = first.Raw_token.span
|ocaml}

let test_write_keeps_qualified_record_fields_in_patterns = fun ctx ->
  let source =
    {ocaml|let get_root=fun syntax->match syntax with|{Syntax_tree.root;Raw_token.span=span}->{root;span}
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|let get_root = fun syntax ->
  match syntax with
  | { Syntax_tree.root; Raw_token.span = span } -> { root; span }
|ocaml}

let test_write_keeps_cons_chains_right_associative = fun ctx ->
  let source =
    {ocaml|let fields=timestamp_field()::("type",Data.Json.String"file")::fields
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|let fields = timestamp_field () :: ("type", Data.Json.String "file") :: fields
|ocaml}

let test_write_keeps_infix_precedence_parens_minimal = fun ctx ->
  let source =
    {ocaml|let values=fun slice last_newline line_start len->let width=Int.(Slice.length slice-last_newline-1) in let ok=line_start&&len>0 in if ok then width else 0
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|let values = fun slice last_newline line_start len ->
  let width = Int.(Slice.length slice - last_newline - 1) in
  let ok = line_start && len > 0 in
  if ok then
    width
  else
    0
|ocaml}

let test_write_parenthesizes_tuple_sequence_operands = fun ctx ->
  let source =
    {ocaml|let render=fun line_start column->if line_start then(line_start,column+1)else(IO.Buffer.add_char buffer ' ';(false,column+1))
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|let render = fun line_start column ->
  if line_start then
    (line_start, column + 1)
  else (
    IO.Buffer.add_char buffer ' ';
    (false, column + 1)
  )
|ocaml}

let test_write_trims_pending_spaces_before_record_field_docstrings = fun ctx ->
  let source =
    {ocaml|type context={(** Path of the file being checked. *)file_path:string;(** Original source text. *)source:string;(** Parsed source file CST. *)cst:Syn.Cst.source_file}
|ocaml}
  in
  let parsed = parse_mli source in
  let actual = capture_write parsed in
  Test.assert_false (has_trailing_horizontal_whitespace actual);
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|type context = {
  (** Path of the file being checked. *)
  file_path: string;
  (** Original source text. *)
  source: string;
  (** Parsed source file CST. *)
  cst: Syn.Cst.source_file;
}
|ocaml}

let test_write_parenthesizes_list_tuple_items_with_match_components = fun ctx ->
  let source =
    {ocaml|let fields=[("error",match result.error with|Some error->String error|None->Null);("diagnostics",match result.diagnostics with|Some diagnostics->Array diagnostics|None->Null)]
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|let fields = [
  ("error", match result.error with
  | Some error -> String error
  | None -> Null);
  ("diagnostics", match result.diagnostics with
  | Some diagnostics -> Array diagnostics
  | None -> Null);
]
|ocaml}

let test_write_parenthesizes_keyword_body_sequences = fun ctx ->
  let source =
    {ocaml|let run=fun cond->if cond then done_ else (let value=next() in push value;loop())
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|let run = fun cond ->
  if cond then
    done_
  else
    (
      let value = next () in
      push value;
      loop ()
    )
|ocaml}

let test_write_keeps_paired_parenthesized_if_branches = fun ctx ->
  let source =
    {ocaml|let render cond=if cond then(render value)else(emit first;emit second)
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|let render cond =
  if cond then (
    render value
  ) else (
    emit first;
    emit second
  )
|ocaml}

let test_write_keeps_parenthesized_then_blocks_attached = fun ctx ->
  let source =
    {ocaml|let render result=if Vector.is_empty result.diagnostics.items then(let intf=render_interface result.intf in let source=if String.equal intf "" then "" else intf in Interface source)else Interface ""
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|let render result =
  if Vector.is_empty result.diagnostics.items then (
    let intf = render_interface result.intf in
    let source =
      if String.equal intf "" then
        ""
      else
        intf
    in
    Interface source
  ) else
    Interface ""
|ocaml}

let test_write_keeps_match_case_keyword_body_sequences_unwrapped = fun ctx ->
  let source =
    {ocaml|let f=fun cond->match x with|Some data->if cond then let result=foo in bar;baz else qux
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  let reparsed = parse_ml actual in
  Test.assert_equal ~expected:(Krasny.syntax_hash parsed) ~actual:(Krasny.syntax_hash reparsed);
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|let f = fun cond ->
  match x with
  | Some data ->
      if cond then
        let result = foo in
        bar;
        baz
      else
        qux
|ocaml}

let test_write_renders_loop_expressions = fun ctx ->
  let source =
    {ocaml|let sum=ref 0
let ()=for i=0 to 10 do sum:=!sum+i done
let ()=while !sum<100 do sum:=!sum+1 done
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|let sum = ref 0

let () =
  for i = 0 to 10 do
    sum := !sum + i
  done

let () =
  while !sum < 100 do
    sum := !sum + 1
  done
|ocaml}

let test_write_normalizes_trailing_sequence_semicolons = fun ctx ->
  let source = {ocaml|let run=fun ()->match x with|A->foo ();bar ();|B->baz ()
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|let run = fun () ->
  match x with
  | A ->
      foo ();
      bar ();
  | B -> baz ()
|ocaml}

let test_write_keeps_local_let_in_on_inline_bindings = fun ctx ->
  let source =
    {ocaml|let run=fun response body->let status=Net.Http.Response.status response in Log.info "Status: %a" Net.Http.Status.pp status;Log.info "Body length: %d bytes" (String.length body);()
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|let run = fun response body ->
  let status = Net.Http.Response.status response in
  Log.info "Status: %a" Net.Http.Status.pp status;
  Log.info "Body length: %d bytes" (String.length body);
  ()
|ocaml}

let test_write_breaks_multiline_match_application_arguments = fun ctx ->
  let source =
    {ocaml|let main=fun result->match result with|Error e->Log.error "Request failed: %s" (match e with|`Connection_failed msg->format "Connection: %s" msg|`Read_error msg->format "Read: %s" msg)|Ok body->Log.info "Body: %s" body
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|let main = fun result ->
  match result with
  | Error e ->
      Log.error
        "Request failed: %s"
        (
          match e with
          | `Connection_failed msg -> format "Connection: %s" msg
          | `Read_error msg -> format "Read: %s" msg
        )
  | Ok body -> Log.info "Body: %s" body
|ocaml}

let test_write_breaks_parenthesized_infix_arguments_containing_matches = fun ctx ->
  let source =
    {ocaml|let show=fun first_event->Log.info ("First event: "^(match first_event with|Some _->"got one"|None->"none"))
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|let show = fun first_event ->
  Log.info
    (
      "First event: " ^ (
        match first_event with
        | Some _ -> "got one"
        | None -> "none"
      )
    )
|ocaml}

let test_write_renders_assert_expressions = fun ctx ->
  let source = {ocaml|let check=fun x->assert(x=1);assert(not(x=2))
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|let check = fun x ->
  assert (x = 1);
  assert (not (x = 2))
|ocaml}

let test_write_renders_local_exception_expressions = fun ctx ->
  let source =
    {ocaml|let run=fun value->let exception Stop of int*string in raise(Stop(value,"x"))
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|let run = fun value ->
  let exception Stop of int * string in
  raise (Stop (value, "x"))
|ocaml}

let test_write_renders_attribute_and_extension_expressions = fun ctx ->
  let source =
    {ocaml|let tagged=match result with|Ok ()->Ok ()[@test]|Error err->Error err[@test]
let loc=Loc.get[%atomic.loc value.contents]
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|let tagged =
  match result with
  | Ok () -> Ok () [@test]
  | Error err -> Error err [@test]

let loc = Loc.get [%atomic.loc value.contents]
|ocaml}

let test_write_preserves_structure_item_attribute_suffixes = fun ctx ->
  let source =
    {ocaml|module Tests=struct let x=1 end[@test]
module type S=sig val x:int end[@test]
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  let reparsed = parse_ml actual in
  Test.assert_equal ~expected:(Krasny.syntax_hash parsed) ~actual:(Krasny.syntax_hash reparsed);
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|module Tests = struct
  let x = 1
end [@test]

module type S = sig
  val x: int
end [@test]
|ocaml}

let test_write_preserves_signature_item_attribute_suffixes = fun ctx ->
  let source = {ocaml|module Tests:sig val x:int end[@test]
|ocaml}
  in
  let parsed = parse_mli source in
  let actual = capture_write parsed in
  let reparsed = parse_mli actual in
  Test.assert_equal ~expected:(Krasny.syntax_hash parsed) ~actual:(Krasny.syntax_hash reparsed);
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|module Tests: sig
  val x: int
end [@test]
|ocaml}

let test_write_renders_let_module_expressions = fun ctx ->
  let source =
    {ocaml|let value=let module M=Existing in M.run ()
let run=let module Box=struct
let x=1
let y=x+1
end in Box.y
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|let value =
  let module M = Existing in
  M.run ()

let run =
  let module Box = struct
    let x = 1

    let y = x + 1
  end in
  Box.y
|ocaml}

let test_write_renders_first_class_module_expressions = fun ctx ->
  let source =
    {ocaml|let sink=IO.Writer.from_sink (module Write) ()
let packed=(module Service:Service.Intf)
let init=let module R=(val config.reporter:Reporter.Intf) in R.init ()
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|let sink = IO.Writer.from_sink (module Write) ()

let packed = (module Service : Service.Intf)

let init =
  let module R = (val config.reporter : Reporter.Intf) in
  R.init ()
|ocaml}

let test_write_renders_first_class_module_packs_and_unpacks_with_constraints = fun ctx ->
  let source =
    {ocaml|let unpack=let module D=(val driver) in D.run()
let unpack_ascribed=let module P=(val server.protocol_mod:Common.ApplicationProtocol with type request=req and type response=res) in P.run()
let pack=make (module FilterIter:Intf with type state=a t and type item=a) iter
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|let unpack =
  let module D = (val driver) in
  D.run ()

let unpack_ascribed =
  let module P = (val server.protocol_mod : Common.ApplicationProtocol with type request = req and type response = res) in
  P.run ()

let pack = make (module FilterIter : Intf with type state = a t and type item = a) iter
|ocaml}

let test_write_renders_gadt_inline_record_constructors = fun ctx ->
  let source =
    {ocaml|type t=| Conn:{protocol:(module Protocol.Intf);mutable state:int}->t
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|type t =
  | Conn: {
      protocol: (module Protocol.Intf);
      mutable state: int;
    } -> t
|ocaml}

let test_write_renders_first_class_module_type_aliases = fun ctx ->
  let source =
    {ocaml|type 'dst sink=(module Write with type t='dst)
type ('item,'state) iter=(module Intf with type item='item and type state='state)
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|type 'dst sink = (module Write with type t = 'dst)

type ('item, 'state) iter = (module Intf with type item = 'item and type state = 'state)
|ocaml}

let test_write_renders_type_extension_declarations = fun ctx ->
  let source =
    {ocaml|type Message.t+=|Actor_self_reply of Pid.t|Actor_stop
type _ Effect.t+=Yield:unit Effect.t
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|type Message.t +=
  | Actor_self_reply of Pid.t
  | Actor_stop

type _ Effect.t +=
  | Yield: unit Effect.t
|ocaml}

let test_write_renders_extensible_and_private_type_declarations = fun ctx ->
  let source =
    {ocaml|type event=..
type 'a effect='a eff=..
type raw=private int
type state=private|Uninitialized|Runnable
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|type event = ..

type 'a effect = 'a eff = ..

type raw = private int

type state =
  private | Uninitialized
  | Runnable
|ocaml}

let test_write_renders_module_type_of_declarations = fun ctx ->
  let source =
    {ocaml|module Protocol:module type of Protocol
module Error:module type of Error
|ocaml}
  in
  let parsed = parse_mli source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|module Protocol: module type of Protocol

module Error: module type of Error
|ocaml}

let test_write_renders_constrained_module_declarations = fun ctx ->
  let source =
    {ocaml|module Make(Order:Order.Ordered):S with type key=Order.t
module Driver:Sqlx_driver.Driver.Intf with type config=Config.t
|ocaml}
  in
  let parsed = parse_mli source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|module Make (Order: Order.Ordered): S with type key = Order.t

module Driver: Sqlx_driver.Driver.Intf with type config = Config.t
|ocaml}

let test_write_preserves_chained_module_type_constraint_connectors = fun ctx ->
  let source =
    {ocaml|module Protocol:Jsonrpc.ApplicationProtocol with type request=request and type response=response=struct end
module type S=sig include Jsonrpc.ApplicationProtocol with type request:=request and type response:=response end
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  let reparsed = parse_ml actual in
  Test.assert_equal ~expected:(Krasny.syntax_hash parsed) ~actual:(Krasny.syntax_hash reparsed);
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|module Protocol: Jsonrpc.ApplicationProtocol with type request = request and type response = response = struct end

module type S = sig
  include Jsonrpc.ApplicationProtocol with type request := request and type response := response
end
|ocaml}

let test_write_renders_applied_and_ascribed_module_declarations = fun ctx ->
  let source =
    {ocaml|module Name_map=Collections.Map.Make(String)
module Owner_map=Collections.Map.Make(struct
type t=TypeConstructorId.t
let compare=TypeConstructorId.compare
end)
module Mock:Driver.Intf with type config=unit=struct
type config=unit
let name="Mock"
end
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|module Name_map = Collections.Map.Make (String)

module Owner_map = Collections.Map.Make (struct
  type t = TypeConstructorId.t

  let compare = TypeConstructorId.compare
end)

module Mock: Driver.Intf with type config = unit = struct
  type config = unit

  let name = "Mock"
end
|ocaml}

let test_write_renders_polymorphic_variant_type_aliases = fun ctx ->
  let source =
    {ocaml|type decode_error=[`Invalid_octal|`Other of string]
type color=[ansi|rgb]
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|type decode_error = [`Invalid_octal | `Other of string]

type color = [ansi | rgb]
|ocaml}

let test_write_preserves_leading_bar_polymorphic_variant_types = fun ctx ->
  let source =
    {ocaml|val connect:mode:[ |`Client of string|`Server of string*string]->unit
|ocaml}
  in
  let parsed = parse_mli source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|val connect: mode:[ | `Client of string | `Server of string * string] -> unit
|ocaml}

let test_write_preserves_include_module_type_of_declarations = fun ctx ->
  let source = {ocaml|include  module type of   Global
|ocaml}
  in
  let parsed = parse_mli source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|include module type of Global
|ocaml}

let test_write_renders_external_and_exception_declarations = fun ctx ->
  let source =
    {ocaml|external int64_bits_of_float:float->int64="caml_int64_bits_of_float" "caml_int64_bits_of_float_unboxed"[@@unboxed][@@noalloc]
exception Assumption_failed
exception Property_failed of string
exception Alias=Failure
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|external int64_bits_of_float: float -> int64 =
  "caml_int64_bits_of_float" "caml_int64_bits_of_float_unboxed" [@@ unboxed] [@@ noalloc]

exception Assumption_failed

exception Property_failed of string

exception Alias = Failure
|ocaml}

let test_write_preserves_semantically_meaningful_type_tuple_parentheses = fun ctx ->
  let source =
    {ocaml|type 'value key=int*(unit->'value)
type key_initializer=|KI:'value key*('value->'value)->key_initializer
external recv_from:t->bytes->int->int->((int*(string*int)),int)Result.t="recv_from"
external accept:t->((Tcp_stream.t*(string*int)),int)Result.t="accept"
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|type 'value key = int * (unit -> 'value)

type key_initializer =
  | KI: 'value key * ('value -> 'value) -> key_initializer

external recv_from: t -> bytes -> int -> int -> ((int * (string * int)), int) Result.t = "recv_from"

external accept: t -> ((Tcp_stream.t * (string * int)), int) Result.t = "accept"
|ocaml}

let test_write_preserves_arrow_tuple_payloads_in_signatures = fun ctx ->
  let source =
    {ocaml|type 'payload t
type 'value variant_case=|Unit:string*('value->bool)->'value variant_case|Newtype:string*'payload t*('payload->'value)->'value variant_case
type 'value field=|Field:string*'field t*('value->'field)->'value field
type 'msg attr=|Event of string*(string->'msg)
val event_handlers:'msg attr list->(string*(string->'msg)) list
|ocaml}
  in
  let parsed = parse_mli source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|type 'payload t
type 'value variant_case =
  | Unit: string * ('value -> bool) -> 'value variant_case
  | Newtype: string * 'payload t * ('payload -> 'value) -> 'value variant_case
type 'value field =
  | Field: string * 'field t * ('value -> 'field) -> 'value field
type 'msg attr =
  | Event of string * (string -> 'msg)

val event_handlers: 'msg attr list -> (string * (string -> 'msg)) list
|ocaml}

let test_write_renders_item_attributes_attached_to_declarations = fun ctx ->
  let source =
    {ocaml|type perform={perform:'a 'b.('a step->'b t)->'a Effect.t->'b t}[@@unboxed]
external round_float:float->float="caml_round_float" "caml_round"[@@unboxed][@@noalloc]
|ocaml}
  in
  let parsed = parse_mli source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|type perform = {
  perform: 'a 'b. ('a step -> 'b t) -> 'a Effect.t -> 'b t;
} [@@ unboxed]

external round_float: float -> float = "caml_round_float" "caml_round" [@@ unboxed] [@@ noalloc]
|ocaml}

let test_write_preserves_abstract_type_declaration_attributes = fun ctx ->
  let source = {ocaml|type('a,'b)stack[@@immediate]
type last_fiber[@@immediate]
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|type ('a, 'b) stack [@@ immediate]

type last_fiber [@@ immediate]
|ocaml}

let test_write_preserves_variant_representation_attributes = fun ctx ->
  let source = {ocaml|type 'a t=Ref of int64[@@unboxed]
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|type 'a t =
  | Ref of int64 [@@ unboxed]
|ocaml}

let test_stream_formatter_renders_polymorphic_variant_type_bodies = fun ctx ->
  let source = {ocaml|type color = [ ansi | rgb | xyz ]
|ocaml}
  in
  let actual =
    Krasny.stream_format_to_string (parse_ml source) ~width:100
    |> Result.expect ~msg:"polymorphic variant type body should format"
  in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|type color = [ansi | rgb | xyz]
|ocaml}

let test_stream_formatter_keeps_closed_polymorphic_variant_aliases_idempotent = fun ctx ->
  let source = {ocaml|type cursor_visibility=[`hidden|`visible]
|ocaml}
  in
  let first =
    parse_ml source
    |> Krasny.format
    |> Result.expect ~msg:"closed polymorphic variant alias should format"
  in
  let second =
    parse_ml first
    |> Krasny.format
    |> Result.expect ~msg:"formatted closed polymorphic variant alias should reformat"
  in
  Test.assert_equal ~expected:first ~actual:second;
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual:first
    ~expected:{ocaml|type cursor_visibility = [`hidden | `visible]
|ocaml}

let test_write_matches_format_for_generated_table_records = fun _ctx ->
  let source =
    {ocaml|let _co = {
  r16 = [|{ lo = 0xe000; hi = 0xf8ff; stride = 1 };|];
  r32 = [||];
  latin_offset = 0
}
|ocaml}
  in
  let parsed = parse_ml source in
  let expected =
    Krasny.format parsed
    |> Result.expect ~msg:"format should render generated table record"
  in
  let actual = capture_write parsed in
  Test.assert_equal ~expected ~actual;
  Ok ()

let test_write_matches_format_for_try_expressions = fun _ctx ->
  let source = {ocaml|let value = try read () with | Failure -> 0
|ocaml}
  in
  let parsed = parse_ml source in
  let expected =
    Krasny.format parsed
    |> Result.expect ~msg:"format should render try expressions"
  in
  let actual = capture_write parsed in
  Test.assert_equal ~expected ~actual;
  Ok ()

let test_write_matches_format_for_lazy_exception_and_interval_patterns = fun ctx ->
  let source =
    {ocaml|let force = function | lazy value -> value
let recovered = match read () with | exception Failure -> 0 | value -> value
let classify = function | 'a' .. 'z' -> 1 | _ -> 0
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|let force = fun (lazy value) -> value

let recovered =
  match read () with
  | exception Failure -> 0
  | value -> value

let classify = fun __tmp1 ->
  match __tmp1 with
  | 'a' .. 'z' -> 1
  | _ -> 0
|ocaml}

let test_write_keeps_or_pattern_alternatives_vertical = fun ctx ->
  let source =
    {ocaml|let is_newline = function | '\n' | '\r' -> true | _ -> false
let is_layout = function | ' ' | '\t' | '\n' | '\r' -> true | _ -> false
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|let is_newline = fun __tmp1 ->
  match __tmp1 with
  | '\n'
  | '\r' -> true
  | _ -> false

let is_layout = fun __tmp1 ->
  match __tmp1 with
  | ' '
  | '\t'
  | '\n'
  | '\r' -> true
  | _ -> false
|ocaml}

let test_write_formats_polymorphic_variants = fun ctx ->
  let source = {ocaml|let classify = function | `Alpha -> `Seen | `Beta value -> value
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|let classify = fun __tmp1 ->
  match __tmp1 with
  | `Alpha -> `Seen
  | `Beta value -> value
|ocaml}

let test_format_keeps_explicit_fun_rhs_bindings_explicit = fun _ctx ->
  let source = "let id = fun x -> x\n" in
  let actual =
    parse_ml source
    |> Krasny.format
    |> Result.expect ~msg:"explicit fun rhs bindings should format"
  in
  Test.assert_equal ~expected:source ~actual;
  Ok ()

let test_format_inlines_short_explicit_fun_rhs_with_qualified_apply_bodies = fun ctx ->
  let source = {ocaml|let execv=fun ~program ~args->Kernel.Process.execv program args
|ocaml}
  in
  let actual =
    parse_ml source
    |> Krasny.format
    |> Result.expect ~msg:"qualified multi-argument apply body should format"
  in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|let execv = fun ~program ~args -> Kernel.Process.execv program args
|ocaml}

let test_format_breaks_explicit_fun_rhs_after_arrow_when_body_exceeds_width = fun ctx ->
  let source =
    {ocaml|let f=fun x->very_long_function_call_name another_long_argument_name third_long_argument_name fourth_long_argument_name fifth_long_argument_name
|ocaml}
  in
  let actual =
    parse_ml source
    |> Krasny.format
    |> Result.expect ~msg:"explicit fun RHS body should break after arrow"
  in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|let f = fun x ->
  very_long_function_call_name
    another_long_argument_name
    third_long_argument_name
    fourth_long_argument_name
    fifth_long_argument_name
|ocaml}

let test_format_breaks_function_binding_body_after_equals_when_body_exceeds_width = fun ctx ->
  let source =
    {ocaml|let f x=very_long_function_call_name another_long_argument_name third_long_argument_name fourth_long_argument_name fifth_long_argument_name
|ocaml}
  in
  let actual =
    parse_ml source
    |> Krasny.format
    |> Result.expect ~msg:"function binding body should break after equals"
  in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|let f x =
  very_long_function_call_name
    another_long_argument_name
    third_long_argument_name
    fourth_long_argument_name
    fifth_long_argument_name
|ocaml}

let test_format_keeps_long_list_rhs_opener_after_equals = fun ctx ->
  let source =
    {ocaml|let values=[very_long_item_name;another_long_item_name;third_long_item_name;fourth_long_item_name;fifth_long_item_name]
|ocaml}
  in
  let actual =
    parse_ml source
    |> Krasny.format
    |> Result.expect ~msg:"long list RHS should keep opener after equals"
  in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|let values = [
  very_long_item_name;
  another_long_item_name;
  third_long_item_name;
  fourth_long_item_name;
  fifth_long_item_name;
]
|ocaml}

let test_format_keeps_long_array_rhs_opener_after_equals = fun ctx ->
  let source =
    {ocaml|let values=[|very_long_item_name;another_long_item_name;third_long_item_name;fourth_long_item_name;fifth_long_item_name|]
|ocaml}
  in
  let actual =
    parse_ml source
    |> Krasny.format
    |> Result.expect ~msg:"long array RHS should keep opener after equals"
  in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|let values = [|
  very_long_item_name;
  another_long_item_name;
  third_long_item_name;
  fourth_long_item_name;
  fifth_long_item_name;
|]
|ocaml}

let test_format_keeps_long_record_rhs_opener_after_equals = fun ctx ->
  let source =
    {ocaml|let value={very_long_field_name;another_long_field_name;third_long_field_name;fourth_long_field_name}
|ocaml}
  in
  let actual =
    parse_ml source
    |> Krasny.format
    |> Result.expect ~msg:"long record RHS should keep opener after equals"
  in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|let value = {
  very_long_field_name;
  another_long_field_name;
  third_long_field_name;
  fourth_long_field_name;
}
|ocaml}

let test_format_keeps_multiline_explicit_fun_rhs_with_local_module_bodies = fun _ctx ->
  let source =
    {|let bytes = fun reader ->
  let module ByteIter = struct
    type t = int
  end in
  reader
|}
  in
  let actual =
    parse_ml source
    |> Krasny.format
    |> Result.expect ~msg:"local module bodies should keep explicit fun rhs multiline"
  in
  Test.assert_equal ~expected:source ~actual;
  Ok ()

let test_format_keeps_structural_let_module_bodies_with_nested_lets = fun _ctx ->
  let source =
    {|let bytes = fun reader ->
  let module ByteIter = struct
    let next = fun state ->
      let scratch = state in
      scratch
  end in
  reader
|}
  in
  let actual =
    parse_ml source
    |> Krasny.format
    |> Result.expect ~msg:"nested local-module lets should stay inside the struct body"
  in
  Test.assert_equal ~expected:source ~actual;
  Ok ()

let test_format_renders_fun_body_trivia_from_token_leading_trivia = fun ctx ->
  let source =
    {|let with_comment = fun x -> (* keep *) x
let with_doc = fun x -> (** keep *) x
|}
  in
  assert_formatted_ml_snapshot
    ~ctx
    ~msg:"fun-body comment and docstring trivia should not need source reparsing"
    source

let test_format_renders_if_branch_trivia_from_token_leading_trivia = fun _ctx ->
  let source =
    {|let classify = fun flag -> if flag then value (* keep before else *) else other
let nested = fun flag other -> if flag then value else (* keep before branch *) if other then (* nested *) next else last
|}
  in
  let actual =
    parse_ml source
    |> Krasny.format
    |> Result.expect ~msg:"if/else comment trivia should not need source reparsing"
  in
  Test.assert_equal
    ~expected:{|let classify = fun flag ->
  if flag then
    value
    (* keep before else *)
  else
    other

let nested = fun flag other ->
  if flag then
    value
  else
    (* keep before branch *)
    if other then
      (* nested *)
      next
    else
      last
|}
    ~actual;
  Ok ()

let test_format_renders_let_rhs_and_body_trivia_from_token_leading_trivia = fun ctx ->
  let source =
    {|let run =
  let value = (* keep before rhs *) compute in
  (* keep before body *)
  use value
|}
  in
  assert_formatted_ml_snapshot
    ~ctx
    ~msg:"let rhs/body trivia should not need source reparsing"
    source

let test_format_renders_sequence_and_let_operator_trivia_from_tokens = fun ctx ->
  let source =
    {|let run = fun () -> first (* keep after first *); (* keep before second *) second; (** keep before third *) third
let bind =
  let* value = (* keep before bound value *) compute in
  (* keep before body *)
  finish value
|}
  in
  assert_formatted_ml_snapshot
    ~ctx
    ~msg:"sequence and binding-operator trivia should not need source reparsing"
    source

let test_format_match_cases_from_structure_not_arrow_source_newlines = fun ctx ->
  let source = {|let render = function
  | A ->
      value
|}
  in
  assert_formatted_ml_snapshot
    ~ctx
    ~msg:"match case layout should not preserve source newlines after arrows"
    source

let test_format_preserves_leading_comments_on_match_cases = fun ctx ->
  let source =
    {ocaml|let infer_record=fun state record->match record with
(* if you have `{}` we'll just fallback to a hole *)
|{update=None;fields=[]}->State.fresh_var state
(* if you have `{hello}` we'll fallback to hello's type assuming its a record update *)
|{update=Some update;fields=[]}->infer_expression state update
(* if you have `{hello=1}` we'll type the record body *)
|{update=None;fields}->infer_record_body state fields
(* if you have `{hello with x=1}` we'll type the record body and unify against the updated value *)
|{update=Some update;fields}->infer_record_update state update fields
|ocaml}
  in
  let actual =
    parse_ml source
    |> Krasny.format
    |> Result.expect ~msg:"match case leading comments should format"
  in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|let infer_record = fun state record ->
  match record with
  (* if you have `{}` we'll just fallback to a hole *)
  | { update = None; fields = [] } -> State.fresh_var state

  (* if you have `{hello}` we'll fallback to hello's type assuming its a record update *)
  | { update = Some update; fields = [] } -> infer_expression state update

  (* if you have `{hello=1}` we'll type the record body *)
  | { update = None; fields } -> infer_record_body state fields

  (* if you have `{hello with x=1}` we'll type the record body and unify against the updated value *)
  | { update = Some update; fields } -> infer_record_update state update fields
|ocaml}

let test_write_breaks_nested_constructor_match_patterns = fun ctx ->
  let source =
    {ocaml|let test=match Cookie.parse_set_cookie_result "session=abc\r\nSet-Cookie: evil=1; Path=/" with|Error(Cookie.InvalidCookie(Cookie.InvalidValueCharacter{index=3;character='\r';reason=Cookie.ControlCharacter}))->Result.Ok()
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|let test =
  match Cookie.parse_set_cookie_result "session=abc\r\nSet-Cookie: evil=1; Path=/" with
  | Error (
    Cookie.InvalidCookie (
      Cookie.InvalidValueCharacter { index = 3; character = '\r'; reason = Cookie.ControlCharacter }
    )
  ) ->
      Result.Ok ()
|ocaml}

let test_write_breaks_after_multiline_constructor_record_match_patterns = fun ctx ->
  let source =
    {ocaml|let test=match parse value with|Error(Cookie.InvalidAttributeCharacter{attribute=Cookie.Path;index=4;character=';';reason=Cookie.AttributeSemicolon})->Result.Ok()
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|let test =
  match parse value with
  | Error (
    Cookie.InvalidAttributeCharacter {
      attribute = Cookie.Path;
      index = 4;
      character = ';';
      reason = Cookie.AttributeSemicolon;
    }
  ) ->
      Result.Ok ()
|ocaml}

let test_write_breaks_deep_record_list_patterns = fun ctx ->
  let source =
    {ocaml|let test ast=match ast.kind with|Implementation[{kind=Let{bindings=[{expr={kind=Constructor{ident;payload=Some{kind=Literal Int;_}};_};_}];_};_}]->assert_path_string ~expected:"Some" ident|_->()
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|let test ast =
  match ast.kind with
  | Implementation [
      {
        kind =
          Let {
            bindings = [
                {
                  expr = {
                    kind = Constructor { ident; payload = Some { kind = Literal Int; _ } };
                    _;
                  };
                  _;
                };
            ];
            _;
          };
        _;
      };
    ] -> assert_path_string ~expected:"Some" ident
  | _ -> ()
|ocaml}

let test_write_keeps_small_record_list_patterns_inline = fun ctx ->
  let source = {ocaml|let test=function|Ok[{Hpack.name="x";value="y"}]->Ok()|_->Error()
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|let test = fun __tmp1 ->
  match __tmp1 with
  | Ok [ { Hpack.name = "x"; value = "y" } ] -> Ok ()
  | _ -> Error ()
|ocaml}

let test_write_aligns_multiline_list_pattern_closing_delimiter = fun ctx ->
  let source =
    {ocaml|let test=function|[({Render.width={left=1;right=1;top=1;bottom=1};color=`rgb(50,50,50);_},_,_)]->Ok()|_->Error()
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|let test = fun __tmp1 ->
  match __tmp1 with
  | [
      (
        {
          Render.width = {
            left = 1;
            right = 1;
            top = 1;
            bottom = 1;
          };
          color = `rgb (50, 50, 50);
          _;
        },
        _,
        _
      );
    ] -> Ok ()
  | _ -> Error ()
|ocaml}

let test_write_aligns_constructor_record_pattern_payloads = fun ctx ->
  let source =
    {ocaml|let message=function|Io{op;path;related_path=None;detail}->op^" failed for "^Path.to_string path^": "^io_detail_message detail|Io{op;path;related_path=Some related_path;detail}->op
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|let message = fun __tmp1 ->
  match __tmp1 with
  | Io {
      op;
      path;
      related_path = None;
      detail;
    } ->
      op ^ " failed for " ^ Path.to_string path ^ ": " ^ io_detail_message detail
  | Io {
      op;
      path;
      related_path = Some related_path;
      detail;
    } ->
      op
|ocaml}

let test_write_keeps_fitting_constructor_or_patterns_inline = fun ctx ->
  let source =
    {ocaml|let test=function|Some(Leading_ordinary_comment|Leading_docstring)->state.suppress_leading_token<-Some token.Ast.id
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|let test = fun (Some (Leading_ordinary_comment | Leading_docstring)) ->
  state.suppress_leading_token <- Some token.Ast.id
|ocaml}

let test_write_keeps_fitting_or_pattern_fun_parameters_inline = fun ctx ->
  let source =
    {ocaml|let close_all=fun pool->List.for_each pool ~fn:(fun(Available conn|InUse(conn,_,_))->Connection.close conn)
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|let close_all = fun pool ->
  List.for_each
    pool
    ~fn:(fun (Available conn | InUse (conn, _, _)) -> Connection.close conn)
|ocaml}

let test_write_keeps_parenthesized_match_case_bodies_attached_to_arrows = fun ctx ->
  let source =
    {ocaml|let get=fun value->match value with|Some x->(match x with|Some y->y|None->0)|None->0
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|let get = fun value ->
  match value with
  | Some x -> (
      match x with
      | Some y -> y
      | None -> 0
    )
  | None -> 0
|ocaml}

let test_write_parenthesizes_tuple_match_case_bodies = fun ctx ->
  let source = {ocaml|let next=fun x->match x with|Some y->(Some y,x)|None->(None,x)
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  let reparsed = parse_ml actual in
  Test.assert_equal ~expected:(Krasny.syntax_hash parsed) ~actual:(Krasny.syntax_hash reparsed);
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|let next = fun x ->
  match x with
  | Some y -> (Some y, x)
  | None -> (None, x)
|ocaml}

let test_write_keeps_commented_tuple_match_case_bodies_safe = fun ctx ->
  let source =
    {ocaml|let update event model=match event with|`Clear->(* Clear selection *)({model with selected_user=None},Command.Noop)|_->(model,Command.Noop)
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  let reparsed = parse_ml actual in
  Test.assert_equal ~expected:(Krasny.syntax_hash parsed) ~actual:(Krasny.syntax_hash reparsed);
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|let update event model =
  match event with
  | `Clear ->
      (* Clear selection *)
      ({ model with selected_user = None }, Command.Noop)
  | _ -> (model, Command.Noop)
|ocaml}

let test_write_parenthesizes_tuple_cases_inside_parenthesized_matches = fun ctx ->
  let source = {ocaml|let x=match a with|A->(match b with|C->(d,e)|F->g)|B->(h,i)
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  let reparsed = parse_ml actual in
  Test.assert_equal ~expected:(Krasny.syntax_hash parsed) ~actual:(Krasny.syntax_hash reparsed);
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|let x =
  match a with
  | A -> (
      match b with
      | C -> (d, e)
      | F -> g
    )
  | B -> (h, i)
|ocaml}

let test_write_parenthesizes_tuple_cases_inside_delimited_let_matches = fun ctx ->
  let source =
    {ocaml|let f=fun control->(let raw=raw_of_control control in match control with|Csi body->(match parse body with|Some event->(state,[event])|None->(state,[`Unknown raw]))|Escape->(state,[]))
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  let reparsed = parse_ml actual in
  Test.assert_equal ~expected:(Krasny.syntax_hash parsed) ~actual:(Krasny.syntax_hash reparsed);
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|let f = fun control ->
  (
    let raw = raw_of_control control in
    match control with
    | Csi body -> (
        match parse body with
        | Some event -> (state, [ event ])
        | None -> (state, [
          `Unknown raw;
        ])
      )
    | Escape -> (state, [])
  )
|ocaml}

let test_write_preserves_nested_tuple_expression_values = fun ctx ->
  let source = {ocaml|let value=((a,b),c)
let record={microseconds=(micros,6);other=x}
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|let value = ((a, b), c)

let record = {
  microseconds = (micros, 6);
  other = x;
}
|ocaml}

let test_write_parenthesizes_tuple_scrutinees_and_tuple_patterns = fun ctx ->
  let source = {ocaml|let triple=match a,b,c with|Some x,Some y,Some z->x|_->fallback
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|let triple =
  match (a, b, c) with
  | (Some x, Some y, Some z) -> x
  | _ -> fallback
|ocaml}

let test_write_breaks_tuple_patterns_before_match_arrows = fun ctx ->
  let source =
    {ocaml|let test=fun result_a result_b->match(result_a,result_b)with|(Some{status=Action_scheduler.Executed;_},Some{status=Action_scheduler.Executed;_})->Ok()|_->Error()
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|let test = fun result_a result_b ->
  match (result_a, result_b) with
  | (
      Some { status = Action_scheduler.Executed; _ },
      Some { status = Action_scheduler.Executed; _ }
    ) -> Ok ()
  | _ -> Error ()
|ocaml}

let test_write_preserves_tuple_pattern_application_payloads = fun ctx ->
  let source = {ocaml|let value=fun x->match x with|Some(a,b)->a|None->b
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|let value = fun x ->
  match x with
  | Some (a, b) -> a
  | None -> b
|ocaml}

let test_write_preserves_comments_before_local_and_bindings = fun ctx ->
  let source =
    {ocaml|let value=let rec first=fun()->0
(* keep before second *)
and second=fun()->1 in first()+second()
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|let value =
  let rec first = fun () -> 0
  (* keep before second *)
  and second = fun () -> 1
  in
  first () + second ()
|ocaml}

let test_format_polymorphic_variant_heads_from_explicit_tag_tokens = fun ctx ->
  let source =
    {|let classify = function
  | `Ok value -> value
  | `Error -> fallback

let value = `Ok 1
|}
  in
  assert_formatted_ml_snapshot
    ~ctx
    ~msg:"polymorphic variant heads should format from tag tokens"
    source

let test_format_quoted_core_type_variables_from_explicit_sigil_tokens = fun ctx ->
  let source = {|type 'a t = 'a list

val id : 'a -> 'a
|}
  in
  assert_formatted_mli_snapshot
    ~ctx
    ~msg:"quoted core type variables should format from sigil and name tokens"
    source

let test_format_core_type_alias_binders_from_explicit_sigil_tokens = fun _ctx ->
  let source = {|val cast : ('a list as 'whole) -> 'whole
|}
  in
  let actual =
    parse_mli source
    |> Krasny.format
    |> Result.expect ~msg:"core type alias binders should format from explicit sigil tokens"
  in
  Test.assert_equal ~expected:{|val cast: ('a list as 'whole) -> 'whole
|} ~actual;
  Ok ()

let test_format_record_fields_break_for_compound_field_types_not_field_name_length = fun ctx ->
  let source =
    {|type t = {
  this_is_a_pretty_long_record_field_name : int list;
}

type u = {
  mutable this_is_a_pretty_long_record_field_name : int list;
}
|}
  in
  let actual =
    parse_ml source
    |> Krasny.format
    |> Result.expect
      ~msg:"record fields should not break after ':' just because the field name is long"
  in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{|type t = {
  this_is_a_pretty_long_record_field_name: int list;
}

type u = {
  mutable this_is_a_pretty_long_record_field_name: int list;
}
|}

let test_format_record_fields_preserves_field_attributes = fun _ctx ->
  let source = {|type 'value t = {
  mutable contents : 'value [@atomic];
}
|}
  in
  let actual =
    parse_ml source
    |> Krasny.format
    |> Result.expect ~msg:"record field attributes should render from the CST field attributes"
  in
  Test.assert_equal ~expected:{|type 'value t = { mutable contents: 'value [@atomic] }
|} ~actual;
  assert_idempotent
    ~source:actual
    ~msg:"record field attributes should remain stable across repeated formatting";
  Ok ()

let test_format_keeps_prefix_minus_separated_from_nested_prefix_expressions = fun _ctx ->
  let source = {|let value = - !acc
|}
  in
  let actual =
    parse_ml source
    |> Krasny.format
    |> Result.expect ~msg:"prefix minus should not merge with nested prefix operators"
  in
  Test.assert_equal ~expected:source ~actual;
  assert_idempotent
    ~source
    ~msg:"prefix minus with nested prefix operators should stay stable across repeated formatting";
  Ok ()

let test_write_preserves_nested_prefix_operator_tokens = fun ctx ->
  let source = {ocaml|let value=- !acc
let flipped=not !flag
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  let reparsed = parse_ml actual in
  Test.assert_equal ~expected:(Krasny.syntax_hash parsed) ~actual:(Krasny.syntax_hash reparsed);
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|let value = - !acc

let flipped = not !flag
|ocaml}

let test_format_keeps_curried_nullary_constructor_fun_parameters_separate = fun _ctx ->
  let source =
    {|let cast_worker:
  type task other. (task, other) Type.eq ->
  other WorkerPool.DynamicWorkerPool.worker ->
  task WorkerPool.DynamicWorkerPool.worker = fun Type.Equal worker -> worker
|}
  in
  let actual =
    parse_ml source
    |> Krasny.format
    |> Result.expect
      ~msg:"curried nullary constructor fun parameters should not collapse into one constructor pattern"
  in
  Test.assert_equal ~expected:source ~actual;
  assert_idempotent
    ~source
    ~msg:"curried nullary constructor fun parameters should stay stable across repeated formatting";
  Ok ()

let test_write_preserves_nullary_constructor_patterns = fun ctx ->
  let source =
    {ocaml|let width=function|EastAsianWide->2|EastAsianNarrow->1
let describe=fun Type.Equal->"equal"
let render=fun value->match value with|Ok->"ok"|Error->"error"
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|let width = fun __tmp1 ->
  match __tmp1 with
  | EastAsianWide -> 2
  | EastAsianNarrow -> 1

let describe = fun Type.Equal -> "equal"

let render = fun value ->
  match value with
  | Ok -> "ok"
  | Error -> "error"
|ocaml}

let test_write_preserves_nested_constructor_patterns = fun ctx ->
  let source =
    {ocaml|let publish=fun result->match result with|Error (Riot_deps.Publisher.RegistryPublishFailed {locator;error})->locator^error|Ok value->value
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|let publish = fun result ->
  match result with
  | Error (Riot_deps.Publisher.RegistryPublishFailed { locator; error }) -> locator ^ error
  | Ok value -> value
|ocaml}

let test_write_preserves_defaulted_optional_parameters_after_renamed_labels = fun ctx ->
  let source = {ocaml|let truncate_width=fun ~width:target_width ?(tail="x") s->tail
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|let truncate_width = fun ~width:target_width ?(tail = "x") s -> tail
|ocaml}

let test_write_preserves_labeled_parameters_after_renamed_label = fun ctx ->
  let source = {ocaml|let found_text=fun ~found:token ~text ~span->text
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|let found_text = fun ~found:token ~text ~span -> text
|ocaml}

let test_write_preserves_return_annotated_fun_parameters = fun ctx ->
  let source =
    {ocaml|let make_release_source=fun ?(files=[]) ~package_name ~version manifest_toml:Pkgs_ml.Registry.release_source->manifest_toml
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|let make_release_source = fun
  ?(files = []) ~package_name ~version manifest_toml: Pkgs_ml.Registry.release_source ->
  manifest_toml
|ocaml}

let test_desugar_typed_named_parameters_without_duplicating_inner_annotations = fun ctx ->
  let source =
    {|type 'a t = 'a list

let map (type a b) (iter : a t) ~(fn : a -> b) : b t = failwith "todo"
|}
  in
  assert_formatted_ml_snapshot
    ~ctx
    ~msg:"typed named parameters should move to the synthesized outer annotation"
    source

let test_keep_typed_parameters_in_the_binding_header_when_annotation_synthesis_declines = fun
  _ctx ->
  let source = {|let pick x : int = x
|}
  in
  let actual =
    parse_ml source
    |> Krasny.format
    |> Result.expect
      ~msg:"typed parameters should stay in the binding header when outer annotation synthesis does not apply"
  in
  Test.assert_equal ~expected:{|let pick x: int = x
|} ~actual;
  Ok ()

let test_keep_binding_return_type_annotations_loose_after_named_parameters = fun _ctx ->
  let source = {|type color

let make ~start ~finish ~steps : color array =
  steps
|}
  in
  let actual =
    parse_ml source
    |> Krasny.format
    |> Result.expect ~msg:"binding return-type annotations after named parameters should stay loose"
  in
  Test.assert_equal
    ~expected:{|type color

let make ~start ~finish ~steps : color array = steps
|}
    ~actual;
  Ok ()

let test_write_keeps_binding_return_type_annotations_loose_after_named_parameters = fun ctx ->
  let source = {ocaml|let symmetric ~h ~v : margin={left=h;right=h;top=v;bottom=v}
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|let symmetric ~h ~v : margin = {
  left = h;
  right = h;
  top = v;
  bottom = v;
}
|ocaml}

let test_format_index_expressions_from_explicit_delimiter_tokens = fun ctx ->
  let source = {|let x = s.[0]
let y = a.(0)
let z = x.%(0)
|}
  in
  assert_formatted_ml_snapshot
    ~ctx
    ~msg:"index expressions should format from CST-carried delimiters, not token replay"
    source

let test_write_keeps_index_expressions_bare_as_application_arguments = fun ctx ->
  let source = {ocaml|let a content pos=Some content.[!pos]
let b arr=Ok arr.(0)
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  let reparsed = parse_ml actual in
  Test.assert_equal ~expected:(Krasny.syntax_hash parsed) ~actual:(Krasny.syntax_hash reparsed);
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|let a content pos = Some content.[!pos]

let b arr = Ok arr.(0)
|ocaml}

let test_format_signed_literal_patterns_from_structural_sign_tokens = fun ctx ->
  let source = {|let classify = function | -1 -> `Neg | +2 -> `Pos | _ -> `Other
|}
  in
  assert_formatted_ml_snapshot
    ~ctx
    ~msg:"signed literal patterns should format from CST-carried sign tokens"
    source

let test_write_preserves_signed_literal_constructor_payload_patterns = fun ctx ->
  let source =
    {ocaml|let parse=function|Ok (Json.Int -123)->Ok ()|Ok (Json.Float -1.5)->Ok ()|_->Error "no"
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|let parse = fun __tmp1 ->
  match __tmp1 with
  | Ok (Json.Int -123) -> Ok ()
  | Ok (Json.Float -1.5) -> Ok ()
  | _ -> Error "no"
|ocaml}

let test_write_keeps_record_field_wildcard_values_closed = fun ctx ->
  let source = {ocaml|let show=function|Error {exception_=_;backtrace}->backtrace|_->""
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|let show = fun __tmp1 ->
  match __tmp1 with
  | Error { exception_ = _; backtrace } -> backtrace
  | _ -> ""
|ocaml}

let test_format_leaves_a_blank_line_before_docstring_led_top_level_items = fun _ctx ->
  let source = {|let first = 1
(** doc for second *)
let second = 2
|}
  in
  let actual =
    parse_ml source
    |> Krasny.format
    |> Result.expect ~msg:"top-level docstring-led items should stay visually separated"
  in
  Test.assert_equal ~expected:{|let first = 1

(** doc for second *)
let second = 2
|} ~actual;
  Ok ()

let test_format_leaves_a_blank_line_before_docstring_led_signature_items = fun ctx ->
  let source = {|val first : int
(** doc for second *)
val second : int
|}
  in
  assert_formatted_mli_snapshot
    ~ctx
    ~msg:"signature docstring-led items should stay visually separated"
    source

let test_format_operator_expressions_and_patterns_from_explicit_operator_tokens = fun ctx ->
  let source = {|let op = ( + )
let is_plus = function | ( + ) -> true | _ -> false
|}
  in
  assert_formatted_ml_snapshot
    ~ctx
    ~msg:"operator expressions and patterns should format from CST-carried operator tokens"
    source

let test_format_infix_and_prefix_expression_operators_from_explicit_operator_tokens = fun _ctx ->
  let source =
    {|let negate value = ~-value
let ready = flag01 && flag02 && flag03 && flag04 && flag05 && flag06 && flag07 && flag08 && flag09
|}
  in
  let actual =
    parse_ml source
    |> Krasny.format
    |> Result.expect
      ~msg:"infix and prefix expressions should format from CST-carried operator tokens"
  in
  Test.assert_equal
    ~expected:{|let negate value = ~-value

let ready =
  flag01
  && flag02
  && flag03
  && flag04
  && flag05
  && flag06
  && flag07
  && flag08
  && flag09
|}
    ~actual;
  Ok ()

let test_format_singleton_list_patterns_with_explicit_formatter_spacing = fun ctx ->
  let compact_source = {|let classify = function
  | [value] -> hit
|}
  in
  assert_formatted_ml_snapshot
    ~ctx
    ~msg:"singleton list patterns should not preserve compact source spacing"
    compact_source

let test_format_if_conditions_from_infix_structure_not_token_scans = fun _ctx ->
  let source = {|let decide =
  if a&&b
     || c
  then hit else miss
|}
  in
  let actual =
    parse_ml source
    |> Krasny.format
    |> Result.expect ~msg:"if conditions should format from infix expression structure"
  in
  Test.assert_equal
    ~expected:{|let decide =
  if a && b || c then
    hit
  else
    miss
|}
    ~actual;
  Ok ()

let test_format_binding_values_from_structure_not_source_newlines = fun ctx ->
  let source = {|let wrapped =
  (
    value
  )
|}
  in
  assert_formatted_ml_snapshot
    ~ctx
    ~msg:"binding layout should not preserve multiline source for a simple wrapped value"
    source

let test_format_simple_string_bindings_inline_from_ordinary_simplicity_checks = fun ctx ->
  let source =
    {|let message =
  (
    "ok"
  )
let bind =
  let* value = "ok" in
  finish value
|}
  in
  assert_formatted_ml_snapshot ~ctx ~msg:"binding operators should always break after in" source

let test_format_keeps_simple_applies_inline_even_when_identifiers_contain_keywords = fun _ctx ->
  let source = "let handler = use function_handler\n" in
  let actual =
    parse_ml source
    |> Krasny.format
    |> Result.expect ~msg:"simple applies should not sniff keyword substrings"
  in
  Test.assert_equal ~expected:source ~actual;
  Ok ()

let test_format_normalizes_simple_applies_from_structure_not_source_newlines = fun _ctx ->
  let source = {|let call =
  run
    first
    second
|}
  in
  let actual =
    parse_ml source
    |> Krasny.format
    |> Result.expect ~msg:"simple applies should not preserve source newlines"
  in
  Test.assert_equal ~expected:{|let call = run first second
|} ~actual;
  Ok ()

let test_format_rewrites_parameterized_let_bindings_between_formatted_lets = fun ctx ->
  let source = "(* intro *)\nlet x = 1 + 2\nlet f x = x + 1\nlet y = 3 + 4\n" in
  assert_formatted_ml_snapshot
    ~ctx
    ~msg:"parameterized let bindings should render through explicit fun syntax"
    source

let test_format_keeps_mixed_trivia_and_unsupported_items_parseable = fun _ctx ->
  let source = {|open Std
type t =
  | A
  | B
(* keep with x *)
let x = 1 + 2
let y = 3 + 4
|}
  in
  assert_idempotent ~source ~msg:"mixed implementation files should format";
  Ok ()

let test_format_keeps_tuple_list_array_docs_idempotent = fun _ctx ->
  let source =
    {|let tuple_value = (left_side_identifier, right_side_identifier, final_identifier)
let list_value = [first_item_identifier; second_item_identifier; third_item_identifier]
let array_value = [|first_item_identifier; second_item_identifier; third_item_identifier|]
|}
  in
  assert_idempotent ~source ~msg:"collection expressions should stay stable";
  Ok ()

let test_write_always_parenthesizes_tuple_expressions_and_patterns = fun ctx ->
  let source =
    {ocaml|let pair=a,b
let nested=a,(b,c)
let opened=M.(a,b)
let destructure=fun pair->let a,b=pair in a,b
let consume=function|[]->Doc.empty,column|M.(x,y)->x,y
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|let pair = (a, b)

let nested = (a, (b, c))

let opened = M.(a, b)

let destructure = fun pair ->
  let (a, b) = pair in
  (a, b)

let consume = fun __tmp1 ->
  match __tmp1 with
  | [] -> (Doc.empty, column)
  | M.(x, y) -> (x, y)
|ocaml}

let test_write_keeps_small_array_expressions_inline = fun ctx ->
  let source = {ocaml|let array=[|1;2|]
|ocaml}
  in
  let actual =
    parse_ml source
    |> Krasny.format
    |> Result.expect ~msg:"small array should format"
  in
  Test.Snapshot.assert_inline_text ~ctx ~actual ~expected:{ocaml|let array = [|1; 2|]
|ocaml}

let test_format_canonicalizes_multiline_list_apply_arguments = fun ctx ->
  let source = {|let cmd =
  f [
    first_item;
    second_item;
  ]
|}
  in
  assert_formatted_ml_snapshot ~ctx ~msg:"list arguments should format" source

let test_write_breaks_let_rhs_before_calls_with_multiline_list_arguments = fun ctx ->
  let source =
    {ocaml|let ui=Element.row[Element.container~style:(Style.empty|>Style.width(Style.Fixed 20.0))[Element.text"Left"];Element.container~style:(Style.empty|>Style.width Style.Grow)[Element.text"Middle"];Element.container~style:(Style.empty|>Style.width(Style.Fixed 15.0))[Element.text"Right"]]
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|let ui =
  Element.row
    [
      Element.container
        ~style:(
          Style.empty
          |> Style.width (Style.Fixed 20.0)
        )
        [ Element.text "Left" ];
      Element.container
        ~style:(
          Style.empty
          |> Style.width Style.Grow
        )
        [ Element.text "Middle" ];
      Element.container
        ~style:(
          Style.empty
          |> Style.width (Style.Fixed 15.0)
        )
        [ Element.text "Right" ];
    ]
|ocaml}

let test_format_normalizes_let_open_bodies_from_structure_not_source_newlines = fun ctx ->
  let source = {|let answer =
  let open Option in
  value
|}
  in
  assert_formatted_ml_snapshot ~ctx ~msg:"let-open expressions should format structurally" source

let test_format_aligns_multiline_let_open_bodies_with_the_let_open_expression = fun ctx ->
  let source = {ocaml|let foo()=let open WellKnownTypes in match x with|true->true
|ocaml}
  in
  let actual =
    parse_ml source
    |> Krasny.format
    |> Result.expect ~msg:"multiline let-open bodies should align"
  in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|let foo () =
  let open WellKnownTypes in
  match x with
  | true -> true
|ocaml}

let test_format_open_bang_from_explicit_bang_tokens_in_ml_and_mli = fun _ctx ->
  let source = "open! Inline\n" in
  let actual_ml =
    parse_ml source
    |> Krasny.format
    |> Result.expect ~msg:"implementation open! should render from bang_token"
  in
  let actual_mli =
    parse_mli source
    |> Krasny.format
    |> Result.expect ~msg:"signature open! should render from bang_token"
  in
  Test.assert_equal ~expected:source ~actual:actual_ml;
  Test.assert_equal ~expected:source ~actual:actual_mli;
  Ok ()

let test_format_local_binding_equals_policy_for_boolean_chains_and_pipelines = fun ctx ->
  let source =
    {|let run flag01 flag02 flag03 flag04 flag05 flag06 flag07 flag08 flag09 value =
  let ready = flag01 && flag02 && flag03 && flag04 && flag05 && flag06 && flag07 && flag08 && flag09 in
  let staged = value |> stage01 |> stage02 |> stage03 |> stage04 |> stage05 |> stage06 in
  ready, staged
|}
  in
  assert_formatted_ml_snapshot
    ~ctx
    ~msg:"local binding equals policy should stay stable while heuristics are isolated"
    source

let test_format_breaks_long_pipeline_rhs_after_equals = fun ctx ->
  let source =
    {ocaml|let test_unify_same_constructor _ctx=Infer.unify ~expected:int_type ~actual:int_type|>Result.map_err ~fn:Typ.Check.Error.to_string
let test_unify_constructor_mismatch _ctx=Infer.unify ~expected:int_type ~actual:bool_type|>assert_type_mismatch
let pipeline=f|>g|>h
|ocaml}
  in
  let actual =
    parse_ml source
    |> Krasny.format
    |> Result.expect ~msg:"long pipeline rhs should break after equals"
  in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|let test_unify_same_constructor _ctx =
  Infer.unify ~expected:int_type ~actual:int_type
  |> Result.map_err ~fn:Typ.Check.Error.to_string

let test_unify_constructor_mismatch _ctx =
  Infer.unify ~expected:int_type ~actual:bool_type
  |> assert_type_mismatch

let pipeline =
  f
  |> g
  |> h
|ocaml}

let test_format_breaks_local_let_rhs_when_trailing_in_would_exceed_width = fun ctx ->
  let source =
    {ocaml|let add_value t ~name ~scheme=let scopes=map_current t.scopes ~fn:(fun scope->IdentMap.insert scope ~key:name ~value:scheme) in {scopes}
|ocaml}
  in
  let actual =
    parse_ml source
    |> Krasny.format
    |> Result.expect ~msg:"local let rhs plus in suffix should respect formatter width"
  in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|let add_value t ~name ~scheme =
  let scopes =
    map_current t.scopes ~fn:(fun scope -> IdentMap.insert scope ~key:name ~value:scheme)
  in
  { scopes }
|ocaml}

let test_format_breaks_let_rhs_after_equals_then_retries_constructor_record_payload = fun ctx ->
  let source =
    {ocaml|let type_declaration_result (decl:type_declaration) arguments=Type.Constructor{ident=decl.name;arguments}
|ocaml}
  in
  let actual =
    parse_ml source
    |> Krasny.format
    |> Result.expect ~msg:"long constructor record payload RHS should retry after equals"
  in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|let type_declaration_result (decl: type_declaration) arguments =
  Type.Constructor { ident = decl.name; arguments }
|ocaml}

let test_format_parenthesized_pipeline_arguments_vertically = fun ctx ->
  let source =
    {ocaml|let styled=Element.container ~style:(Style.empty|>Style.width Style.Grow|>Style.height (Style.Fixed 20.0)) [Element.text "Middle"]
let nested=concat(first::(rest|>List.map ~fn:render|>List.flatten))
|ocaml}
  in
  let actual =
    parse_ml source
    |> Krasny.format
    |> Result.expect ~msg:"parenthesized pipeline arguments should indent vertically"
  in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|let styled =
  Element.container
    ~style:(
      Style.empty
      |> Style.width Style.Grow
      |> Style.height (Style.Fixed 20.0)
    )
    [ Element.text "Middle" ]

let nested =
  concat
    (
      first :: (
        rest
        |> List.map ~fn:render
        |> List.flatten
      )
    )
|ocaml}

let test_format_local_binding_infix_threshold_around_inline_after_equals_cutoff = fun ctx ->
  let source =
    {|let totals a b c d e f g h i =
  let total8 = a + b + c + d + e + f + g + h in
  let total9 = a + b + c + d + e + f + g + h + i in
  total8, total9
|}
  in
  assert_formatted_ml_snapshot
    ~ctx
    ~msg:"local binding infix threshold should stay explicit and stable"
    source

let test_format_simple_apply_rhs_by_shape_not_comment_scans = fun ctx ->
  let source = {ocaml|let run x=let value=f(* keep *)x in value
|ocaml}
  in
  let actual =
    parse_ml source
    |> Krasny.format
    |> Result.expect ~msg:"simple apply rhs layout should not depend on scanning raw token trivia"
  in
  match Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|let run x =
  let value = f
    (* keep *)
    x
  in
  value
|ocaml} with
  | Ok () ->
      assert_idempotent ~source ~msg:"comment-bearing simple apply rhs should stay stable";
      Ok ()
  | Error _ as error -> error

let test_format_binding_operator_equals_policy_with_explicit_fun_and_multiline_values = fun ctx ->
  let source =
    {|let bind flag01 flag02 flag03 flag04 flag05 flag06 flag07 flag08 flag09 value =
  let* callback = fun x -> x in
  let* ready = flag01 && flag02 && flag03 && flag04 && flag05 && flag06 && flag07 && flag08 && flag09 in
  let+ staged = value |> stage01 |> stage02 |> stage03 |> stage04 |> stage05 |> stage06 in
  callback staged, ready
|}
  in
  assert_formatted_ml_snapshot
    ~ctx
    ~msg:"binding-operator equals policy should stay aligned with local bindings"
    source

let test_format_recursive_local_bindings_with_multiline_bodies = fun _ctx ->
  let source =
    {|let outer value =
  let rec loop n = if n = 0 then value else loop (n - 1) in
  loop 3
|}
  in
  let actual =
    parse_ml source
    |> Krasny.format
    |> Result.expect ~msg:"recursive local bindings should keep multiline bodies explicit"
  in
  Test.assert_equal
    ~expected:{|let outer value =
  let rec loop n =
    if n = 0 then
      value
    else
      loop (n - 1)
  in
  loop 3
|}
    ~actual;
  Ok ()

let test_format_breaks_tuples_that_exceed_formatter_width = fun ctx ->
  let source =
    {|let tuple_value = (left_side_identifier, right_side_identifier, final_identifier, fourth_identifier, fifth_identifier, sixth_identifier)
|}
  in
  assert_formatted_ml_snapshot ~ctx ~msg:"tuples should break from solver width" source

let test_verify_treats_stream_formatter_rewrites_as_safe = fun _ctx ->
  with_tempdir
    "krasny_runner_verify_semantic_hash"
    (fun tmpdir ->
      let parens = Path.(tmpdir / Path.v "parens.ml") in
      let listy = Path.(tmpdir / Path.v "listy.ml") in
      let recordy = Path.(tmpdir / Path.v "recordy.ml") in
      Fs.write "let x = configure    ~style:(Style.Grow)\n" parens
      |> Result.expect ~msg:"write parens";
      Fs.write {|let cmd =
  f [
    first_item;
    second_item;
  ]
|} listy
      |> Result.expect ~msg:"write listy";
      Fs.write
        {record_fixture|let explanation =
  Api.Explanation.
    {
      rule_id = package_rule_id;
      message = "Use != instead of <> for inequality.";
      body = {|body|};
    }
|record_fixture}
        recordy
      |> Result.expect ~msg:"write recordy";
      let result = Krasny.Runner.run_verify [ parens; listy; recordy ] in
      Test.assert_equal ~expected:3 ~actual:result.summary.total_files;
      Test.assert_equal ~expected:3 ~actual:result.summary.would_reformat;
      Test.assert_equal ~expected:0 ~actual:result.summary.unsafe_to_format;
      Ok ())

let test_syntax_hash_normalizes_commented_pipeline_parens = fun _ctx ->
  let original =
    parse_ml
      {ocaml|let run events=if List.is_empty events then Error "empty" else (
(* Log each event *)
events|>List.enumerate|>List.for_each ~fn:(fun (_i,event)->ignore event);
Ok())
|ocaml}
  in
  let with_parens =
    parse_ml
      {ocaml|let run events=if List.is_empty events then Error "empty" else (
(* Log each event *)
(events|>List.enumerate)|>List.for_each ~fn:(fun (_i,event)->ignore event);
Ok())
|ocaml}
  in
  Test.assert_equal ~expected:(Krasny.syntax_hash original) ~actual:(Krasny.syntax_hash with_parens);
  Ok ()

let test_syntax_hash_normalizes_tuple_edge_parens = fun _ctx ->
  let parenthesized = parse_ml {ocaml|let pair=(left,right)
|ocaml}
  in
  let bare = parse_ml {ocaml|let pair=left,right
|ocaml}
  in
  Test.assert_equal ~expected:(Krasny.syntax_hash parenthesized) ~actual:(Krasny.syntax_hash bare);
  let parenthesized_triple = parse_ml {ocaml|let result={value=(a,b,c);remaining=d}
|ocaml}
  in
  let bare_triple = parse_ml {ocaml|let result={value=a,b,c;remaining=d}
|ocaml}
  in
  Test.assert_equal
    ~expected:(Krasny.syntax_hash parenthesized_triple)
    ~actual:(Krasny.syntax_hash bare_triple);
  let nested_pair = parse_ml {ocaml|let result=(a,b),c
|ocaml}
  in
  Test.assert_false (String.equal (Krasny.syntax_hash bare_triple) (Krasny.syntax_hash nested_pair));
  let parenthesized_pattern =
    parse_ml
      {ocaml|let equal left right=match (left,right) with|(None,None)->true|(Some left,Some right)->left=right|_->false
|ocaml}
  in
  let bare_pattern =
    parse_ml
      {ocaml|let equal left right=match left,right with|None,None->true|Some left,Some right->left=right|_->false
|ocaml}
  in
  Test.assert_equal
    ~expected:(Krasny.syntax_hash parenthesized_pattern)
    ~actual:(Krasny.syntax_hash bare_pattern);
  Ok ()

let test_syntax_hash_normalizes_leading_variant_pipes = fun _ctx ->
  let without_first_pipe = parse_mli {ocaml|type role=Client|Server
|ocaml}
  in
  let with_first_pipe = parse_mli {ocaml|type role=|Client|Server
|ocaml}
  in
  Test.assert_equal
    ~expected:(Krasny.syntax_hash without_first_pipe)
    ~actual:(Krasny.syntax_hash with_first_pipe);
  Ok ()

let test_syntax_hash_normalizes_record_pattern_trailing_semis = fun _ctx ->
  let with_trailing_semi =
    parse_ml {ocaml|let get=function|Some {left;right;}->left+right|None->0
|ocaml}
  in
  let without_trailing_semi =
    parse_ml {ocaml|let get=function|Some {left;right}->left+right|None->0
|ocaml}
  in
  Test.assert_equal
    ~expected:(Krasny.syntax_hash with_trailing_semi)
    ~actual:(Krasny.syntax_hash without_trailing_semi);
  Ok ()

let test_syntax_hash_normalizes_list_pattern_trailing_semis = fun _ctx ->
  let with_trailing_semi =
    parse_ml
      {ocaml|let get=function|[left;right;]->left+right|[|left;right;|]->left+right|_->0
|ocaml}
  in
  let without_trailing_semi =
    parse_ml
      {ocaml|let get=function|[left;right]->left+right|[|left;right|]->left+right|_->0
|ocaml}
  in
  Test.assert_equal
    ~expected:(Krasny.syntax_hash with_trailing_semi)
    ~actual:(Krasny.syntax_hash without_trailing_semi);
  Ok ()

let test_syntax_hash_normalizes_constructor_pattern_parens = fun _ctx ->
  let parenthesized = parse_ml {ocaml|let sum=function|(Some value)::rest->value|_->0
|ocaml}
  in
  let bare = parse_ml {ocaml|let sum=function|Some value::rest->value|_->0
|ocaml}
  in
  Test.assert_equal ~expected:(Krasny.syntax_hash parenthesized) ~actual:(Krasny.syntax_hash bare);
  Ok ()

let test_syntax_hash_normalizes_function_desugaring = fun _ctx ->
  let multi_case_function =
    parse_ml {ocaml|let classify=function|Some value->value|None->0
|ocaml}
  in
  let multi_case_fun_match =
    parse_ml {ocaml|let classify=fun __tmp1->match __tmp1 with|Some value->value|None->0
|ocaml}
  in
  Test.assert_equal
    ~expected:(Krasny.syntax_hash multi_case_function)
    ~actual:(Krasny.syntax_hash multi_case_fun_match);
  let single_case_function = parse_ml {ocaml|let message=function|Error msg->msg
|ocaml}
  in
  let single_case_fun = parse_ml {ocaml|let message=fun (Error msg)->msg
|ocaml}
  in
  Test.assert_equal
    ~expected:(Krasny.syntax_hash single_case_function)
    ~actual:(Krasny.syntax_hash single_case_fun);
  Ok ()

let test_syntax_hash_normalizes_trailing_sequence_semis = fun _ctx ->
  let with_trailing_semi =
    parse_ml {ocaml|let run=function|Ok value->println value;|Error error->println error
|ocaml}
  in
  let without_trailing_semi =
    parse_ml {ocaml|let run=function|Ok value->println value|Error error->println error
|ocaml}
  in
  Test.assert_equal
    ~expected:(Krasny.syntax_hash with_trailing_semi)
    ~actual:(Krasny.syntax_hash without_trailing_semi);
  Ok ()

let test_syntax_hash_normalizes_trivia_line_indentation = fun _ctx ->
  let shallow =
    parse_mli {ocaml|module S:sig
val run:unit->unit
(** doc
    details *)
end
|ocaml}
  in
  let indented =
    parse_mli {ocaml|module S:sig
val run:unit->unit
(** doc
      details *)
end
|ocaml}
  in
  Test.assert_equal ~expected:(Krasny.syntax_hash shallow) ~actual:(Krasny.syntax_hash indented);
  Ok ()

let test_format_keeps_function_and_match_rendering_idempotent = fun _ctx ->
  let source =
    {|let f = function x, y -> x + y
let g = function 0 -> "zero" | _ -> "other"
let h = fun x -> match x with 0 -> "zero" | _ -> "other"
|}
  in
  assert_idempotent ~source ~msg:"function and match forms should stay stable";
  Ok ()

let test_format_keeps_let_if_sequence_layouts_idempotent = fun _ctx ->
  let source =
    {|let x =
  if a then (
    b;
    c)
  else d

let y =
  let rec f n = if n = 0 then 1 else n * f (n - 1) in
  f 5
|}
  in
  assert_idempotent ~source ~msg:"control-flow layouts should stay stable";
  Ok ()

let test_format_keeps_typed_and_labeled_bindings_idempotent = fun _ctx ->
  let source =
    {|let delimiter_of_keyword : keyword -> delimiter option = function | Begin -> Some BeginEnd | _ -> None
let label_arg = f ~y
let optional_arg = f ?y
let optional_fun = fun ?(y = 0) -> y + 1
|}
  in
  assert_idempotent ~source ~msg:"typed/labeled forms should stay stable";
  Ok ()

let test_format_keeps_labeled_infix_arguments_singly_parenthesized = fun _ctx ->
  let source = "let next = foo ~pos:(pos + read)\n" in
  let actual =
    parse_ml source
    |> Krasny.format
    |> Result.expect ~msg:"labeled infix arguments should not gain redundant parentheses"
  in
  Test.assert_equal ~expected:source ~actual;
  Ok ()

let test_format_keeps_structural_named_parameters_with_defaults_idempotent = fun _ctx ->
  let source =
    {|let configure ?(timeout : int = 30) ?retry:retries ~point:{ x; y } ~limit:seconds () =
  (timeout, retries, x, y, seconds)
|}
  in
  assert_idempotent
    ~source
    ~msg:"named parameter defaults, renames, and destructuring should format structurally";
  Ok ()

let test_format_keeps_signature_operator_values_structural = fun _ctx ->
  let source =
    {|val ( = ) : 'a -> 'a -> bool
val (mod) : int -> int -> int
val ( := ) : 'a ref -> 'a -> unit
|}
  in
  let formatted =
    parse_mli source
    |> Krasny.format
    |> Result.expect ~msg:"operator value declarations should format structurally"
  in
  Test.assert_equal
    ~expected:{|val ( = ): 'a -> 'a -> bool

val ( mod ): int -> int -> int

val ( := ): 'a ref -> 'a -> unit
|}
    ~actual:formatted;
  Ok ()

let test_format_keeps_alias_patterns_idempotent = fun _ctx ->
  let source = {|open Std

let request = fun (Conn conn as c) () -> ()
|}
  in
  assert_idempotent ~source ~msg:"alias patterns should stay stable";
  Ok ()

let test_format_keeps_constructor_parameter_patterns_idempotent = fun _ctx ->
  let source = {|open Std

let request = fun (Conn conn) () -> ()
|}
  in
  assert_idempotent ~source ~msg:"constructor parameter patterns should not gain extra parentheses";
  Ok ()

let test_format_keeps_typed_first_class_module_patterns_idempotent = fun _ctx ->
  let source = {|let run_comparison index (module R : Reporter.Intf.Intf) comp = (index, comp)
|}
  in
  assert_idempotent ~source ~msg:"typed first-class module patterns should render structurally";
  Ok ()

let test_format_keeps_first_class_module_expressions_idempotent = fun _ctx ->
  let source =
    {|open Std

module Protocol = struct
  module Http1 = struct end
end

let packed = (module Protocol.Http1)
|}
  in
  assert_idempotent ~source ~msg:"first-class module expressions should stay stable";
  Ok ()

let test_format_keeps_structural_signature_items_idempotent = fun _ctx ->
  let source =
    {|[@@@warning "-32"]

type t +=
  | Added of int

exception Parse_error of string
exception Nested = Std.Result.Error
|}
  in
  let formatted =
    parse_mli source
    |> Krasny.format
    |> Result.expect
      ~msg:"signature attributes, type extensions, and exceptions should format structurally"
  in
  let reparsed =
    parse_mli formatted
    |> Krasny.format
    |> Result.expect ~msg:"formatted signature items should reformat"
  in
  Test.assert_equal ~expected:formatted ~actual:reparsed;
  Ok ()

let test_format_floating_attributes_from_structural_payload_items = fun ctx ->
  let source = "[@@@warning    \"-32\"]\n" in
  assert_formatted_ml_snapshot
    ~ctx
    ~msg:"floating attributes should render from structural payload items"
    source

let test_format_floating_extension_items_structurally = fun ctx ->
  let structure_source = {|[%%foo]
[%%bar let x = 1]
|}
  in
  assert_formatted_ml_snapshot
    ~ctx
    ~msg:"floating structure extensions should render structurally from the extension shell and payload"
    structure_source

let test_format_preserves_scientific_float_exponents_without_introducing_spaces = fun _ctx ->
  let source = {|let trillion = 1.0e12
let tiny = 1.0e-6
let tagged = 1.2e3g
|}
  in
  let actual =
    parse_ml source
    |> Krasny.format
    |> Result.expect ~msg:"scientific float literals should format structurally"
  in
  Test.assert_equal
    ~expected:{|let trillion = 1.0e12

let tiny = 1.0e-6

let tagged = 1.2e3g
|}
    ~actual;
  Ok ()

let test_format_module_expression_and_module_type_extensions_structurally = fun ctx ->
  let source = {|module type S = [%foo]
module M = [%foo]
|}
  in
  assert_formatted_ml_snapshot
    ~ctx
    ~msg:"module-expression and module-type extensions should render from the structural extension shell"
    source

let test_format_structural_core_type_token_fallbacks = fun _ctx ->
  let source = {|val use : #service -> M.(t list) -> < close : unit -> unit; next : int >
|}
  in
  let actual =
    parse_mli source
    |> Krasny.format
    |> Result.expect ~msg:"structural core type token fallback should format"
  in
  Test.assert_equal
    ~expected:{|val use: # service -> M.(t list) -> < close:unit -> unit; next:int >
|}
    ~actual;
  Ok ()

let test_format_first_class_module_types_from_structural_module_type_docs = fun ctx ->
  let source =
    {|type packed = (module   Transport   with   type t = int)
type extended = (module [%foo])
type payload = (module [%foo: S])
|}
  in
  let actual =
    parse_ml source
    |> Krasny.format
    |> Result.expect ~msg:"first-class module types should format through token fallback"
  in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{|type packed = (module Transport with type t = int)

type extended = (module [%foo])

type payload = (module [%foo: S])
|}

let test_format_shared_core_type_attributes_keeps_opaque_payload_tokens = fun _ctx ->
  let source = "type t = int [@deprecated   \"use other\"]\n" in
  let actual =
    parse_ml source
    |> Krasny.format
    |> Result.expect ~msg:"shared core-type attributes should render from opaque payload tokens"
  in
  Test.assert_equal ~expected:"type t = int [@deprecated \"use other\"]\n" ~actual;
  Ok ()

let test_format_shared_attribute_payload_infix_expressions_opaquely = fun _ctx ->
  let source = "type t = int [@foo 1 + 2]\n" in
  let actual =
    parse_ml source
    |> Krasny.format
    |> Result.expect ~msg:"shared attribute payload infix expressions should render opaquely"
  in
  Test.assert_equal ~expected:source ~actual;
  Ok ()

let test_format_expression_attributes_keeps_opaque_payload_tokens = fun _ctx ->
  let source = "let _ = value [@foo   1  +  2]\n" in
  let actual =
    parse_ml source
    |> Krasny.format
    |> Result.expect ~msg:"expression attributes should render from opaque payload tokens"
  in
  Test.assert_equal ~expected:"let _ = value [@foo 1 + 2]\n" ~actual;
  Ok ()

let test_format_ordinary_pattern_payload_attributes_structurally = fun ctx ->
  let source = {|let simple = 1 [@foo? Some y]
let guarded = 1 [@foo? Some y when y > 0]
|}
  in
  assert_formatted_ml_snapshot
    ~ctx
    ~msg:"ordinary pattern-payload attributes should render structurally"
    source

let test_format_parenthesizes_attributed_non_atomic_expressions = fun ctx ->
  let source =
    {|let constructor = Some 0 [@inline always]
let apply = I64.logor b (I64.shift_left b 32) [@inline always]
let infix = mask land (mask - 1) [@inline always]
|}
  in
  assert_formatted_ml_snapshot
    ~ctx
    ~msg:"postfix expression attributes should preserve attributed apply and infix payloads"
    source

let test_format_currently_fails_for_plain_object_expressions = fun _ctx ->
  let source =
    {|let empty = object end
let methods =
  object
    method m = 1
    method! private x = 3 [@@foo]
  end
let fields =
  object
    val mutable y = 1
    val virtual z : t [@@foo]
  end
let self_ =
  object (self)
    method m = self#n
  end
let inherited =
  object
    inherit c [@@foo]
    initializer setup [@@foo]
  end
let typed =
  (object
     method m = 1
   end
   : < m : int >)
|}
  in
  assert_format_ml_fails ~msg:"plain object expressions are not supported structurally yet" source

let test_format_currently_fails_for_object_core_types = fun _ctx ->
  let source = "type t = < run : int >\n" in
  assert_format_mli_fails ~msg:"object core types are outside parser2 formatter scope" source

let test_format_object_bodies_preserve_terminal_trivia = fun _ctx ->
  let source =
    {|let empty = object
  (* trailing comment *)
  (** trailing docstring *)
  method run = 1
  (* trailing comment *)
  (** trailing docstring *)
  end
|}
  in
  assert_format_ml_fails ~msg:"object bodies are outside parser2 formatter scope" source

let test_format_object_extension_members_structurally = fun _ctx ->
  let source = {|let extended =
  object
    [%%foo]
    [%%bar let x = 1]
  end
|}
  in
  assert_format_ml_fails ~msg:"object extension members are outside parser2 formatter scope" source

let test_format_trailing_variant_comments_with_explicit_separator_policy = fun ctx ->
  let source = "type t =\n  | A (* comment *)\n" in
  assert_formatted_ml_snapshot
    ~ctx
    ~msg:"trailing variant comments should format from explicit trivia separators"
    source

let test_format_trailing_variant_docstrings_with_explicit_separator_policy = fun ctx ->
  let source = "type t =\n  | A (** doc *)\n" in
  assert_formatted_ml_snapshot
    ~ctx
    ~msg:"trailing variant docstrings should format from explicit trivia separators"
    source

let test_format_fails_for_signature_bodied_first_class_module_types = fun _ctx ->
  let source = {|type packed = (module sig
  type t
end)
|}
  in
  match parse_ml source
  |> Krasny.format with
  | Ok _ ->
      panic
        "signature-bodied first-class module types should fail until they have a structural formatter"
  | Error _ -> Ok ()

let test_format_core_type_extensions_structurally = fun ctx ->
  let source = "val use : [%foo: int]\n" in
  assert_formatted_mli_snapshot
    ~ctx
    ~msg:"core-type extensions should render structurally from the extension shell and payload"
    source

let test_format_keeps_structural_patterns_idempotent = fun _ctx ->
  let source =
    {|let unpack = function
  | (module M) -> ()
  | (M.(Some x) as whole) -> whole
  | (lazy y : t) -> y
|}
  in
  assert_idempotent
    ~source
    ~msg:"first-class module, local-open, alias, and typed patterns should format structurally";
  Ok ()

let test_format_keeps_polymorphic_variant_inherit_patterns_idempotent = fun _ctx ->
  let source = "let x = match y with #color -> 1\n" in
  let actual =
    parse_ml source
    |> Krasny.format
    |> Result.expect
      ~msg:"polymorphic-variant inherit patterns should render from the structural path"
  in
  Test.assert_equal ~expected:{|let x =
  match y with
  | #color -> 1
|} ~actual;
  assert_idempotent ~source ~msg:"polymorphic-variant inherit patterns should stay stable";
  Ok ()

let test_format_typed_first_class_module_patterns_structurally = fun ctx ->
  let source = {ocaml|let unpack=function|(module M:S)->()|(module _)->()
|ocaml}
  in
  let parsed = parse_ml source in
  let actual = capture_write parsed in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{ocaml|let unpack = fun __tmp1 ->
  match __tmp1 with
  | (module M : S) -> ()
  | (module _) -> ()
|ocaml}

let test_format_pattern_extensions_structurally = fun ctx ->
  let source =
    {|let unpack = function
  | [%foo? Some x] -> x
  | [%foo? Some y when y > 0] -> y
|}
  in
  assert_formatted_ml_snapshot
    ~ctx
    ~msg:"pattern extensions should render structurally from the extension shell and payload"
    source

let test_format_keeps_structural_imperative_and_module_expressions_idempotent = fun _ctx ->
  let source =
    {|let packed = (module M : S)
let guarded ready = assert ready
let delayed compute = lazy (compute ())
let loop cond body = while cond () do body () done
let count () = for i = 10 downto 0 do print_int i done
let cast value = (value : source :> target)
let widen value = (value :> target)
|}
  in
  assert_idempotent
    ~source
    ~msg:"module-pack, imperative, and coercion expressions should format structurally";
  Ok ()

let test_format_object_override_expressions = fun _ctx ->
  let source = {|let update next count = {< current = next; count >}
|}
  in
  assert_format_ml_fails
    ~msg:"object override expressions are outside parser2 formatter scope"
    source

let test_format_expression_extensions_structurally = fun ctx ->
  let source =
    {|let generated = [%foo]
let computed = [%test 42]
let typed = [%foo: int]
let nested = [%foo let x = 1]
|}
  in
  assert_formatted_ml_snapshot
    ~ctx
    ~msg:"expression extensions should render structurally from the extension shell and payload"
    source

let test_format_atomic_loc_extension_keeps_qualified_name = fun _ctx ->
  let source = {|let foo = fun t -> [%atomic.loc t.a]
|}
  in
  let actual =
    parse_ml source
    |> Krasny.format
    |> Result.expect ~msg:"atomic.loc extension should preserve qualified name and payload boundary"
  in
  Test.assert_equal ~expected:source ~actual;
  Ok ()

let test_format_unreachable_expressions_structurally = fun _ctx ->
  let source = {|let absurd maybe =
  match maybe with
  | Some value -> value
  | None -> .
|}
  in
  let actual =
    parse_ml source
    |> Krasny.format
    |> Result.expect ~msg:"unreachable expressions should render structurally from the CST token"
  in
  Test.assert_equal ~expected:source ~actual;
  assert_idempotent
    ~source
    ~msg:"unreachable expressions should stay stable across repeated formatting";
  Ok ()

let test_format_keeps_typed_and_polymorphic_expressions_structural = fun ctx ->
  let source =
    {ocaml|let typed value = (value : source)
let poly : 'a. 'a -> 'a = fun x -> x
|ocaml}
  in
  assert_formatted_ml_snapshot
    ~ctx
    ~msg:"typed and polymorphic expressions should render through structural core-type rendering"
    source

let test_format_keeps_nested_module_bodies_structural = fun _ctx ->
  let source =
    {|module type S = sig
  (** x *)
  val x : int
end

module M = struct
  let x = 1
end
|}
  in
  assert_idempotent
    ~source
    ~msg:"nested signature and structure bodies should render from structural item streams";
  Ok ()

let test_format_keeps_grouped_gadt_type_declarations_structural = fun _ctx ->
  let source = {|type _ expr =
  | Int : int expr
and packed =
  | Packed : int expr -> packed
|}
  in
  assert_idempotent
    ~source
    ~msg:"grouped GADT type declarations should render structurally instead of preserving source";
  Ok ()

let test_format_inline_record_constructors_from_structure_not_source_newlines = fun _ctx ->
  let source = {|type t =
  | A of {
      x : int;
      y : int;
    }
  | B
|}
  in
  let actual =
    parse_ml source
    |> Krasny.format
    |> Result.expect ~msg:"inline record constructors should format structurally"
  in
  Test.assert_equal ~expected:{|type t =
  | A of { x: int; y: int }
  | B
|} ~actual;
  Ok ()

let test_format_keeps_boolean_if_conditions_with_matches_idempotent = fun _ctx ->
  let source =
    {|open Std

let status_char mode summary =
  if
    match mode with
    | Runner.Check -> summary.needs_formatting = 0 && summary.failed_files = 0
    | Runner.Verify -> summary.unsafe_to_format = 0 && summary.failed_files = 0
    | Runner.Format -> summary.failed_files = 0
  then
    "."
  else
    "!"
|}
  in
  assert_idempotent ~source ~msg:"boolean match conditions should stay stable";
  Ok ()

let test_format_keeps_simple_nested_match_case_bodies_idempotent = fun ctx ->
  let source =
    {ocaml|let finish=fun lane lane_result->{had_partial_failure=lane_had_error lane||match lane_result with|Some result->Lane_result.had_partial_failure result|None->false;}
|ocaml}
  in
  let first =
    parse_ml source
    |> Krasny.format
    |> Result.expect ~msg:"nested match field should format"
  in
  let second =
    parse_ml first
    |> Krasny.format
    |> Result.expect ~msg:"formatted nested match field should reformat"
  in
  Test.assert_equal ~expected:first ~actual:second;
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual:first
    ~expected:{ocaml|let finish = fun lane lane_result ->
  {
    had_partial_failure =
      lane_had_error lane || match lane_result with
      | Some result -> Lane_result.had_partial_failure result
      | None ->
          false;
  }
|ocaml}

let test_format_keeps_top_level_fun_phrases_separated = fun _ctx ->
  let source = {|open Std

let ( .??[] ) () () = ();;

(()).??[(();
         ())]
;;
|}
  in
  assert_idempotent ~source ~msg:"top-level expression phrases should stay outside fun bindings";
  Ok ()

let test_format_keeps_top_level_phrase_separators_structural = fun ctx ->
  let source = {|let project x = x
;;
1
;;
module M = struct end
|}
  in
  assert_formatted_ml_snapshot
    ~ctx
    ~msg:"top-level phrase separators should come from source-file tokens, not source gaps"
    source

let test_format_preserves_syntax_hash_for_selected_codebase_files = fun _ctx ->
  List.for_each workspace_files ~fn:assert_roundtrip_hash;
  Ok ()

let test_runner_skips_hidden_and_build_directories = fun _ctx ->
  with_tempdir
    "krasny_runner_scan"
    (fun tmpdir ->
      let visible_ml = Path.(tmpdir / Path.v "visible.ml") in
      let nested_dir = Path.(tmpdir / Path.v "nested") in
      let nested_mli = Path.(nested_dir / Path.v "visible.mli") in
      let hidden_dir = Path.(tmpdir / Path.v ".hidden") in
      let build_dir = Path.(tmpdir / Path.v "_build") in
      Fs.create_dir_all nested_dir
      |> Result.expect ~msg:"create nested";
      Fs.create_dir_all hidden_dir
      |> Result.expect ~msg:"create hidden";
      Fs.create_dir_all build_dir
      |> Result.expect ~msg:"create build";
      Fs.write "let x = 1\n" visible_ml
      |> Result.expect ~msg:"write visible";
      Fs.write "val x : int\n" nested_mli
      |> Result.expect ~msg:"write nested";
      Fs.write "let hidden = 1\n" Path.(hidden_dir / Path.v "hidden.ml")
      |> Result.expect ~msg:"write hidden";
      Fs.write "let built = 1\n" Path.(build_dir / Path.v "built.ml")
      |> Result.expect ~msg:"write build";
      let files =
        Krasny.Runner.collect_ocaml_files ~roots:[ tmpdir ] ()
        |> List.map ~fn:Path.to_string
      in
      let expected =
        [ Path.to_string visible_ml; Path.to_string nested_mli ]
        |> List.sort ~compare:String.compare
      in
      let actual = List.sort files ~compare:String.compare in
      Test.assert_equal ~expected ~actual;
      Ok ())

let test_runner_skips_ignored_subtrees_during_collection = fun _ctx ->
  with_tempdir
    "krasny_runner_ignore_tree"
    (fun tmpdir ->
      let keep = Path.(tmpdir / Path.v "keep.ml") in
      let fixtures_dir = Path.(tmpdir / Path.v "tests" / Path.v "fixtures") in
      let ignored = Path.(fixtures_dir / Path.v "fixture.ml") in
      Fs.create_dir_all fixtures_dir
      |> Result.expect ~msg:"create fixtures dir";
      Fs.write "let kept = 1\n" keep
      |> Result.expect ~msg:"write keep";
      Fs.write "let ignored = 1\n" ignored
      |> Result.expect ~msg:"write ignored";
      let files =
        Krasny.Runner.collect_ocaml_files
          ~roots:[ tmpdir ]
          ~should_ignore:(fun path -> String.contains (Path.to_string path) "fixtures")
          ()
        |> List.map ~fn:Path.to_string
      in
      Test.assert_equal ~expected:[ Path.to_string keep ] ~actual:files;
      Ok ())

let test_runner_reports_formatting_status_and_emits_json_events = fun _ctx ->
  with_tempdir
    "krasny_runner_check"
    (fun tmpdir ->
      let formatted = Path.(tmpdir / Path.v "formatted.ml") in
      let needs = Path.(tmpdir / Path.v "needs.ml") in
      Fs.write "let x = 1 + 2\n" formatted
      |> Result.expect ~msg:"write formatted";
      Fs.write "let x = 1 + 2\nlet f x = x + 1\n" needs
      |> Result.expect ~msg:"write needs";
      let result = Krasny.Runner.run_checks [ formatted; needs ] in
      Test.assert_equal ~expected:2 ~actual:result.summary.total_files;
      Test.assert_equal ~expected:1 ~actual:result.summary.already_formatted;
      Test.assert_equal ~expected:1 ~actual:result.summary.needs_formatting;
      Test.assert_equal ~expected:0 ~actual:result.summary.would_reformat;
      Test.assert_equal ~expected:0 ~actual:result.summary.unsafe_to_format;
      Test.assert_equal ~expected:0 ~actual:result.summary.failed_files;
      let needs_result =
        result.files
        |> List.find
          ~fn:(fun file_result ->
            String.equal
              (Path.to_string file_result.Krasny.Runner.file)
              (Path.to_string needs))
        |> Option.expect ~msg:"needs result missing"
      in
      let json =
        capture_json_event ~root:tmpdir (Krasny.Report.File needs_result)
        |> Data.Json.of_string
        |> Result.expect ~msg:"parse event json"
      in
      let open Data.Json in
      Test.assert_equal ~expected:(Some (String "file")) ~actual:(get_field "type" json);
      assert_json_timestamp_field json;
      assert_json_duration_ms_field json;
      Test.assert_equal ~expected:(Some (String "needs.ml")) ~actual:(get_field "file" json);
      Test.assert_equal
        ~expected:(Some (String "needs_formatting"))
        ~actual:(get_field "status" json);
      Ok ())

let test_verify_reports_files_that_would_reformat_safely = fun _ctx ->
  with_tempdir
    "krasny_runner_verify"
    (fun tmpdir ->
      let formatted = Path.(tmpdir / Path.v "formatted.ml") in
      let needs = Path.(tmpdir / Path.v "needs.ml") in
      Fs.write "let x = 1 + 2\n" formatted
      |> Result.expect ~msg:"write formatted";
      Fs.write "let x = 1 + 2\nlet f x = x + 1\n" needs
      |> Result.expect ~msg:"write needs";
      let result = Krasny.Runner.run_verify [ formatted; needs ] in
      Test.assert_equal ~expected:2 ~actual:result.summary.total_files;
      Test.assert_equal ~expected:1 ~actual:result.summary.already_formatted;
      Test.assert_equal ~expected:0 ~actual:result.summary.needs_formatting;
      Test.assert_equal ~expected:1 ~actual:result.summary.would_reformat;
      Test.assert_equal ~expected:0 ~actual:result.summary.unsafe_to_format;
      Test.assert_equal ~expected:0 ~actual:result.summary.failed_files;
      let needs_result =
        result.files
        |> List.find
          ~fn:(fun file_result ->
            String.equal
              (Path.to_string file_result.Krasny.Runner.file)
              (Path.to_string needs))
        |> Option.expect ~msg:"verify result missing"
      in
      let json =
        capture_json_event ~root:tmpdir (Krasny.Report.File needs_result)
        |> Data.Json.of_string
        |> Result.expect ~msg:"parse event json"
      in
      let open Data.Json in
      Test.assert_equal ~expected:(Some (String "would_reformat")) ~actual:(get_field "status" json);
      Ok ())

let test_format_rewrites_files_in_place_and_reports_formatted_status = fun _ctx ->
  with_tempdir
    "krasny_runner_format"
    (fun tmpdir ->
      let formatted = Path.(tmpdir / Path.v "formatted.ml") in
      let needs = Path.(tmpdir / Path.v "needs.ml") in
      Fs.write "let x = 1 + 2\n" formatted
      |> Result.expect ~msg:"write formatted";
      Fs.write "let x = 1 + 2\nlet f x = x + 1\n" needs
      |> Result.expect ~msg:"write needs";
      let result = Krasny.Runner.run_format [ formatted; needs ] in
      Test.assert_equal ~expected:2 ~actual:result.summary.total_files;
      Test.assert_equal ~expected:1 ~actual:result.summary.already_formatted;
      Test.assert_equal ~expected:1 ~actual:result.summary.formatted_files;
      Test.assert_equal ~expected:0 ~actual:result.summary.failed_files;
      let formatted_source =
        Fs.read needs
        |> Result.expect ~msg:"read formatted output"
      in
      Test.assert_equal ~expected:"let x = 1 + 2\n\nlet f x = x + 1\n" ~actual:formatted_source;
      let file_result =
        result.files
        |> List.find
          ~fn:(fun file_result ->
            String.equal
              (Path.to_string file_result.Krasny.Runner.file)
              (Path.to_string needs))
        |> Option.expect ~msg:"format result missing"
      in
      let json =
        capture_json_event ~root:tmpdir (Krasny.Report.File file_result)
        |> Data.Json.of_string
        |> Result.expect ~msg:"parse event json"
      in
      let open Data.Json in
      Test.assert_equal ~expected:(Some (String "formatted")) ~actual:(get_field "status" json);
      Ok ())

let test_json_file_events_include_structured_diagnostics_for_parse_failures = fun _ctx ->
  with_tempdir
    "krasny_runner_json_diagnostics"
    (fun tmpdir ->
      let broken = Path.(tmpdir / Path.v "broken.ml") in
      let source = "let x =\n" in
      Fs.write source broken
      |> Result.expect ~msg:"write broken";
      let result = Krasny.Runner.run_format [ broken ] in
      Test.assert_equal ~expected:1 ~actual:result.summary.failed_files;
      let file_result =
        result.files
        |> List.find
          ~fn:(fun file_result ->
            String.equal
              (Path.to_string file_result.Krasny.Runner.file)
              (Path.to_string broken))
        |> Option.expect ~msg:"broken result missing"
      in
      match file_result.Krasny.Runner.diagnostics with
      | Some diagnostics ->
          if List.is_empty diagnostics then
            Error "expected broken source to carry diagnostics"
          else
            let json =
              capture_json_event ~root:tmpdir (Krasny.Report.File file_result)
              |> Data.Json.of_string
              |> Result.expect ~msg:"parse event json"
            in
            let expected = Some (Data.Json.Array (List.map diagnostics ~fn:Syn.Diagnostic.to_json))
            in
            Test.assert_equal ~expected ~actual:(Data.Json.get_field "diagnostics" json);
          Ok ()
      | None -> Error "expected broken source to carry diagnostics")

let test_streaming_runner_skips_ignored_files = fun _ctx ->
  with_tempdir
    "krasny_runner_ignore"
    (fun tmpdir ->
      let keep = Path.(tmpdir / Path.v "keep.ml") in
      let fixtures_dir = Path.(tmpdir / Path.v "tests" / Path.v "fixtures") in
      let ignored = Path.(fixtures_dir / Path.v "fixture.ml") in
      Fs.create_dir_all fixtures_dir
      |> Result.expect ~msg:"create fixtures dir";
      Fs.write "let kept = 1\n" keep
      |> Result.expect ~msg:"write keep";
      Fs.write "let ignored = 1\n" ignored
      |> Result.expect ~msg:"write ignored";
      let seen = cell [] in
      let result =
        Krasny.Runner.run_checks_streaming
          ~concurrency:1
          ~roots:[ tmpdir ]
          ~should_ignore:(fun path -> String.contains (Path.to_string path) "fixtures")
          ~on_result:(fun file_result -> seen := Path.to_string file_result.file :: !seen)
          ()
      in
      Test.assert_equal ~expected:[ Path.to_string keep ] ~actual:(List.reverse !seen);
      Test.assert_equal ~expected:1 ~actual:result.summary.total_files;
      Ok ())

let test_streaming_runner_scans_roots_and_streams_file_results = fun _ctx ->
  with_tempdir
    "krasny_runner_stream"
    (fun tmpdir ->
      let formatted = Path.(tmpdir / Path.v "formatted.ml") in
      let nested_dir = Path.(tmpdir / Path.v "nested") in
      let needs = Path.(nested_dir / Path.v "needs.mli") in
      Fs.create_dir_all nested_dir
      |> Result.expect ~msg:"create nested";
      Fs.write "let x = 1 + 2\n" formatted
      |> Result.expect ~msg:"write formatted";
      Fs.write "val x: int\n" needs
      |> Result.expect ~msg:"write needs";
      let seen = cell [] in
      let result =
        Krasny.Runner.run_checks_streaming
          ~concurrency:1
          ~roots:[ tmpdir ]
          ~on_result:(fun file_result -> seen := Path.to_string file_result.file :: !seen)
          ()
      in
      let actual = List.sort !seen ~compare:String.compare in
      let expected =
        [ Path.to_string formatted; Path.to_string needs ]
        |> List.sort ~compare:String.compare
      in
      Test.assert_equal ~expected ~actual;
      Test.assert_equal ~expected:2 ~actual:result.summary.total_files;
      Test.assert_equal ~expected:2 ~actual:result.summary.already_formatted;
      Test.assert_equal ~expected:0 ~actual:result.summary.needs_formatting;
      Test.assert_equal ~expected:0 ~actual:result.summary.would_reformat;
      Test.assert_equal ~expected:0 ~actual:result.summary.unsafe_to_format;
      Test.assert_equal ~expected:0 ~actual:result.summary.failed_files;
      let start_json =
        capture_json_event
          ~root:tmpdir
          (Krasny.Report.Start { mode = Krasny.Runner.Check; concurrency = 3 })
        |> Data.Json.of_string
        |> Result.expect ~msg:"parse start json"
      in
      let open Data.Json in
      Test.assert_equal ~expected:(Some (String "start")) ~actual:(get_field "type" start_json);
      assert_json_timestamp_field start_json;
      Test.assert_equal ~expected:(Some (Int 3)) ~actual:(get_field "concurrency" start_json);
      Test.assert_equal ~expected:(Some (String "check")) ~actual:(get_field "mode" start_json);
      Test.assert_equal ~expected:None ~actual:(get_field "total_files" start_json);
      Ok ())

let tests =
  Test.[
    case
      "format returns the original source for a simple implementation"
      test_format_returns_the_original_source_for_a_simple_implementation;
    case
      "format_source uses the public krasny parse facade"
      test_format_source_uses_the_public_krasny_parse_facade;
    case
      "syntax hash normalizes formatter-safe punctuation"
      test_syntax_hash_normalizes_formatter_safe_punctuation;
    case
      "format adds a final newline to non-empty output"
      test_format_adds_a_final_newline_to_non_empty_output;
    case "format keeps empty files empty" test_format_keeps_empty_files_empty;
    case
      "write renders formatted output into the supplied writer"
      test_write_renders_formatted_output_into_the_supplied_writer;
    case "write renders simple interfaces" test_write_renders_simple_interfaces;
    case
      "write normalizes multiline docstring indentation"
      test_write_normalizes_multiline_docstring_indentation;
    case
      "write preserves relative indentation inside docstrings"
      test_write_preserves_relative_indentation_inside_docstrings;
    case
      "write collapses blank lines before leading comments"
      test_write_collapses_blank_lines_before_leading_comments;
    case
      "format keeps signature docstring spacing idempotent"
      test_format_keeps_signature_docstring_spacing_idempotent;
    case
      "write preserves terminal docstrings before nested signature end"
      test_write_preserves_terminal_docstrings_before_nested_signature_end;
    case "write preserves module functor parameters" test_write_preserves_module_functor_parameters;
    case
      "write keeps adjacent module structures separated"
      test_write_keeps_adjacent_module_structures_separated;
    case
      "write preserves variant constructor docstrings in interfaces"
      test_write_preserves_variant_constructor_docstrings_in_interfaces;
    case "write renders short record types inline" test_write_renders_short_record_types_inline;
    case "write keeps heavy record types vertical" test_write_keeps_heavy_record_types_vertical;
    case
      "write preserves terminal record field docstrings"
      test_write_preserves_terminal_record_field_docstrings;
    case
      "write renders type alias record representations"
      test_write_renders_type_alias_record_representations;
    case
      "write breaks record type aliases when width is exceeded"
      test_write_breaks_record_type_aliases_when_width_is_exceeded;
    case
      "write preserves unit parameter return annotation"
      test_write_preserves_unit_parameter_return_annotation;
    case
      "write preserves function return annotation before record body"
      test_write_preserves_function_return_annotation_before_record_body;
    case
      "write preserves local function return annotation before record body"
      test_write_preserves_local_function_return_annotation_before_record_body;
    case
      "write parenthesizes annotated record expressions"
      test_write_parenthesizes_annotated_record_expressions;
    case
      "write parenthesizes annotated identifier expressions"
      test_write_parenthesizes_annotated_identifier_expressions;
    case "write preserves coercion expressions" test_write_preserves_coercion_expressions;
    case
      "write keeps nullary constructor pattern payloads atomic"
      test_write_keeps_nullary_constructor_pattern_payloads_atomic;
    case "write desugars function expressions" test_write_desugars_function_expressions;
    case
      "write desugars single-case function to fun pattern"
      test_write_desugars_single_case_function_to_fun_pattern;
    case
      "write parenthesizes single-case poly variant payload function"
      test_write_parenthesizes_single_case_poly_variant_payload_function;
    case
      "write desugars function application arguments"
      test_write_desugars_function_application_arguments;
    case
      "write preserves labeled parameter order around wildcards"
      test_write_preserves_labeled_parameter_order_around_wildcards;
    case
      "write preserves labeled binding return annotation"
      test_write_preserves_labeled_binding_return_annotation;
    case "write breaks long let binding parameters" test_write_breaks_long_let_binding_parameters;
    case "write preserves assignment operators" test_write_preserves_assignment_operators;
    case
      "write preserves labeled wildcard function parameters"
      test_write_preserves_labeled_wildcard_function_parameters;
    case
      "write preserves renamed labeled function parameters"
      test_write_preserves_renamed_labeled_function_parameters;
    case
      "write preserves let binding annotations exactly once"
      test_write_preserves_let_binding_annotations_exactly_once;
    case
      "write preserves comments before else tokens"
      test_write_preserves_comments_before_else_tokens;
    case
      "write preserves EOF-owned trailing comments"
      test_write_preserves_eof_owned_trailing_comments;
    case
      "write keeps multiline ordinary comments idempotent"
      test_write_keeps_multiline_ordinary_comments_idempotent;
    case
      "write preserves shallow ordinary comment indentation"
      test_write_preserves_shallow_ordinary_comment_indentation;
    case
      "write normalizes multiline ordinary comment continuation indentation"
      test_write_normalizes_multiline_ordinary_comment_continuation_indentation;
    case
      "format keeps constructor record update arguments idempotent"
      test_format_keeps_constructor_record_update_arguments_idempotent;
    case
      "write renders parenthesized type constructor arguments"
      test_write_renders_parenthesized_type_constructor_arguments;
    case "write renders operator value declarations" test_write_renders_operator_value_declarations;
    case "write renders include declarations" test_write_renders_include_declarations;
    case "write renders adjacent opens tightly" test_write_renders_adjacent_opens_tightly;
    case "write renders record updates" test_write_renders_record_updates;
    case "write renders coercion expressions" test_write_renders_coercion_expressions;
    case "write renders local-open patterns" test_write_renders_local_open_patterns;
    case
      "write renders operator bindings and local-open operator values"
      test_write_renders_operator_bindings_and_local_open_operator_values;
    case
      "write renders polymorphic variants with qualified record payloads"
      test_write_renders_polymorphic_variants_with_qualified_record_payloads;
    case
      "write renders binding operator expressions"
      test_write_renders_binding_operator_expressions;
    case "write keeps @@ fun applications bare" test_write_keeps_fun_applications_bare;
    case "write keeps pipeline fun operands bare" test_write_keeps_pipeline_fun_operands_bare;
    case "write parenthesizes tuple function bodies" test_write_parenthesizes_tuple_function_bodies;
    case
      "write parenthesizes tuple let bodies inside function arguments"
      test_write_parenthesizes_tuple_let_bodies_inside_function_arguments;
    case "write parenthesizes tuple if branches" test_write_parenthesizes_tuple_if_branches;
    case
      "write keeps infix application operands bare when precedence allows it"
      test_write_keeps_infix_application_operands_bare_when_precedence_allows_it;
    case
      "write keeps low-precedence right infix operands bare"
      test_write_keeps_low_precedence_right_infix_operands_bare;
    case
      "write keeps application on the right side of exponent infix"
      test_write_keeps_application_on_the_right_side_of_exponent_infix;
    case
      "write keeps bare polymorphic variant application arguments split"
      test_write_keeps_bare_polymorphic_variant_application_arguments_split;
    case
      "write keeps field access bound to polyvariant payloads"
      test_write_keeps_field_access_bound_to_polyvariant_payloads;
    case
      "write keeps qualified record field accesses"
      test_write_keeps_qualified_record_field_accesses;
    case
      "write keeps qualified record fields in patterns"
      test_write_keeps_qualified_record_fields_in_patterns;
    case "write keeps cons chains right associative" test_write_keeps_cons_chains_right_associative;
    case
      "write keeps infix precedence parens minimal"
      test_write_keeps_infix_precedence_parens_minimal;
    case
      "write parenthesizes tuple sequence operands"
      test_write_parenthesizes_tuple_sequence_operands;
    case
      "write trims pending spaces before record field docstrings"
      test_write_trims_pending_spaces_before_record_field_docstrings;
    case
      "write parenthesizes list tuple items with match components"
      test_write_parenthesizes_list_tuple_items_with_match_components;
    case
      "write parenthesizes keyword body sequences"
      test_write_parenthesizes_keyword_body_sequences;
    case
      "write keeps paired parenthesized if branches"
      test_write_keeps_paired_parenthesized_if_branches;
    case
      "write keeps parenthesized then blocks attached"
      test_write_keeps_parenthesized_then_blocks_attached;
    case
      "write keeps match case keyword body sequences unwrapped"
      test_write_keeps_match_case_keyword_body_sequences_unwrapped;
    case "write renders loop expressions" test_write_renders_loop_expressions;
    case
      "write normalizes trailing sequence semicolons"
      test_write_normalizes_trailing_sequence_semicolons;
    case
      "write keeps local let in on inline bindings"
      test_write_keeps_local_let_in_on_inline_bindings;
    case
      "write breaks multiline match application arguments"
      test_write_breaks_multiline_match_application_arguments;
    case
      "write breaks parenthesized infix arguments containing matches"
      test_write_breaks_parenthesized_infix_arguments_containing_matches;
    case "write renders assert expressions" test_write_renders_assert_expressions;
    case "write renders local exception expressions" test_write_renders_local_exception_expressions;
    case
      "write renders attribute and extension expressions"
      test_write_renders_attribute_and_extension_expressions;
    case
      "write preserves structure item attribute suffixes"
      test_write_preserves_structure_item_attribute_suffixes;
    case
      "write preserves signature item attribute suffixes"
      test_write_preserves_signature_item_attribute_suffixes;
    case "write renders let module expressions" test_write_renders_let_module_expressions;
    case
      "write renders first-class module expressions"
      test_write_renders_first_class_module_expressions;
    case
      "write renders first-class module packs and unpacks with constraints"
      test_write_renders_first_class_module_packs_and_unpacks_with_constraints;
    case
      "write renders GADT inline record constructors"
      test_write_renders_gadt_inline_record_constructors;
    case
      "write renders first-class module type aliases"
      test_write_renders_first_class_module_type_aliases;
    case "write renders type extension declarations" test_write_renders_type_extension_declarations;
    case
      "write renders extensible and private type declarations"
      test_write_renders_extensible_and_private_type_declarations;
    case "write renders module type-of declarations" test_write_renders_module_type_of_declarations;
    case
      "write renders constrained module declarations"
      test_write_renders_constrained_module_declarations;
    case
      "write preserves chained module type constraint connectors"
      test_write_preserves_chained_module_type_constraint_connectors;
    case
      "write renders applied and ascribed module declarations"
      test_write_renders_applied_and_ascribed_module_declarations;
    case
      "write renders polymorphic variant type aliases"
      test_write_renders_polymorphic_variant_type_aliases;
    case
      "write preserves leading bar polymorphic variant types"
      test_write_preserves_leading_bar_polymorphic_variant_types;
    case
      "write preserves include module type of declarations"
      test_write_preserves_include_module_type_of_declarations;
    case
      "write renders external and exception declarations"
      test_write_renders_external_and_exception_declarations;
    case
      "write preserves semantically meaningful type tuple parentheses"
      test_write_preserves_semantically_meaningful_type_tuple_parentheses;
    case
      "write preserves arrow tuple payloads in signatures"
      test_write_preserves_arrow_tuple_payloads_in_signatures;
    case
      "write renders item attributes attached to declarations"
      test_write_renders_item_attributes_attached_to_declarations;
    case
      "write preserves abstract type declaration attributes"
      test_write_preserves_abstract_type_declaration_attributes;
    case
      "write preserves variant representation attributes"
      test_write_preserves_variant_representation_attributes;
    case
      "stream formatter renders polymorphic variant type bodies"
      test_stream_formatter_renders_polymorphic_variant_type_bodies;
    case
      "stream formatter keeps closed polymorphic variant aliases idempotent"
      test_stream_formatter_keeps_closed_polymorphic_variant_aliases_idempotent;
    case
      "write matches format for generated table records"
      test_write_matches_format_for_generated_table_records;
    case "write matches format for try expressions" test_write_matches_format_for_try_expressions;
    case
      "write matches format for lazy exception and interval patterns"
      test_write_matches_format_for_lazy_exception_and_interval_patterns;
    case
      "write keeps or-pattern alternatives vertical"
      test_write_keeps_or_pattern_alternatives_vertical;
    case "write formats polymorphic variants" test_write_formats_polymorphic_variants;
    case
      "format keeps explicit fun rhs bindings explicit"
      test_format_keeps_explicit_fun_rhs_bindings_explicit;
    case
      "format inlines short explicit fun rhs with qualified apply bodies"
      test_format_inlines_short_explicit_fun_rhs_with_qualified_apply_bodies;
    case
      "format breaks explicit fun rhs after arrow when body exceeds width"
      test_format_breaks_explicit_fun_rhs_after_arrow_when_body_exceeds_width;
    case
      "format breaks function binding body after equals when body exceeds width"
      test_format_breaks_function_binding_body_after_equals_when_body_exceeds_width;
    case
      "format keeps long list rhs opener after equals"
      test_format_keeps_long_list_rhs_opener_after_equals;
    case
      "format keeps long array rhs opener after equals"
      test_format_keeps_long_array_rhs_opener_after_equals;
    case
      "format keeps long record rhs opener after equals"
      test_format_keeps_long_record_rhs_opener_after_equals;
    case
      "format keeps multiline explicit fun rhs with local module bodies"
      test_format_keeps_multiline_explicit_fun_rhs_with_local_module_bodies;
    case
      "format keeps structural let module bodies with nested lets"
      test_format_keeps_structural_let_module_bodies_with_nested_lets;
    case
      "format renders fun body trivia from token-leading trivia"
      test_format_renders_fun_body_trivia_from_token_leading_trivia;
    case
      "format renders if-branch trivia from token-leading trivia"
      test_format_renders_if_branch_trivia_from_token_leading_trivia;
    case
      "format renders let rhs and body trivia from token-leading trivia"
      test_format_renders_let_rhs_and_body_trivia_from_token_leading_trivia;
    case
      "format renders sequence and let-operator trivia from tokens"
      test_format_renders_sequence_and_let_operator_trivia_from_tokens;
    case
      "format match cases from structure, not arrow source newlines"
      test_format_match_cases_from_structure_not_arrow_source_newlines;
    case
      "format preserves leading comments on match cases"
      test_format_preserves_leading_comments_on_match_cases;
    case
      "write breaks nested constructor match patterns"
      test_write_breaks_nested_constructor_match_patterns;
    case
      "write breaks after multiline constructor record match patterns"
      test_write_breaks_after_multiline_constructor_record_match_patterns;
    case "write breaks deep record list patterns" test_write_breaks_deep_record_list_patterns;
    case
      "write keeps small record list patterns inline"
      test_write_keeps_small_record_list_patterns_inline;
    case
      "write aligns multiline list pattern closing delimiter"
      test_write_aligns_multiline_list_pattern_closing_delimiter;
    case
      "write aligns constructor record pattern payloads"
      test_write_aligns_constructor_record_pattern_payloads;
    case
      "write keeps fitting constructor or patterns inline"
      test_write_keeps_fitting_constructor_or_patterns_inline;
    case
      "write keeps fitting or-pattern fun parameters inline"
      test_write_keeps_fitting_or_pattern_fun_parameters_inline;
    case
      "write keeps parenthesized match case bodies attached to arrows"
      test_write_keeps_parenthesized_match_case_bodies_attached_to_arrows;
    case
      "write parenthesizes tuple match case bodies"
      test_write_parenthesizes_tuple_match_case_bodies;
    case
      "write keeps commented tuple match case bodies safe"
      test_write_keeps_commented_tuple_match_case_bodies_safe;
    case
      "write parenthesizes tuple cases inside parenthesized matches"
      test_write_parenthesizes_tuple_cases_inside_parenthesized_matches;
    case
      "write parenthesizes tuple cases inside delimited let matches"
      test_write_parenthesizes_tuple_cases_inside_delimited_let_matches;
    case
      "write preserves nested tuple expression values"
      test_write_preserves_nested_tuple_expression_values;
    case
      "write parenthesizes tuple scrutinees and tuple patterns"
      test_write_parenthesizes_tuple_scrutinees_and_tuple_patterns;
    case
      "write breaks tuple patterns before match arrows"
      test_write_breaks_tuple_patterns_before_match_arrows;
    case
      "write preserves tuple pattern application payloads"
      test_write_preserves_tuple_pattern_application_payloads;
    case
      "write preserves comments before local and bindings"
      test_write_preserves_comments_before_local_and_bindings;
    case
      "format polymorphic variant heads from explicit tag tokens"
      test_format_polymorphic_variant_heads_from_explicit_tag_tokens;
    case
      "format quoted core type variables from explicit sigil tokens"
      test_format_quoted_core_type_variables_from_explicit_sigil_tokens;
    case
      "format core type alias binders from explicit sigil tokens"
      test_format_core_type_alias_binders_from_explicit_sigil_tokens;
    case
      "format record fields break for compound field types, not field-name length"
      test_format_record_fields_break_for_compound_field_types_not_field_name_length;
    case
      "format record fields preserves field attributes"
      test_format_record_fields_preserves_field_attributes;
    case
      "format keeps prefix minus separated from nested prefix expressions"
      test_format_keeps_prefix_minus_separated_from_nested_prefix_expressions;
    case
      "write preserves nested prefix operator tokens"
      test_write_preserves_nested_prefix_operator_tokens;
    case
      "format keeps curried nullary constructor fun parameters separate"
      test_format_keeps_curried_nullary_constructor_fun_parameters_separate;
    case
      "write preserves nullary constructor patterns"
      test_write_preserves_nullary_constructor_patterns;
    case
      "write preserves nested constructor patterns"
      test_write_preserves_nested_constructor_patterns;
    case
      "write preserves defaulted optional parameters after renamed labels"
      test_write_preserves_defaulted_optional_parameters_after_renamed_labels;
    case
      "write preserves labeled parameters after renamed label"
      test_write_preserves_labeled_parameters_after_renamed_label;
    case
      "write preserves return annotated fun parameters"
      test_write_preserves_return_annotated_fun_parameters;
    case
      "desugar typed named parameters without duplicating inner annotations"
      test_desugar_typed_named_parameters_without_duplicating_inner_annotations;
    case
      "keep typed parameters in the binding header when annotation synthesis declines"
      test_keep_typed_parameters_in_the_binding_header_when_annotation_synthesis_declines;
    case
      "keep binding return type annotations loose after named parameters"
      test_keep_binding_return_type_annotations_loose_after_named_parameters;
    case
      "write keeps binding return type annotations loose after named parameters"
      test_write_keeps_binding_return_type_annotations_loose_after_named_parameters;
    case
      "format index expressions from explicit delimiter tokens"
      test_format_index_expressions_from_explicit_delimiter_tokens;
    case
      "write keeps index expressions bare as application arguments"
      test_write_keeps_index_expressions_bare_as_application_arguments;
    case
      "format signed literal patterns from structural sign tokens"
      test_format_signed_literal_patterns_from_structural_sign_tokens;
    case
      "write preserves signed literal constructor payload patterns"
      test_write_preserves_signed_literal_constructor_payload_patterns;
    case
      "write keeps record field wildcard values closed"
      test_write_keeps_record_field_wildcard_values_closed;
    case
      "format leaves a blank line before docstring-led top-level items"
      test_format_leaves_a_blank_line_before_docstring_led_top_level_items;
    case
      "format leaves a blank line before docstring-led signature items"
      test_format_leaves_a_blank_line_before_docstring_led_signature_items;
    case
      "format operator expressions and patterns from explicit operator tokens"
      test_format_operator_expressions_and_patterns_from_explicit_operator_tokens;
    case
      "format infix and prefix expression operators from explicit operator tokens"
      test_format_infix_and_prefix_expression_operators_from_explicit_operator_tokens;
    case
      "format singleton list patterns with explicit formatter spacing"
      test_format_singleton_list_patterns_with_explicit_formatter_spacing;
    case
      "format if conditions from infix structure, not token scans"
      test_format_if_conditions_from_infix_structure_not_token_scans;
    case
      "format binding values from structure, not source newlines"
      test_format_binding_values_from_structure_not_source_newlines;
    case
      "format simple string bindings inline from ordinary simplicity checks"
      test_format_simple_string_bindings_inline_from_ordinary_simplicity_checks;
    case
      "format keeps simple applies inline even when identifiers contain keywords"
      test_format_keeps_simple_applies_inline_even_when_identifiers_contain_keywords;
    case
      "format normalizes simple applies from structure, not source newlines"
      test_format_normalizes_simple_applies_from_structure_not_source_newlines;
    case
      "format rewrites parameterized let bindings between formatted lets"
      test_format_rewrites_parameterized_let_bindings_between_formatted_lets;
    case
      "format keeps mixed trivia and unsupported items parseable"
      test_format_keeps_mixed_trivia_and_unsupported_items_parseable;
    case
      "format keeps tuple/list/array docs idempotent"
      test_format_keeps_tuple_list_array_docs_idempotent;
    case
      "write always parenthesizes tuple expressions and patterns"
      test_write_always_parenthesizes_tuple_expressions_and_patterns;
    case
      "write keeps small array expressions inline"
      test_write_keeps_small_array_expressions_inline;
    case
      "format canonicalizes multiline list apply arguments"
      test_format_canonicalizes_multiline_list_apply_arguments;
    case
      "write breaks let RHS before calls with multiline list arguments"
      test_write_breaks_let_rhs_before_calls_with_multiline_list_arguments;
    case
      "format normalizes let-open bodies from structure, not source newlines"
      test_format_normalizes_let_open_bodies_from_structure_not_source_newlines;
    case
      "format aligns multiline let-open bodies with the let-open expression"
      test_format_aligns_multiline_let_open_bodies_with_the_let_open_expression;
    case
      "format open bang from explicit bang tokens in ml and mli"
      test_format_open_bang_from_explicit_bang_tokens_in_ml_and_mli;
    case
      "format local binding equals policy for boolean chains and pipelines"
      test_format_local_binding_equals_policy_for_boolean_chains_and_pipelines;
    case
      "format breaks long pipeline rhs after equals"
      test_format_breaks_long_pipeline_rhs_after_equals;
    case
      "format breaks local let rhs when trailing in would exceed width"
      test_format_breaks_local_let_rhs_when_trailing_in_would_exceed_width;
    case
      "format breaks let rhs after equals then retries constructor record payload"
      test_format_breaks_let_rhs_after_equals_then_retries_constructor_record_payload;
    case
      "format parenthesized pipeline arguments vertically"
      test_format_parenthesized_pipeline_arguments_vertically;
    case
      "format local binding infix threshold around inline-after-equals cutoff"
      test_format_local_binding_infix_threshold_around_inline_after_equals_cutoff;
    case
      "format simple apply rhs by shape, not comment scans"
      test_format_simple_apply_rhs_by_shape_not_comment_scans;
    case
      "format binding-operator equals policy with explicit fun and multiline values"
      test_format_binding_operator_equals_policy_with_explicit_fun_and_multiline_values;
    case
      "format recursive local bindings with multiline bodies"
      test_format_recursive_local_bindings_with_multiline_bodies;
    case
      "format breaks tuples that exceed formatter width"
      test_format_breaks_tuples_that_exceed_formatter_width;
    case
      "verify treats stream formatter rewrites as safe"
      test_verify_treats_stream_formatter_rewrites_as_safe;
    case
      "syntax hash normalizes commented pipeline parens"
      test_syntax_hash_normalizes_commented_pipeline_parens;
    case "syntax hash normalizes tuple edge parens" test_syntax_hash_normalizes_tuple_edge_parens;
    case
      "syntax hash normalizes leading variant pipes"
      test_syntax_hash_normalizes_leading_variant_pipes;
    case
      "syntax hash normalizes record pattern trailing semis"
      test_syntax_hash_normalizes_record_pattern_trailing_semis;
    case
      "syntax hash normalizes list pattern trailing semis"
      test_syntax_hash_normalizes_list_pattern_trailing_semis;
    case
      "syntax hash normalizes constructor pattern parens"
      test_syntax_hash_normalizes_constructor_pattern_parens;
    case
      "syntax hash normalizes function desugaring"
      test_syntax_hash_normalizes_function_desugaring;
    case
      "syntax hash normalizes trailing sequence semis"
      test_syntax_hash_normalizes_trailing_sequence_semis;
    case
      "syntax hash normalizes trivia line indentation"
      test_syntax_hash_normalizes_trivia_line_indentation;
    case
      "format keeps function and match rendering idempotent"
      test_format_keeps_function_and_match_rendering_idempotent;
    case
      "format keeps let/if/sequence layouts idempotent"
      test_format_keeps_let_if_sequence_layouts_idempotent;
    case
      "format keeps typed and labeled bindings idempotent"
      test_format_keeps_typed_and_labeled_bindings_idempotent;
    case
      "format keeps labeled infix arguments singly parenthesized"
      test_format_keeps_labeled_infix_arguments_singly_parenthesized;
    case
      "format keeps structural named parameters with defaults idempotent"
      test_format_keeps_structural_named_parameters_with_defaults_idempotent;
    case
      "format keeps signature operator values structural"
      test_format_keeps_signature_operator_values_structural;
    case "format keeps alias patterns idempotent" test_format_keeps_alias_patterns_idempotent;
    case
      "format keeps constructor parameter patterns idempotent"
      test_format_keeps_constructor_parameter_patterns_idempotent;
    case
      "format keeps typed first-class module patterns idempotent"
      test_format_keeps_typed_first_class_module_patterns_idempotent;
    case
      "format keeps first-class module expressions idempotent"
      test_format_keeps_first_class_module_expressions_idempotent;
    case
      "format keeps structural signature items idempotent"
      test_format_keeps_structural_signature_items_idempotent;
    case
      "format floating attributes from structural payload items"
      test_format_floating_attributes_from_structural_payload_items;
    case
      "format floating extension items structurally"
      test_format_floating_extension_items_structurally;
    case
      "format preserves scientific float exponents without introducing spaces"
      test_format_preserves_scientific_float_exponents_without_introducing_spaces;
    case
      "format module-expression and module-type extensions structurally"
      test_format_module_expression_and_module_type_extensions_structurally;
    case
      "format structural core type token fallbacks"
      test_format_structural_core_type_token_fallbacks;
    case
      "format first-class module types from structural module-type docs"
      test_format_first_class_module_types_from_structural_module_type_docs;
    case
      "format shared core-type attributes keeps opaque payload tokens"
      test_format_shared_core_type_attributes_keeps_opaque_payload_tokens;
    case
      "format shared attribute payload infix expressions opaquely"
      test_format_shared_attribute_payload_infix_expressions_opaquely;
    case
      "format expression attributes keeps opaque payload tokens"
      test_format_expression_attributes_keeps_opaque_payload_tokens;
    case
      "format ordinary pattern-payload attributes structurally"
      test_format_ordinary_pattern_payload_attributes_structurally;
    case
      "format parenthesizes attributed non-atomic expressions"
      test_format_parenthesizes_attributed_non_atomic_expressions;
    case
      "format currently fails for plain object expressions"
      test_format_currently_fails_for_plain_object_expressions;
    case
      "format currently fails for object core types"
      test_format_currently_fails_for_object_core_types;
    case
      "format object bodies preserve terminal trivia"
      test_format_object_bodies_preserve_terminal_trivia;
    case
      "format object extension members structurally"
      test_format_object_extension_members_structurally;
    case
      "format trailing variant comments with explicit separator policy"
      test_format_trailing_variant_comments_with_explicit_separator_policy;
    case
      "format trailing variant docstrings with explicit separator policy"
      test_format_trailing_variant_docstrings_with_explicit_separator_policy;
    case
      "format fails for signature-bodied first-class module types"
      test_format_fails_for_signature_bodied_first_class_module_types;
    case "format core-type extensions structurally" test_format_core_type_extensions_structurally;
    case
      "format keeps structural patterns idempotent"
      test_format_keeps_structural_patterns_idempotent;
    case
      "format keeps polymorphic-variant inherit patterns idempotent"
      test_format_keeps_polymorphic_variant_inherit_patterns_idempotent;
    case
      "format typed first-class-module patterns structurally"
      test_format_typed_first_class_module_patterns_structurally;
    case "format pattern extensions structurally" test_format_pattern_extensions_structurally;
    case
      "format keeps structural imperative and module expressions idempotent"
      test_format_keeps_structural_imperative_and_module_expressions_idempotent;
    case "format object override expressions" test_format_object_override_expressions;
    case "format expression extensions structurally" test_format_expression_extensions_structurally;
    case
      "format atomic.loc extension keeps qualified name"
      test_format_atomic_loc_extension_keeps_qualified_name;
    case
      "format unreachable expressions structurally"
      test_format_unreachable_expressions_structurally;
    case
      "format keeps typed and polymorphic expressions structural"
      test_format_keeps_typed_and_polymorphic_expressions_structural;
    case
      "format keeps nested module bodies structural"
      test_format_keeps_nested_module_bodies_structural;
    case
      "format keeps grouped GADT type declarations structural"
      test_format_keeps_grouped_gadt_type_declarations_structural;
    case
      "format inline record constructors from structure, not source newlines"
      test_format_inline_record_constructors_from_structure_not_source_newlines;
    case
      "format keeps boolean if conditions with matches idempotent"
      test_format_keeps_boolean_if_conditions_with_matches_idempotent;
    case
      "format keeps simple nested match case bodies idempotent"
      test_format_keeps_simple_nested_match_case_bodies_idempotent;
    case
      "format keeps top-level fun phrases separated"
      test_format_keeps_top_level_fun_phrases_separated;
    case
      "format keeps top-level phrase separators structural"
      test_format_keeps_top_level_phrase_separators_structural;
    case
      "format preserves syntax hash for selected codebase files"
      test_format_preserves_syntax_hash_for_selected_codebase_files;
    case "runner skips hidden and build directories" test_runner_skips_hidden_and_build_directories;
    case
      "runner skips ignored subtrees during collection"
      test_runner_skips_ignored_subtrees_during_collection;
    case
      "runner reports formatting status and emits json events"
      test_runner_reports_formatting_status_and_emits_json_events;
    case
      "verify reports files that would reformat safely"
      test_verify_reports_files_that_would_reformat_safely;
    case
      "format rewrites files in place and reports formatted status"
      test_format_rewrites_files_in_place_and_reports_formatted_status;
    case
      "json file events include structured diagnostics for parse failures"
      test_json_file_events_include_structured_diagnostics_for_parse_failures;
    case "streaming runner skips ignored files" test_streaming_runner_skips_ignored_files;
    case
      "streaming runner scans roots and streams file results"
      test_streaming_runner_scans_roots_and_streams_file_results;
  ]

let main ~args:_ = Test.Cli.main ~name:"krasny:format" ~tests ~args:Env.args ()

let () = Runtime.run ~main ~args:Env.args ()
