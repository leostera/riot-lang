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

let tests = [
  Test.case
    "format returns the original source for a simple implementation"
    (fun _ctx ->
      let source = "let x = 1 + 2\n" in
      let actual =
        parse_ml source
        |> Krasny.format
        |> Result.expect ~msg:"simple implementations should format"
      in
      Test.assert_equal ~expected:source ~actual;
      Ok ());
  Test.case
    "format_source uses the public krasny parse facade"
    (fun _ctx ->
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
      Ok ());
  Test.case
    "syntax hash normalizes formatter-safe punctuation"
    (fun _ctx ->
      let expected =
        parse_ml {ocaml|type row={x:int;y:int}
let check=pos+pattern_len>String.length str
|ocaml}
        |> Krasny.syntax_hash
      in
      let actual =
        parse_ml
          {ocaml|type row={x:int;y:int;}
let check=(pos+pattern_len)>(String.length str)
|ocaml}
        |> Krasny.syntax_hash
      in
      Test.assert_equal ~expected ~actual;
      Ok ());
  Test.case
    "format adds a final newline to non-empty output"
    (fun _ctx ->
      let source = "let x = 1 + 2" in
      let actual =
        parse_ml source
        |> Krasny.format
        |> Result.expect ~msg:"formatted output should end with a final newline"
      in
      Test.assert_equal ~expected:"let x = 1 + 2\n" ~actual;
      Ok ());
  Test.case
    "format keeps empty files empty"
    (fun _ctx ->
      let actual =
        parse_ml ""
        |> Krasny.format
        |> Result.expect ~msg:"empty files should still format"
      in
      Test.assert_equal ~expected:"" ~actual;
      Ok ());
  Test.case
    "write renders formatted output into the supplied writer"
    (fun _ctx ->
      let source = "let x = 1 + 2\n" in
      let parsed = parse_ml source in
      let expected =
        Krasny.format parsed
        |> Result.expect ~msg:"format should render the same source"
      in
      let actual = capture_write parsed in
      Test.assert_equal ~expected ~actual;
      Ok ());
  Test.case
    "write renders simple interfaces"
    (fun ctx ->
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
|ocaml});
  Test.case
    "write normalizes multiline docstring indentation"
    (fun ctx ->
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
|ocaml});
  Test.case
    "write preserves relative indentation inside docstrings"
    (fun ctx ->
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
|ocaml});
  Test.case
    "write collapses blank lines before leading comments"
    (fun ctx ->
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
|ocaml});
  Test.case
    "format keeps signature docstring spacing idempotent"
    (fun ctx ->
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
|ocaml});
  Test.case
    "write preserves terminal docstrings before nested signature end"
    (fun ctx ->
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
|ocaml});
  Test.case
    "write preserves module functor parameters"
    (fun ctx ->
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
|ocaml});
  Test.case
    "write keeps adjacent module structures separated"
    (fun ctx ->
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
|ocaml});
  Test.case
    "write preserves variant constructor docstrings in interfaces"
    (fun ctx ->
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
|ocaml});
  Test.case
    "write renders short record types inline"
    (fun ctx ->
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
|ocaml});
  Test.case
    "write preserves terminal record field docstrings"
    (fun ctx ->
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
|ocaml});
  Test.case
    "write renders type alias record representations"
    (fun ctx ->
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
|ocaml});
  Test.case
    "write breaks record type aliases when width is exceeded"
    (fun ctx ->
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
|ocaml});
  Test.case
    "write preserves assignment operators"
    (fun ctx ->
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
|ocaml});
  Test.case
    "write preserves labeled wildcard function parameters"
    (fun ctx ->
      let source = {ocaml|let run=fun ~args:_->()
|ocaml}
      in
      let parsed = parse_ml source in
      let actual = capture_write parsed in
      Test.Snapshot.assert_inline_text
        ~ctx
        ~actual
        ~expected:{ocaml|let run = fun ~args:_ -> ()
|ocaml});
  Test.case
    "write preserves renamed labeled function parameters"
    (fun ctx ->
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
|ocaml});
  Test.case
    "write preserves let binding annotations exactly once"
    (fun ctx ->
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
|ocaml});
  Test.case
    "write preserves comments before else tokens"
    (fun ctx ->
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
|ocaml});
  Test.case
    "write preserves EOF-owned trailing comments"
    (fun ctx ->
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
|ocaml});
  Test.case
    "write keeps multiline ordinary comments idempotent"
    (fun ctx ->
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
|ocaml});
  Test.case
    "write preserves shallow ordinary comment indentation"
    (fun ctx ->
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
|ocaml});
  Test.case
    "write normalizes multiline ordinary comment continuation indentation"
    (fun ctx ->
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
|ocaml});
  Test.case
    "format keeps constructor record update arguments idempotent"
    (fun ctx ->
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
|ocaml});
  Test.case
    "write renders parenthesized type constructor arguments"
    (fun ctx ->
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
|ocaml});
  Test.case
    "write renders operator value declarations"
    (fun ctx ->
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
|ocaml});
  Test.case
    "write renders include declarations"
    (fun ctx ->
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
|ocaml});
  Test.case
    "write renders adjacent opens tightly"
    (fun ctx ->
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
|ocaml});
  Test.case
    "write renders record updates"
    (fun ctx ->
      let source = {ocaml|let next={state with count=state.count+1;ready=true}
|ocaml}
      in
      let parsed = parse_ml source in
      let actual = capture_write parsed in
      Test.Snapshot.assert_inline_text
        ~ctx
        ~actual
        ~expected:{ocaml|let next = { state with count = state.count + 1; ready = true }
|ocaml});
  Test.case
    "write renders coercion expressions"
    (fun ctx ->
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
|ocaml});
  Test.case
    "write renders local-open patterns"
    (fun ctx ->
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
|ocaml});
  Test.case
    "write renders operator bindings and local-open operator values"
    (fun ctx ->
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
|ocaml});
  Test.case
    "write renders polymorphic variants with qualified record payloads"
    (fun ctx ->
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
|ocaml});
  Test.case
    "write renders binding operator expressions"
    (fun ctx ->
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
|ocaml});
  Test.case
    "write keeps @@ fun applications bare"
    (fun ctx ->
      let source = {ocaml|let ()=start ~apps:[]@@fun ()->main ()
|ocaml}
      in
      let parsed = parse_ml source in
      let actual = capture_write parsed in
      Test.Snapshot.assert_inline_text
        ~ctx
        ~actual
        ~expected:{ocaml|let () = start ~apps:[] @@ fun () -> main ()
|ocaml});
  Test.case
    "write keeps pipeline fun operands bare"
    (fun ctx ->
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
|ocaml});
  Test.case
    "write parenthesizes tuple function bodies"
    (fun ctx ->
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
|ocaml});
  Test.case
    "write parenthesizes tuple let bodies inside function arguments"
    (fun ctx ->
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
|ocaml});
  Test.case
    "write parenthesizes tuple if branches"
    (fun ctx ->
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
|ocaml});
  Test.case
    "write keeps infix application operands bare when precedence allows it"
    (fun ctx ->
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
|ocaml});
  Test.case
    "write keeps low-precedence right infix operands bare"
    (fun ctx ->
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
|ocaml});
  Test.case
    "write keeps application on the right side of exponent infix"
    (fun ctx ->
      let source = {ocaml|let multiplier=10.0**Float.from_int precision
|ocaml}
      in
      let parsed = parse_ml source in
      let actual = capture_write parsed in
      Test.Snapshot.assert_inline_text
        ~ctx
        ~actual
        ~expected:{ocaml|let multiplier = 10.0 ** Float.from_int precision
|ocaml});
  Test.case
    "write keeps bare polymorphic variant application arguments split"
    (fun ctx ->
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
|ocaml});
  Test.case
    "write keeps cons chains right associative"
    (fun ctx ->
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
|ocaml});
  Test.case
    "write keeps infix precedence parens minimal"
    (fun ctx ->
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
|ocaml});
  Test.case
    "write parenthesizes tuple sequence operands"
    (fun ctx ->
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
|ocaml});
  Test.case
    "write trims pending spaces before record field docstrings"
    (fun ctx ->
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
|ocaml});
  Test.case
    "write parenthesizes list tuple items with match components"
    (fun ctx ->
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
|ocaml});
  Test.case
    "write parenthesizes keyword body sequences"
    (fun ctx ->
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
|ocaml});
  Test.case
    "write keeps match case keyword body sequences unwrapped"
    (fun ctx ->
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
|ocaml});
  Test.case
    "write renders loop expressions"
    (fun ctx ->
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
|ocaml});
  Test.case
    "write normalizes trailing sequence semicolons"
    (fun ctx ->
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
|ocaml});
  Test.case
    "write keeps local let in on inline bindings"
    (fun ctx ->
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
|ocaml});
  Test.case
    "write breaks multiline match application arguments"
    (fun ctx ->
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
|ocaml});
  Test.case
    "write breaks parenthesized infix arguments containing matches"
    (fun ctx ->
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
|ocaml});
  Test.case
    "write renders assert expressions"
    (fun ctx ->
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
|ocaml});
  Test.case
    "write renders local exception expressions"
    (fun ctx ->
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
|ocaml});
  Test.case
    "write renders attribute and extension expressions"
    (fun ctx ->
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
|ocaml});
  Test.case
    "write preserves structure item attribute suffixes"
    (fun ctx ->
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
|ocaml});
  Test.case
    "write preserves signature item attribute suffixes"
    (fun ctx ->
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
|ocaml});
  Test.case
    "write renders let module expressions"
    (fun ctx ->
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
|ocaml});
  Test.case
    "write renders first-class module expressions"
    (fun ctx ->
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
|ocaml});
  Test.case
    "write renders first-class module packs and unpacks with constraints"
    (fun ctx ->
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
|ocaml});
  Test.case
    "write renders GADT inline record constructors"
    (fun ctx ->
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
|ocaml});
  Test.case
    "write renders first-class module type aliases"
    (fun ctx ->
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
|ocaml});
  Test.case
    "write renders type extension declarations"
    (fun ctx ->
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
|ocaml});
  Test.case
    "write renders extensible and private type declarations"
    (fun ctx ->
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
|ocaml});
  Test.case
    "write renders module type-of declarations"
    (fun ctx ->
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
|ocaml});
  Test.case
    "write renders constrained module declarations"
    (fun ctx ->
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
|ocaml});
  Test.case
    "write preserves chained module type constraint connectors"
    (fun ctx ->
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
|ocaml});
  Test.case
    "write renders applied and ascribed module declarations"
    (fun ctx ->
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
|ocaml});
  Test.case
    "write renders polymorphic variant type aliases"
    (fun ctx ->
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
|ocaml});
  Test.case
    "write preserves leading bar polymorphic variant types"
    (fun ctx ->
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
|ocaml});
  Test.case
    "write preserves include module type of declarations"
    (fun ctx ->
      let source = {ocaml|include  module type of   Global
|ocaml}
      in
      let parsed = parse_mli source in
      let actual = capture_write parsed in
      Test.Snapshot.assert_inline_text
        ~ctx
        ~actual
        ~expected:{ocaml|include module type of Global
|ocaml});
  Test.case
    "write renders external and exception declarations"
    (fun ctx ->
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
|ocaml});
  Test.case
    "write renders item attributes attached to declarations"
    (fun ctx ->
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
|ocaml});
  Test.case
    "write preserves abstract type declaration attributes"
    (fun ctx ->
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
|ocaml});
  Test.case
    "write preserves variant representation attributes"
    (fun ctx ->
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
|ocaml});
  Test.case
    "stream formatter renders polymorphic variant type bodies"
    (fun ctx ->
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
|ocaml});
  Test.case
    "stream formatter keeps closed polymorphic variant aliases idempotent"
    (fun ctx ->
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
|ocaml});
  Test.case
    "write matches format for generated table records"
    (fun _ctx ->
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
      Ok ());
  Test.case
    "write matches format for try expressions"
    (fun _ctx ->
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
      Ok ());
  Test.case
    "write matches format for lazy exception and interval patterns"
    (fun ctx ->
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
        ~expected:{ocaml|let force = function
  | lazy value -> value

let recovered =
  match read () with
  | exception Failure -> 0
  | value -> value

let classify = function
  | 'a' .. 'z' -> 1
  | _ -> 0
|ocaml});
  Test.case
    "write keeps or-pattern alternatives vertical"
    (fun ctx ->
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
        ~expected:{ocaml|let is_newline = function
  | '\n'
  | '\r' -> true
  | _ -> false

let is_layout = function
  | ' '
  | '\t'
  | '\n'
  | '\r' -> true
  | _ -> false
|ocaml});
  Test.case
    "write formats polymorphic variants"
    (fun ctx ->
      let source = {ocaml|let classify = function | `Alpha -> `Seen | `Beta value -> value
|ocaml}
      in
      let parsed = parse_ml source in
      let actual = capture_write parsed in
      Test.Snapshot.assert_inline_text
        ~ctx
        ~actual
        ~expected:{ocaml|let classify = function
  | `Alpha -> `Seen
  | `Beta value -> value
|ocaml});
  Test.case
    "format keeps explicit fun rhs bindings explicit"
    (fun _ctx ->
      let source = "let id = fun x -> x\n" in
      let actual =
        parse_ml source
        |> Krasny.format
        |> Result.expect ~msg:"explicit fun rhs bindings should format"
      in
      Test.assert_equal ~expected:source ~actual;
      Ok ());
  Test.case
    "format inlines short explicit fun rhs with qualified apply bodies"
    (fun ctx ->
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
|ocaml});
  Test.case
    "format keeps multiline explicit fun rhs with local module bodies"
    (fun _ctx ->
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
      Ok ());
  Test.case
    "format keeps structural let module bodies with nested lets"
    (fun _ctx ->
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
      Ok ());
  Test.case
    "format renders fun body trivia from token-leading trivia"
    (fun ctx ->
      let source =
        {|let with_comment = fun x -> (* keep *) x
let with_doc = fun x -> (** keep *) x
|}
      in
      assert_formatted_ml_snapshot
        ~ctx
        ~msg:"fun-body comment and docstring trivia should not need source reparsing"
        source);
  Test.case
    "format renders if-branch trivia from token-leading trivia"
    (fun _ctx ->
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
      Ok ());
  Test.case
    "format renders let rhs and body trivia from token-leading trivia"
    (fun ctx ->
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
        source);
  Test.case
    "format renders sequence and let-operator trivia from tokens"
    (fun ctx ->
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
        source);
  Test.case
    "format match cases from structure, not arrow source newlines"
    (fun ctx ->
      let source = {|let render = function
  | A ->
      value
|}
      in
      assert_formatted_ml_snapshot
        ~ctx
        ~msg:"match case layout should not preserve source newlines after arrows"
        source);
  Test.case
    "write keeps parenthesized match case bodies attached to arrows"
    (fun ctx ->
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
|ocaml});
  Test.case
    "write parenthesizes tuple match case bodies"
    (fun ctx ->
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
|ocaml});
  Test.case
    "write keeps commented tuple match case bodies safe"
    (fun ctx ->
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
|ocaml});
  Test.case
    "write parenthesizes tuple cases inside parenthesized matches"
    (fun ctx ->
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
|ocaml});
  Test.case
    "write parenthesizes tuple cases inside delimited let matches"
    (fun ctx ->
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
|ocaml});
  Test.case
    "write preserves nested tuple expression values"
    (fun ctx ->
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
|ocaml});
  Test.case
    "write parenthesizes tuple scrutinees and tuple patterns"
    (fun ctx ->
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
|ocaml});
  Test.case
    "write preserves tuple pattern application payloads"
    (fun ctx ->
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
|ocaml});
  Test.case
    "write preserves comments before local and bindings"
    (fun ctx ->
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
|ocaml});
  Test.case
    "format polymorphic variant heads from explicit tag tokens"
    (fun ctx ->
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
        source);
  Test.case
    "format quoted core type variables from explicit sigil tokens"
    (fun ctx ->
      let source = {|type 'a t = 'a list

val id : 'a -> 'a
|}
      in
      assert_formatted_mli_snapshot
        ~ctx
        ~msg:"quoted core type variables should format from sigil and name tokens"
        source);
  Test.case
    "format core type alias binders from explicit sigil tokens"
    (fun _ctx ->
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
      Ok ());
  Test.case
    "format record fields break for compound field types, not field-name length"
    (fun ctx ->
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
|});
  Test.case
    "format record fields preserves field attributes"
    (fun _ctx ->
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
      Test.assert_equal
        ~expected:{|type 'value t = { mutable contents: 'value [@atomic] }
|}
        ~actual;
      assert_idempotent
        ~source:actual
        ~msg:"record field attributes should remain stable across repeated formatting";
      Ok ());
  Test.case
    "format keeps prefix minus separated from nested prefix expressions"
    (fun _ctx ->
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
      Ok ());
  Test.case
    "write preserves nested prefix operator tokens"
    (fun ctx ->
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
|ocaml});
  Test.case
    "format keeps curried nullary constructor fun parameters separate"
    (fun _ctx ->
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
      Ok ());
  Test.case
    "desugar typed named parameters without duplicating inner annotations"
    (fun ctx ->
      let source =
        {|type 'a t = 'a list

let map (type a b) (iter : a t) ~(fn : a -> b) : b t = failwith "todo"
|}
      in
      assert_formatted_ml_snapshot
        ~ctx
        ~msg:"typed named parameters should move to the synthesized outer annotation"
        source);
  Test.case
    "keep typed parameters in the binding header when annotation synthesis declines"
    (fun _ctx ->
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
      Ok ());
  Test.case
    "keep binding return type annotations loose after named parameters"
    (fun _ctx ->
      let source = {|type color

let make ~start ~finish ~steps : color array =
  steps
|}
      in
      let actual =
        parse_ml source
        |> Krasny.format
        |> Result.expect
          ~msg:"binding return-type annotations after named parameters should stay loose"
      in
      Test.assert_equal
        ~expected:{|type color

let make ~start ~finish ~steps : color array = steps
|}
        ~actual;
      Ok ());
  Test.case
    "write keeps binding return type annotations loose after named parameters"
    (fun ctx ->
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
|ocaml});
  Test.case
    "format index expressions from explicit delimiter tokens"
    (fun ctx ->
      let source = {|let x = s.[0]
let y = a.(0)
let z = x.%(0)
|}
      in
      assert_formatted_ml_snapshot
        ~ctx
        ~msg:"index expressions should format from CST-carried delimiters, not token replay"
        source);
  Test.case
    "write keeps index expressions bare as application arguments"
    (fun ctx ->
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
|ocaml});
  Test.case
    "format signed literal patterns from structural sign tokens"
    (fun ctx ->
      let source = {|let classify = function | -1 -> `Neg | +2 -> `Pos | _ -> `Other
|}
      in
      assert_formatted_ml_snapshot
        ~ctx
        ~msg:"signed literal patterns should format from CST-carried sign tokens"
        source);
  Test.case
    "write preserves signed literal constructor payload patterns"
    (fun ctx ->
      let source =
        {ocaml|let parse=function|Ok (Json.Int -123)->Ok ()|Ok (Json.Float -1.5)->Ok ()|_->Error "no"
|ocaml}
      in
      let parsed = parse_ml source in
      let actual = capture_write parsed in
      Test.Snapshot.assert_inline_text
        ~ctx
        ~actual
        ~expected:{ocaml|let parse = function
  | Ok (Json.Int -123) -> Ok ()
  | Ok (Json.Float -1.5) -> Ok ()
  | _ -> Error "no"
|ocaml});
  Test.case
    "write keeps record field wildcard values closed"
    (fun ctx ->
      let source = {ocaml|let show=function|Error {exception_=_;backtrace}->backtrace|_->""
|ocaml}
      in
      let parsed = parse_ml source in
      let actual = capture_write parsed in
      Test.Snapshot.assert_inline_text
        ~ctx
        ~actual
        ~expected:{ocaml|let show = function
  | Error { exception_ = _; backtrace } -> backtrace
  | _ -> ""
|ocaml});
  Test.case
    "format leaves a blank line before docstring-led top-level items"
    (fun _ctx ->
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
      Ok ());
  Test.case
    "format leaves a blank line before docstring-led signature items"
    (fun ctx ->
      let source = {|val first : int
(** doc for second *)
val second : int
|}
      in
      assert_formatted_mli_snapshot
        ~ctx
        ~msg:"signature docstring-led items should stay visually separated"
        source);
  Test.case
    "format operator expressions and patterns from explicit operator tokens"
    (fun ctx ->
      let source = {|let op = ( + )
let is_plus = function | ( + ) -> true | _ -> false
|}
      in
      assert_formatted_ml_snapshot
        ~ctx
        ~msg:"operator expressions and patterns should format from CST-carried operator tokens"
        source);
  Test.case
    "format infix and prefix expression operators from explicit operator tokens"
    (fun _ctx ->
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
      Ok ());
  Test.case
    "format singleton list patterns with explicit formatter spacing"
    (fun ctx ->
      let compact_source = {|let classify = function
  | [value] -> hit
|}
      in
      assert_formatted_ml_snapshot
        ~ctx
        ~msg:"singleton list patterns should not preserve compact source spacing"
        compact_source);
  Test.case
    "format if conditions from infix structure, not token scans"
    (fun _ctx ->
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
      Ok ());
  Test.case
    "format binding values from structure, not source newlines"
    (fun ctx ->
      let source = {|let wrapped =
  (
    value
  )
|}
      in
      assert_formatted_ml_snapshot
        ~ctx
        ~msg:"binding layout should not preserve multiline source for a simple wrapped value"
        source);
  Test.case
    "format simple string bindings inline from ordinary simplicity checks"
    (fun ctx ->
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
      assert_formatted_ml_snapshot ~ctx ~msg:"binding operators should always break after in" source);
  Test.case
    "format keeps simple applies inline even when identifiers contain keywords"
    (fun _ctx ->
      let source = "let handler = use function_handler\n" in
      let actual =
        parse_ml source
        |> Krasny.format
        |> Result.expect ~msg:"simple applies should not sniff keyword substrings"
      in
      Test.assert_equal ~expected:source ~actual;
      Ok ());
  Test.case
    "format normalizes simple applies from structure, not source newlines"
    (fun _ctx ->
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
      Ok ());
  Test.case
    "format rewrites parameterized let bindings between formatted lets"
    (fun ctx ->
      let source = "(* intro *)\nlet x = 1 + 2\nlet f x = x + 1\nlet y = 3 + 4\n" in
      assert_formatted_ml_snapshot
        ~ctx
        ~msg:"parameterized let bindings should lower through explicit fun syntax"
        source);
  Test.case
    "format keeps mixed trivia and unsupported items parseable"
    (fun _ctx ->
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
      Ok ());
  Test.case
    "format keeps tuple/list/array docs idempotent"
    (fun _ctx ->
      let source =
        {|let tuple_value = (left_side_identifier, right_side_identifier, final_identifier)
let list_value = [first_item_identifier; second_item_identifier; third_item_identifier]
let array_value = [|first_item_identifier; second_item_identifier; third_item_identifier|]
|}
      in
      assert_idempotent ~source ~msg:"collection expressions should stay stable";
      Ok ());
  Test.case
    "write always parenthesizes tuple expressions and patterns"
    (fun ctx ->
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

let consume = function
  | [] -> (Doc.empty, column)
  | M.(x, y) -> (x, y)
|ocaml});
  Test.case
    "write keeps small array expressions inline"
    (fun ctx ->
      let source = {ocaml|let array=[|1;2|]
|ocaml}
      in
      let actual =
        parse_ml source
        |> Krasny.format
        |> Result.expect ~msg:"small array should format"
      in
      Test.Snapshot.assert_inline_text ~ctx ~actual ~expected:{ocaml|let array = [|1; 2|]
|ocaml});
  Test.case
    "format canonicalizes multiline list apply arguments"
    (fun ctx ->
      let source = {|let cmd =
  f [
    first_item;
    second_item;
  ]
|}
      in
      assert_formatted_ml_snapshot ~ctx ~msg:"list arguments should format" source);
  Test.case
    "write breaks let RHS before calls with multiline list arguments"
    (fun ctx ->
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
|ocaml});
  Test.case
    "format normalizes let-open bodies from structure, not source newlines"
    (fun ctx ->
      let source = {|let answer =
  let open Option in
  value
|}
      in
      assert_formatted_ml_snapshot
        ~ctx
        ~msg:"let-open expressions should format structurally"
        source);
  Test.case
    "format aligns multiline let-open bodies with the let-open expression"
    (fun ctx ->
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
|ocaml});
  Test.case
    "format open bang from explicit bang tokens in ml and mli"
    (fun _ctx ->
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
      Ok ());
  Test.case
    "format local binding equals policy for boolean chains and pipelines"
    (fun ctx ->
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
        source);
  Test.case
    "format breaks long pipeline rhs after equals"
    (fun ctx ->
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
|ocaml});
  Test.case
    "format breaks local let rhs when trailing in would exceed width"
    (fun ctx ->
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
  let scopes = map_current t.scopes ~fn:(fun scope -> IdentMap.insert scope ~key:name ~value:scheme)
  in
  { scopes }
|ocaml});
  Test.case
    "format parenthesized pipeline arguments vertically"
    (fun ctx ->
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
|ocaml});
  Test.case
    "format local binding infix threshold around inline-after-equals cutoff"
    (fun ctx ->
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
        source);
  Test.case
    "format simple apply rhs by shape, not comment scans"
    (fun ctx ->
      let source = {ocaml|let run x=let value=f(* keep *)x in value
|ocaml}
      in
      let actual =
        parse_ml source
        |> Krasny.format
        |> Result.expect
          ~msg:"simple apply rhs layout should not depend on scanning raw token trivia"
      in
      Test.Snapshot.assert_inline_text
        ~ctx
        ~actual
        ~expected:{ocaml|let run x =
  let value = f
    (* keep *)
    x in
  value
|ocaml};
      assert_idempotent ~source ~msg:"comment-bearing simple apply rhs should stay stable";
      Ok ());
  Test.case
    "format binding-operator equals policy with explicit fun and multiline values"
    (fun ctx ->
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
        source);
  Test.case
    "format recursive local bindings with multiline bodies"
    (fun _ctx ->
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
      Ok ());
  Test.case
    "format breaks tuples that exceed formatter width"
    (fun ctx ->
      let source =
        {|let tuple_value = (left_side_identifier, right_side_identifier, final_identifier, fourth_identifier, fifth_identifier, sixth_identifier)
|}
      in
      assert_formatted_ml_snapshot ~ctx ~msg:"tuples should break from solver width" source);
  Test.case
    "verify treats stream formatter rewrites as safe"
    (fun _ctx ->
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
          Ok ()));
  Test.case
    "syntax hash normalizes commented pipeline parens"
    (fun _ctx ->
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
      Test.assert_equal
        ~expected:(Krasny.syntax_hash original)
        ~actual:(Krasny.syntax_hash with_parens);
      Ok ());
  Test.case
    "syntax hash normalizes tuple edge parens"
    (fun _ctx ->
      let parenthesized = parse_ml {ocaml|let pair=(left,right)
|ocaml}
      in
      let bare = parse_ml {ocaml|let pair=left,right
|ocaml}
      in
      Test.assert_equal
        ~expected:(Krasny.syntax_hash parenthesized)
        ~actual:(Krasny.syntax_hash bare);
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
      Test.assert_false
        (String.equal (Krasny.syntax_hash bare_triple) (Krasny.syntax_hash nested_pair));
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
      Ok ());
  Test.case
    "syntax hash normalizes leading variant pipes"
    (fun _ctx ->
      let without_first_pipe = parse_mli {ocaml|type role=Client|Server
|ocaml}
      in
      let with_first_pipe = parse_mli {ocaml|type role=|Client|Server
|ocaml}
      in
      Test.assert_equal
        ~expected:(Krasny.syntax_hash without_first_pipe)
        ~actual:(Krasny.syntax_hash with_first_pipe);
      Ok ());
  Test.case
    "syntax hash normalizes record pattern trailing semis"
    (fun _ctx ->
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
      Ok ());
  Test.case
    "syntax hash normalizes list pattern trailing semis"
    (fun _ctx ->
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
      Ok ());
  Test.case
    "syntax hash normalizes constructor pattern parens"
    (fun _ctx ->
      let parenthesized = parse_ml {ocaml|let sum=function|(Some value)::rest->value|_->0
|ocaml}
      in
      let bare = parse_ml {ocaml|let sum=function|Some value::rest->value|_->0
|ocaml}
      in
      Test.assert_equal
        ~expected:(Krasny.syntax_hash parenthesized)
        ~actual:(Krasny.syntax_hash bare);
      Ok ());
  Test.case
    "syntax hash normalizes trailing sequence semis"
    (fun _ctx ->
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
      Ok ());
  Test.case
    "syntax hash normalizes trivia line indentation"
    (fun _ctx ->
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
      Ok ());
  Test.case
    "format keeps function and match lowering idempotent"
    (fun _ctx ->
      let source =
        {|let f = function x, y -> x + y
let g = function 0 -> "zero" | _ -> "other"
let h = fun x -> match x with 0 -> "zero" | _ -> "other"
|}
      in
      assert_idempotent ~source ~msg:"function and match forms should stay stable";
      Ok ());
  Test.case
    "format keeps let/if/sequence layouts idempotent"
    (fun _ctx ->
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
      Ok ());
  Test.case
    "format keeps typed and labeled bindings idempotent"
    (fun _ctx ->
      let source =
        {|let delimiter_of_keyword : keyword -> delimiter option = function | Begin -> Some BeginEnd | _ -> None
let label_arg = f ~y
let optional_arg = f ?y
let optional_fun = fun ?(y = 0) -> y + 1
|}
      in
      assert_idempotent ~source ~msg:"typed/labeled forms should stay stable";
      Ok ());
  Test.case
    "format keeps labeled infix arguments singly parenthesized"
    (fun _ctx ->
      let source = "let next = foo ~pos:(pos + read)\n" in
      let actual =
        parse_ml source
        |> Krasny.format
        |> Result.expect ~msg:"labeled infix arguments should not gain redundant parentheses"
      in
      Test.assert_equal ~expected:source ~actual;
      Ok ());
  Test.case
    "format keeps structural named parameters with defaults idempotent"
    (fun _ctx ->
      let source =
        {|let configure ?(timeout : int = 30) ?retry:retries ~point:{ x; y } ~limit:seconds () =
  (timeout, retries, x, y, seconds)
|}
      in
      assert_idempotent
        ~source
        ~msg:"named parameter defaults, renames, and destructuring should format structurally";
      Ok ());
  Test.case
    "format keeps signature operator values structural"
    (fun _ctx ->
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
      Ok ());
  Test.case
    "format keeps alias patterns idempotent"
    (fun _ctx ->
      let source = {|open Std

let request = fun (Conn conn as c) () -> ()
|}
      in
      assert_idempotent ~source ~msg:"alias patterns should stay stable";
      Ok ());
  Test.case
    "format keeps constructor parameter patterns idempotent"
    (fun _ctx ->
      let source = {|open Std

let request = fun (Conn conn) () -> ()
|}
      in
      assert_idempotent
        ~source
        ~msg:"constructor parameter patterns should not gain extra parentheses";
      Ok ());
  Test.case
    "format keeps typed first-class module patterns idempotent"
    (fun _ctx ->
      let source =
        {|let run_comparison index (module R : Reporter.Intf.Intf) comp = (index, comp)
|}
      in
      assert_idempotent ~source ~msg:"typed first-class module patterns should lower structurally";
      Ok ());
  Test.case
    "format keeps first-class module expressions idempotent"
    (fun _ctx ->
      let source =
        {|open Std

module Protocol = struct
  module Http1 = struct end
end

let packed = (module Protocol.Http1)
|}
      in
      assert_idempotent ~source ~msg:"first-class module expressions should stay stable";
      Ok ());
  Test.skip
    "format class declaration items"
    (fun _ctx ->
      let source =
        {|class ['a] service : object
  val mutable state : int
  method private run : int
end =
  object (self)
    val mutable state = 0
    method private run = self#state
    (** keep body doc *)
    [%%foo]
  end
|}
      in
      assert_format_ml_fails
        ~msg:"class declaration items are outside parser2 formatter scope"
        source);
  Test.skip
    "format class type declaration items"
    (fun _ctx ->
      let source =
        {|class type ['a] service = object
  inherit base
  val mutable state : int
  method private run : 'a
  (** keep body doc *)
  [%%foo]
end

class worker : int -> service
|}
      in
      assert_format_mli_fails
        ~msg:"class type declaration items are outside parser2 formatter scope"
        source);
  Test.skip
    "format keeps shortcut class declaration modifiers idempotent"
    (fun _ctx ->
      let source = {|class%foo [@foo] x = x
class type%foo [@foo] y = y
|}
      in
      assert_format_ml_fails
        ~msg:"shortcut class declaration modifiers are outside parser2 formatter scope"
        source);
  Test.case
    "format keeps structural signature items idempotent"
    (fun _ctx ->
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
      Ok ());
  Test.case
    "format floating attributes from structural payload items"
    (fun ctx ->
      let source = "[@@@warning    \"-32\"]\n" in
      assert_formatted_ml_snapshot
        ~ctx
        ~msg:"floating attributes should render from structural payload items"
        source);
  Test.case
    "format floating extension items structurally"
    (fun ctx ->
      let structure_source = {|[%%foo]
[%%bar let x = 1]
|}
      in
      assert_formatted_ml_snapshot
        ~ctx
        ~msg:"floating structure extensions should render structurally from the extension shell and payload"
        structure_source);
  Test.case
    "format preserves scientific float exponents without introducing spaces"
    (fun _ctx ->
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
      Ok ());
  Test.case
    "format module-expression and module-type extensions structurally"
    (fun ctx ->
      let source = {|module type S = [%foo]
module M = [%foo]
|}
      in
      assert_formatted_ml_snapshot
        ~ctx
        ~msg:"module-expression and module-type extensions should render from the structural extension shell"
        source);
  Test.case
    "format structural core type token fallbacks"
    (fun _ctx ->
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
      Ok ());
  Test.case
    "format first-class module types from structural module-type docs"
    (fun ctx ->
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
|});
  Test.case
    "format shared core-type attributes keeps opaque payload tokens"
    (fun _ctx ->
      let source = "type t = int [@deprecated   \"use other\"]\n" in
      let actual =
        parse_ml source
        |> Krasny.format
        |> Result.expect ~msg:"shared core-type attributes should render from opaque payload tokens"
      in
      Test.assert_equal ~expected:"type t = int [@deprecated \"use other\"]\n" ~actual;
      Ok ());
  Test.case
    "format shared attribute payload infix expressions opaquely"
    (fun _ctx ->
      let source = "type t = int [@foo 1 + 2]\n" in
      let actual =
        parse_ml source
        |> Krasny.format
        |> Result.expect ~msg:"shared attribute payload infix expressions should render opaquely"
      in
      Test.assert_equal ~expected:source ~actual;
      Ok ());
  Test.case
    "format expression attributes keeps opaque payload tokens"
    (fun _ctx ->
      let source = "let _ = value [@foo   1  +  2]\n" in
      let actual =
        parse_ml source
        |> Krasny.format
        |> Result.expect ~msg:"expression attributes should render from opaque payload tokens"
      in
      Test.assert_equal ~expected:"let _ = value [@foo 1 + 2]\n" ~actual;
      Ok ());
  Test.case
    "format ordinary pattern-payload attributes structurally"
    (fun ctx ->
      let source = {|let simple = 1 [@foo? Some y]
let guarded = 1 [@foo? Some y when y > 0]
|}
      in
      assert_formatted_ml_snapshot
        ~ctx
        ~msg:"ordinary pattern-payload attributes should render structurally"
        source);
  Test.case
    "format parenthesizes attributed non-atomic expressions"
    (fun ctx ->
      let source =
        {|let constructor = Some 0 [@inline always]
let apply = I64.logor b (I64.shift_left b 32) [@inline always]
let infix = mask land (mask - 1) [@inline always]
|}
      in
      assert_formatted_ml_snapshot
        ~ctx
        ~msg:"postfix expression attributes should preserve attributed apply and infix payloads"
        source);
  Test.case
    "format currently fails for plain object expressions"
    (fun _ctx ->
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
      assert_format_ml_fails
        ~msg:"plain object expressions are not supported structurally yet"
        source);
  Test.case
    "format object bodies preserve terminal trivia"
    (fun _ctx ->
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
      assert_format_ml_fails ~msg:"object bodies are outside parser2 formatter scope" source);
  Test.case
    "format object extension members structurally"
    (fun _ctx ->
      let source = {|let extended =
  object
    [%%foo]
    [%%bar let x = 1]
  end
|}
      in
      assert_format_ml_fails
        ~msg:"object extension members are outside parser2 formatter scope"
        source);
  Test.case
    "format trailing variant comments with explicit separator policy"
    (fun ctx ->
      let source = "type t =\n  | A (* comment *)\n" in
      assert_formatted_ml_snapshot
        ~ctx
        ~msg:"trailing variant comments should format from explicit trivia separators"
        source);
  Test.case
    "format trailing variant docstrings with explicit separator policy"
    (fun ctx ->
      let source = "type t =\n  | A (** doc *)\n" in
      assert_formatted_ml_snapshot
        ~ctx
        ~msg:"trailing variant docstrings should format from explicit trivia separators"
        source);
  Test.case
    "format fails for signature-bodied first-class module types"
    (fun _ctx ->
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
      | Error _ -> Ok ());
  Test.case
    "format core-type extensions structurally"
    (fun ctx ->
      let source = "val use : [%foo: int]\n" in
      assert_formatted_mli_snapshot
        ~ctx
        ~msg:"core-type extensions should render structurally from the extension shell and payload"
        source);
  Test.case
    "format keeps structural patterns idempotent"
    (fun _ctx ->
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
      Ok ());
  Test.case
    "format keeps polymorphic-variant inherit patterns idempotent"
    (fun _ctx ->
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
      Ok ());
  Test.case
    "format typed first-class-module patterns structurally"
    (fun ctx ->
      let source = {ocaml|let unpack=function|(module M:S)->()|(module _)->()
|ocaml}
      in
      let parsed = parse_ml source in
      let actual = capture_write parsed in
      Test.Snapshot.assert_inline_text
        ~ctx
        ~actual
        ~expected:{ocaml|let unpack = function
  | (module M : S) -> ()
  | (module _) -> ()
|ocaml});
  Test.case
    "format pattern extensions structurally"
    (fun ctx ->
      let source =
        {|let unpack = function
  | [%foo? Some x] -> x
  | [%foo? Some y when y > 0] -> y
|}
      in
      assert_formatted_ml_snapshot
        ~ctx
        ~msg:"pattern extensions should render structurally from the extension shell and payload"
        source);
  Test.case
    "format keeps structural imperative and module expressions idempotent"
    (fun _ctx ->
      let source =
        {|let packed = (module M : S)
let guarded ready = assert ready
let delayed compute = lazy (compute ())
let loop cond body = while cond () do body () done
let count () = for i = 10 downto 0 do print_int i done
let call obj = obj#run
let cast value = (value : source :> target)
let widen value = (value :> target)
|}
      in
      assert_idempotent
        ~source
        ~msg:"module-pack, imperative, coercion, and object-override expressions should format structurally";
      Ok ());
  Test.case
    "format object override expressions"
    (fun _ctx ->
      let source = {|let update next count = {< current = next; count >}
|}
      in
      assert_format_ml_fails
        ~msg:"object override expressions are outside parser2 formatter scope"
        source);
  Test.case
    "format expression extensions structurally"
    (fun ctx ->
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
        source);
  Test.case
    "format atomic.loc extension keeps qualified name"
    (fun _ctx ->
      let source = {|let foo = fun t -> [%atomic.loc t.a]
|}
      in
      let actual =
        parse_ml source
        |> Krasny.format
        |> Result.expect
          ~msg:"atomic.loc extension should preserve qualified name and payload boundary"
      in
      Test.assert_equal ~expected:source ~actual;
      Ok ());
  Test.case
    "format unreachable expressions structurally"
    (fun _ctx ->
      let source = {|let absurd maybe =
  match maybe with
  | Some value -> value
  | None -> .
|}
      in
      let actual =
        parse_ml source
        |> Krasny.format
        |> Result.expect
          ~msg:"unreachable expressions should render structurally from the CST token"
      in
      Test.assert_equal ~expected:source ~actual;
      assert_idempotent
        ~source
        ~msg:"unreachable expressions should stay stable across repeated formatting";
      Ok ());
  Test.case
    "format keeps typed and polymorphic expressions structural"
    (fun ctx ->
      let source =
        {|let typed value = (value : source)
let shaped handler = (handler : < run : int >)
let poly = ((fun x -> x) : 'a. 'a -> 'a)
|}
      in
      assert_formatted_ml_snapshot
        ~ctx
        ~msg:"typed and polymorphic expressions should lower through structural core-type rendering"
        source);
  Test.case
    "format keeps nested module bodies structural"
    (fun _ctx ->
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
        ~msg:"nested signature and structure bodies should lower from structural item streams";
      Ok ());
  Test.case
    "format keeps grouped GADT type declarations structural"
    (fun _ctx ->
      let source =
        {|type _ expr =
  | Int : int expr
and packed =
  | Packed : int expr -> packed
|}
      in
      assert_idempotent
        ~source
        ~msg:"grouped GADT type declarations should lower structurally instead of preserving source";
      Ok ());
  Test.case
    "format inline record constructors from structure, not source newlines"
    (fun _ctx ->
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
      Ok ());
  Test.case
    "format keeps boolean if conditions with matches idempotent"
    (fun _ctx ->
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
      Ok ());
  Test.case
    "format keeps simple nested match case bodies idempotent"
    (fun ctx ->
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
|ocaml});
  Test.case
    "format keeps top-level lowered fun phrases separated"
    (fun _ctx ->
      let source = {|open Std

let ( .??[] ) () () = ();;

(()).??[(();
         ())]
;;
|}
      in
      assert_idempotent
        ~source
        ~msg:"top-level expression phrases should stay outside lowered fun bindings";
      Ok ());
  Test.case
    "format keeps top-level phrase separators structural"
    (fun ctx ->
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
        source);
  Test.case
    "format preserves syntax hash for selected codebase files"
    (fun _ctx ->
      List.for_each workspace_files ~fn:assert_roundtrip_hash;
      Ok ());
  Test.case
    "runner skips hidden and build directories"
    (fun _ctx ->
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
          Ok ()));
  Test.case
    "runner skips ignored subtrees during collection"
    (fun _ctx ->
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
              ~should_ignore:(fun path ->
                String.contains (Path.to_string path) "fixtures")
              ()
            |> List.map ~fn:Path.to_string
          in
          Test.assert_equal ~expected:[ Path.to_string keep ] ~actual:files;
          Ok ()));
  Test.case
    "runner reports formatting status and emits json events"
    (fun _ctx ->
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
          Ok ()));
  Test.case
    "verify reports files that would reformat safely"
    (fun _ctx ->
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
          Test.assert_equal
            ~expected:(Some (String "would_reformat"))
            ~actual:(get_field "status" json);
          Ok ()));
  Test.case
    "format rewrites files in place and reports formatted status"
    (fun _ctx ->
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
          Ok ()));
  Test.case
    "json file events include structured diagnostics for parse failures"
    (fun _ctx ->
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
                let expected = Some (Data.Json.Array (List.map
                  diagnostics
                  ~fn:Syn.Diagnostic.to_json))
                in
                Test.assert_equal ~expected ~actual:(Data.Json.get_field "diagnostics" json);
              Ok ()
          | None -> Error "expected broken source to carry diagnostics"));
  Test.case
    "streaming runner skips ignored files"
    (fun _ctx ->
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
              ~should_ignore:(fun path ->
                String.contains (Path.to_string path) "fixtures")
              ~on_result:(fun file_result ->
                seen := Path.to_string file_result.file :: !seen)
              ()
          in
          Test.assert_equal ~expected:[ Path.to_string keep ] ~actual:(List.reverse !seen);
          Test.assert_equal ~expected:1 ~actual:result.summary.total_files;
          Ok ()));
  Test.case
    "streaming runner scans roots and streams file results"
    (fun _ctx ->
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
              ~on_result:(fun file_result ->
                seen := Path.to_string file_result.file :: !seen)
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
          Ok ()));
]

let main ~args:_ = Test.Cli.main ~name:"krasny:format" ~tests ~args:Env.args ()

let () = Runtime.run ~main ~args:Env.args ()
