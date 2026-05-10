open Std

type t = {
  input_hash: Std.Crypto.hash;
  output_hash: Std.Crypto.hash;
  size_bytes: int64;
  files: Manifest.file_entry list;
  ocamlc_warnings: string list;
  exports: Manifest.export_entry list;
}

module De = Serde.De
module Ser = Serde.Ser

let vector_to_list = fun values ->
  let rec loop index items =
    if index < 0 then
      items
    else
      loop (Int.sub index 1) (Std.Collections.Vector.get_unchecked values ~at:index :: items)
  in
  loop (Int.sub (Std.Collections.Vector.length values) 1) []

let de_list = fun decode -> De.map (De.list decode) vector_to_list

let ser_list = fun encode -> Ser.contramap Std.Collections.Vector.from_list (Ser.list encode)

let hash_of_hex = fun hex ->
  let hex_nibble ch =
    match ch with
    | '0' .. '9' -> Some (Char.code ch - Char.code '0')
    | 'a' .. 'f' -> Some (10 + Char.code ch - Char.code 'a')
    | 'A' .. 'F' -> Some (10 + Char.code ch - Char.code 'A')
    | _ -> None
  in
  let len = String.length hex in
  if len = 0 || len mod 2 != 0 then
    None
  else
    let bytes = IO.Bytes.create ~size:(len / 2) in
    let rec loop index =
      if index >= len then
        Some (Crypto.Hash.from_bytes bytes)
      else
        match (
          hex_nibble (String.get_unchecked hex ~at:index),
          hex_nibble (String.get_unchecked hex ~at:(index + 1))
        ) with
        | (Some hi, Some lo) ->
            IO.Bytes.set_unchecked
              bytes
              ~at:(index / 2)
              ~char:(Char.from_int_unchecked ((hi lsl 4) lor lo));
            loop (index + 2)
        | _ -> None
    in
    loop 0

let hash_deserializer =
  De.map
    De.string
    (fun hex ->
      match hash_of_hex hex with
      | Some hash -> hash
      | None -> De.raise_error (`Msg "invalid hash hex"))

let hash_serializer = Ser.contramap Crypto.Digest.hex Ser.string

let int64_string_deserializer =
  De.map
    De.string
    (fun value ->
      match Int64.parse value with
      | Some value -> value
      | None -> De.raise_error (`Msg "invalid int64 string"))

let int64_string_serializer = Ser.contramap Int64.to_string Ser.string

type field =
  | Input_hash
  | Output_hash
  | Size_bytes
  | Files
  | Ocamlc_warnings
  | Exports

type builder = {
  mutable input_hash: Crypto.hash option;
  mutable output_hash: Crypto.hash option;
  mutable size_bytes: int64 option;
  mutable files: Manifest.file_entry list option;
  mutable ocamlc_warnings: string list;
  mutable exports: Manifest.export_entry list;
}

let fields =
  De.fields
    [
      De.field "input_hash" Input_hash;
      De.field "output_hash" Output_hash;
      De.field "size_bytes" Size_bytes;
      De.field "files" Files;
      De.field "ocamlc_warnings" Ocamlc_warnings;
      De.field "exports" Exports;
    ]

let deserializer =
  De.record_mut
    ~fields
    ~create:(fun () ->
      {
        input_hash = None;
        output_hash = None;
        size_bytes = None;
        files = Some [];
        ocamlc_warnings = [];
        exports = [];
      })
    ~step:(fun reader builder field ->
      match field with
      | Some Input_hash -> builder.input_hash <- Some (De.read reader hash_deserializer)
      | Some Output_hash -> builder.output_hash <- Some (De.read reader hash_deserializer)
      | Some Size_bytes -> builder.size_bytes <- Some (De.read reader int64_string_deserializer)
      | Some Files ->
          builder.files <- Some (De.read reader (de_list Manifest.file_entry_deserializer))
      | Some Ocamlc_warnings -> builder.ocamlc_warnings <- De.read reader (de_list De.string)
      | Some Exports ->
          builder.exports <- De.read reader (de_list Manifest.export_entry_deserializer)
      | None -> ignore (De.read reader De.skip_any))
    ~finish:(fun builder ->
      match (builder.input_hash, builder.output_hash, builder.size_bytes, builder.files) with
      | (Some input_hash, Some output_hash, Some size_bytes, Some files) ->
          ({
            input_hash;
            output_hash;
            size_bytes;
            files;
            ocamlc_warnings = builder.ocamlc_warnings;
            exports = builder.exports;
          }: t)
      | _ -> De.missing_field ())

let serializer =
  Ser.record
    (
      Ser.fields
        [
          Ser.field "input_hash" hash_serializer (fun (artifact: t) -> artifact.input_hash);
          Ser.field "output_hash" hash_serializer (fun (artifact: t) -> artifact.output_hash);
          Ser.field "size_bytes" int64_string_serializer (fun (artifact: t) -> artifact.size_bytes);
          Ser.field
            "files"
            (ser_list Manifest.file_entry_serializer)
            (fun (artifact: t) -> artifact.files);
          Ser.field
            "ocamlc_warnings"
            (ser_list Ser.string)
            (fun (artifact: t) -> artifact.ocamlc_warnings);
          Ser.field
            "exports"
            (ser_list Manifest.export_entry_serializer)
            (fun (artifact: t) -> artifact.exports);
        ]
    )
