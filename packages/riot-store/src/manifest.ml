(** Manifest for tracking stored build artifacts *)
open Std
open Std.Data
open Std.Collections
open Std.Result.Syntax

module De = Serde.De
module Ser = Serde.Ser

type version =
  | V2

type file_entry = {
  path: Path.t;
  hash: string;
  (* SHA512 hash of the file *)
  size: int;
}

type export_entry = {
  name: string;
  path: Path.t;
  action_hash: string;
}

type t = {
  version: version;
  package: string;
  input_hash: string;
  output_hash: string;
  timestamp: Time.SystemTime.t;
  size_bytes: int64;
  files: file_entry list;
  ocamlc_warnings: string list;
  exports: export_entry list;
}

type metadata = {
  input_hash: string;
  output_hash: string;
  size_bytes: int64;
  ocamlc_warnings: string list;
  exports: export_entry list;
}

type manifest = t

let file_entries_size_bytes = fun files ->
  List.fold_left
    files
    ~init:0L
    ~fn:(fun acc (entry: file_entry) -> Int64.add acc (Int64.from_int entry.size))

let version_to_string = fun __tmp1 ->
  match __tmp1 with
  | V2 -> "v2"

let version_of_string = fun __tmp1 ->
  match __tmp1 with
  | "v2" -> V2
  | _ -> De.raise_error (`Msg "Invalid or missing version")

let vector_to_list = fun values ->
  let rec loop index items =
    if index < 0 then
      items
    else
      loop (Int.sub index 1) (Vector.get_unchecked values ~at:index :: items)
  in
  loop (Int.sub (Vector.length values) 1) []

let de_list = fun decode -> De.map (De.list decode) vector_to_list

let ser_list = fun encode -> Ser.contramap Vector.from_list (Ser.list encode)

let path_decode = De.map De.string Path.v

let path_encode = Ser.contramap Path.to_string Ser.string

let version_decode = De.map De.string version_of_string

let version_encode = Ser.contramap version_to_string Ser.string

let int64_string_decode =
  De.map
    De.string
    (fun value ->
      match Int64.parse value with
      | Some value -> value
      | None -> De.raise_error (`Msg "Invalid size_bytes"))

let int64_string_encode = Ser.contramap Int64.to_string Ser.string

let system_time_decode =
  De.map De.int Time.SystemTime.from_unix_timestamp

let system_time_encode =
  Ser.contramap Time.SystemTime.to_unix_timestamp Ser.int

type file_entry_field =
  | File_path
  | File_hash
  | File_size

type export_entry_field =
  | Export_name
  | Export_path
  | Export_action_hash

type manifest_field =
  | Manifest_version
  | Manifest_package
  | Manifest_input_hash
  | Manifest_output_hash
  | Manifest_timestamp
  | Manifest_size_bytes
  | Manifest_files
  | Manifest_ocamlc_warnings
  | Manifest_exports

type file_entry_builder = {
  mutable file_path: Path.t option;
  mutable file_hash: string option;
  mutable file_size: int option;
}

type export_entry_builder = {
  mutable export_name: string option;
  mutable export_path: Path.t option;
  mutable export_action_hash: string option;
}

type manifest_builder = {
  mutable version: version option;
  mutable package: string option;
  mutable input_hash: string option;
  mutable output_hash: string option;
  mutable timestamp: Time.SystemTime.t option;
  mutable size_bytes: int64 option;
  mutable files: file_entry list option;
  mutable ocamlc_warnings: string list;
  mutable exports: export_entry list;
}

let file_entry_fields =
  De.fields [
    De.field "path" File_path;
    De.field "hash" File_hash;
    De.field "size" File_size;
  ]

let export_entry_fields =
  De.fields [
    De.field "name" Export_name;
    De.field "path" Export_path;
    De.field "action_hash" Export_action_hash;
  ]

let manifest_fields =
  De.fields [
    De.field "version" Manifest_version;
    De.field "package" Manifest_package;
    De.field "input_hash" Manifest_input_hash;
    De.field "output_hash" Manifest_output_hash;
    De.field "timestamp" Manifest_timestamp;
    De.field "size_bytes" Manifest_size_bytes;
    De.field "files" Manifest_files;
    De.field "ocamlc_warnings" Manifest_ocamlc_warnings;
    De.field "exports" Manifest_exports;
  ]

let metadata_fields =
  De.fields [
    De.field "input_hash" Manifest_input_hash;
    De.field "output_hash" Manifest_output_hash;
    De.field "size_bytes" Manifest_size_bytes;
    De.field "ocamlc_warnings" Manifest_ocamlc_warnings;
    De.field "exports" Manifest_exports;
  ]

let file_entry_decode =
  De.record_mut
    ~fields:file_entry_fields
    ~create:(fun () -> { file_path = None; file_hash = None; file_size = None })
    ~step:(fun reader builder field ->
      match field with
      | Some File_path -> builder.file_path <- Some (De.read reader path_decode)
      | Some File_hash -> builder.file_hash <- Some (De.read reader De.string)
      | Some File_size -> builder.file_size <- Some (De.read reader De.int)
      | None -> ignore (De.read reader De.skip_any))
    ~finish:(fun builder ->
      match (builder.file_path, builder.file_hash, builder.file_size) with
      | (Some path, Some hash, Some size) -> { path; hash; size }
      | _ -> De.missing_field ())

let export_entry_decode =
  De.record_mut
    ~fields:export_entry_fields
    ~create:(fun () -> {
      export_name = None;
      export_path = None;
      export_action_hash = None;
    })
    ~step:(fun reader builder field ->
      match field with
      | Some Export_name -> builder.export_name <- Some (De.read reader De.string)
      | Some Export_path -> builder.export_path <- Some (De.read reader path_decode)
      | Some Export_action_hash ->
          builder.export_action_hash <- Some (De.read reader De.string)
      | None -> ignore (De.read reader De.skip_any))
    ~finish:(fun builder ->
      match (builder.export_name, builder.export_path, builder.export_action_hash) with
      | (Some name, Some path, Some action_hash) -> { name; path; action_hash }
      | _ -> De.missing_field ())

let manifest_decode =
  De.record_mut
    ~fields:manifest_fields
    ~create:(fun () -> {
      version = None;
      package = None;
      input_hash = None;
      output_hash = None;
      timestamp = None;
      size_bytes = None;
      files = None;
      ocamlc_warnings = [];
      exports = [];
    })
    ~step:(fun reader builder field ->
      match field with
      | Some Manifest_version -> builder.version <- Some (De.read reader version_decode)
      | Some Manifest_package -> builder.package <- Some (De.read reader De.string)
      | Some Manifest_input_hash -> builder.input_hash <- Some (De.read reader De.string)
      | Some Manifest_output_hash -> builder.output_hash <- Some (De.read reader De.string)
      | Some Manifest_timestamp -> builder.timestamp <- Some (De.read reader system_time_decode)
      | Some Manifest_size_bytes -> builder.size_bytes <- Some (De.read reader int64_string_decode)
      | Some Manifest_files -> builder.files <- Some (De.read reader (de_list file_entry_decode))
      | Some Manifest_ocamlc_warnings ->
          builder.ocamlc_warnings <- De.read reader (de_list De.string)
      | Some Manifest_exports -> builder.exports <- De.read reader (de_list export_entry_decode)
      | None -> ignore (De.read reader De.skip_any))
    ~finish:(fun builder ->
      match
        (
          builder.version,
          builder.package,
          builder.input_hash,
          builder.output_hash,
          builder.timestamp,
          builder.files
        )
      with
      | (Some version, Some package, Some input_hash, Some output_hash, Some timestamp, Some files) ->
          let size_bytes =
            match builder.size_bytes with
            | Some size_bytes -> size_bytes
            | None -> file_entries_size_bytes files
          in
          ({
            version;
            package;
            input_hash;
            output_hash;
            timestamp;
            size_bytes;
            files;
            ocamlc_warnings = builder.ocamlc_warnings;
            exports = builder.exports;
          }: t)
      | _ -> De.missing_field ())

let metadata_decode =
  De.record_mut
    ~fields:metadata_fields
    ~create:(fun () -> {
      version = None;
      package = None;
      input_hash = None;
      output_hash = None;
      timestamp = None;
      size_bytes = None;
      files = None;
      ocamlc_warnings = [];
      exports = [];
    })
    ~step:(fun reader builder field ->
      match field with
      | Some Manifest_input_hash -> builder.input_hash <- Some (De.read reader De.string)
      | Some Manifest_output_hash -> builder.output_hash <- Some (De.read reader De.string)
      | Some Manifest_size_bytes -> builder.size_bytes <- Some (De.read reader int64_string_decode)
      | Some Manifest_ocamlc_warnings ->
          builder.ocamlc_warnings <- De.read reader (de_list De.string)
      | Some Manifest_exports -> builder.exports <- De.read reader (de_list export_entry_decode)
      | _ -> ignore (De.read reader De.skip_any))
    ~finish:(fun builder ->
      match (builder.input_hash, builder.output_hash, builder.size_bytes) with
      | (Some input_hash, Some output_hash, Some size_bytes) ->
          {
            input_hash;
            output_hash;
            size_bytes;
            ocamlc_warnings = builder.ocamlc_warnings;
            exports = builder.exports;
          }
      | _ -> De.missing_field ())

let file_entry_encode =
  Ser.record
    (
      Ser.fields [
        Ser.field "path" path_encode (fun (entry: file_entry) -> entry.path);
        Ser.field "hash" Ser.string (fun (entry: file_entry) -> entry.hash);
        Ser.field "size" Ser.int (fun (entry: file_entry) -> entry.size);
      ]
    )

let export_entry_encode =
  Ser.record
    (
      Ser.fields [
        Ser.field "name" Ser.string (fun (entry: export_entry) -> entry.name);
        Ser.field "path" path_encode (fun (entry: export_entry) -> entry.path);
        Ser.field "action_hash" Ser.string (fun (entry: export_entry) -> entry.action_hash);
      ]
    )

let manifest_encode =
  Ser.record
    (
      Ser.fields [
        Ser.field "version" version_encode (fun (manifest: t) -> manifest.version);
        Ser.field "package" Ser.string (fun (manifest: t) -> manifest.package);
        Ser.field "input_hash" Ser.string (fun (manifest: t) -> manifest.input_hash);
        Ser.field "output_hash" Ser.string (fun (manifest: t) -> manifest.output_hash);
        Ser.field "timestamp" system_time_encode (fun (manifest: t) -> manifest.timestamp);
        Ser.field "size_bytes" int64_string_encode (fun (manifest: t) -> manifest.size_bytes);
        Ser.field "files" (ser_list file_entry_encode) (fun (manifest: t) -> manifest.files);
        Ser.field
          "ocamlc_warnings"
          (ser_list Ser.string)
          (fun (manifest: t) -> manifest.ocamlc_warnings);
        Ser.field "exports" (ser_list export_entry_encode) (fun (manifest: t) -> manifest.exports);
      ]
    )

(** Convert manifest to JSON *)
let to_json (manifest: t) =
  let file_entry_to_json (entry: file_entry) =
    Data.Json.Object [
      ("path", Data.Json.String (Path.to_string entry.path));
      ("hash", Data.Json.String entry.hash);
      ("size", Data.Json.Int entry.size);
    ]
  in
  let export_entry_to_json (entry: export_entry) =
    Data.Json.Object [
      ("name", Data.Json.String entry.name);
      ("path", Data.Json.String (Path.to_string entry.path));
      ("action_hash", Data.Json.String entry.action_hash);
    ]
  in
  Data.Json.Object [
    ("version", Data.Json.String (version_to_string manifest.version));
    ("package", Data.Json.String manifest.package);
    ("input_hash", Data.Json.String manifest.input_hash);
    ("output_hash", Data.Json.String manifest.output_hash);
    ("timestamp", Data.Json.Int (Time.SystemTime.to_unix_timestamp manifest.timestamp));
    ("size_bytes", Data.Json.String (Int64.to_string manifest.size_bytes));
    ("files", Data.Json.Array (List.map manifest.files ~fn:file_entry_to_json));
    (
      "ocamlc_warnings",
      Data.Json.Array (List.map manifest.ocamlc_warnings ~fn:(fun msg -> Data.Json.String msg))
    );
    ("exports", Data.Json.Array (List.map manifest.exports ~fn:export_entry_to_json));
  ]

(** Parse manifest from JSON *)
let from_json = fun json ->
  let open Data.Json in
  try
    match json with
    | Object fields ->
        let get_field name =
          List.find fields ~fn:(fun (field_name, _) -> String.equal field_name name)
          |> Option.map ~fn:(fun (_, value) -> value)
        in
        let required_string = fun name error ->
          match get_field name with
          | Some (String value) -> Ok value
          | _ -> Error error
        in
        let required_int = fun name error ->
          match get_field name with
          | Some (Int value) -> Ok value
          | _ -> Error error
        in
        let parse_object_field = fun fields name ->
          List.find fields ~fn:(fun (field_name, _) -> String.equal field_name name)
          |> Option.map ~fn:(fun (_, value) -> value)
        in
        let parse_list = fun entries ~fn ->
          List.fold_left
            entries
            ~init:(Ok [])
            ~fn:(fun acc_result entry ->
              let* acc = acc_result in
              let* parsed = fn entry in
              Ok (parsed :: acc))
          |> Result.map ~fn:List.reverse
        in
        let* version =
          match get_field "version" with
          | Some (String "v2") -> Ok V2
          | _ -> Error "Invalid or missing version"
        in
        let* package = required_string "package" "Invalid or missing package" in
        let* input_hash = required_string "input_hash" "Invalid or missing input_hash" in
        let* output_hash = required_string "output_hash" "Invalid or missing output_hash" in
        let* timestamp =
          required_int "timestamp" "Invalid or missing timestamp"
          |> Result.map ~fn:Time.SystemTime.from_unix_timestamp
        in
        let parse_file_entry = fun __tmp1 ->
          match __tmp1 with
          | Object entry_fields ->
              let get_entry_field = parse_object_field entry_fields in
              let* path =
                match get_entry_field "path" with
                | Some (String path) -> Ok path
                | _ -> Error "Invalid file entry"
              in
              let* hash =
                match get_entry_field "hash" with
                | Some (String hash) -> Ok hash
                | _ -> Error "Invalid file entry"
              in
              let* size =
                match get_entry_field "size" with
                | Some (Int size) -> Ok size
                | _ -> Error "Invalid file entry"
              in
              Ok { path = Path.v path; hash; size }
          | _ -> Error "File entry must be an object"
        in
        let* files =
          match get_field "files" with
          | Some (Array entries) -> parse_list entries ~fn:parse_file_entry
          | _ -> Error "Invalid or missing files"
        in
        let parse_warning = fun __tmp1 ->
          match __tmp1 with
          | String msg -> Ok msg
          | _ -> Error "Invalid ocamlc warning entry"
        in
        let* ocamlc_warnings =
          match get_field "ocamlc_warnings" with
          | None -> Ok []
          | Some (Array entries) -> parse_list entries ~fn:parse_warning
          | Some _ -> Error "Invalid ocamlc_warnings"
        in
        let parse_export_entry = fun __tmp1 ->
          match __tmp1 with
          | Object entry_fields ->
              let get_entry_field = parse_object_field entry_fields in
              let* name =
                match get_entry_field "name" with
                | Some (String name) -> Ok name
                | _ -> Error "Invalid export entry"
              in
              let* path =
                match get_entry_field "path" with
                | Some (String path) -> Ok path
                | _ -> Error "Invalid export entry"
              in
              let* action_hash =
                match get_entry_field "action_hash" with
                | Some (String action_hash) -> Ok action_hash
                | _ -> Error "Invalid export entry"
              in
              Ok { name; path = Path.v path; action_hash }
          | _ -> Error "Export entry must be an object"
        in
        let* exports =
          match get_field "exports" with
          | None -> Ok []
          | Some (Array entries) -> parse_list entries ~fn:parse_export_entry
          | Some _ -> Error "Invalid exports"
        in
        let* size_bytes =
          match get_field "size_bytes" with
          | Some (String size_bytes) -> (
              match Int64.parse size_bytes with
              | Some size_bytes -> Ok size_bytes
              | None -> Error "Invalid size_bytes"
            )
          | None -> Ok (file_entries_size_bytes files)
          | Some _ -> Error "Invalid size_bytes"
        in
        Ok ({
          version;
          package;
          input_hash;
          output_hash;
          timestamp;
          size_bytes;
          files;
          ocamlc_warnings;
          exports;
        }: manifest)
    | _ -> Error "Manifest must be a JSON object"
  with
  | Not_found -> Error "Missing required field"
  | _ -> Error "Failed to parse manifest"

(** Write manifest to file *)
let save = fun (manifest: t) ~path ->
  match Serde_json.to_string manifest_encode manifest with
  | Error err -> Error (Serde.Error.to_string err)
  | Ok content -> (
      match Std.Fs.write content path with
      | Ok () -> Ok ()
      | Error _ -> Error "Failed to write manifest"
    )

(** Read manifest from file *)
let load = fun ~path ->
  match Std.Fs.read path with
  | Ok content -> (
      match Serde_json.from_string manifest_decode content with
      | Ok manifest -> Ok manifest
      | Error err -> Error (Serde.Error.to_string err)
    )
  | Error _ -> Error "Failed to read manifest file"

let load_metadata = fun ~path ->
  match Std.Fs.read path with
  | Ok content -> (
      match Serde_json.from_string metadata_decode content with
      | Ok metadata -> Ok metadata
      | Error err -> Error (Serde.Error.to_string err)
    )
  | Error _ -> Error "Failed to read manifest file"

(** Create a manifest for stored files *)
let compute_output_hash = fun ~package ~files ~exports ->
  let hasher = Std.Crypto.Sha256.create () in
  let write = Std.Crypto.Sha256.write hasher in
  write "riot-output:v1";
  write package;
  let sorted_files =
    List.sort
      files
      ~compare:(fun (left: file_entry) (right: file_entry) ->
        String.compare
          (Path.to_string left.path)
          (Path.to_string right.path))
  in
  List.for_each
    sorted_files
    ~fn:(fun (entry: file_entry) ->
      write (Path.to_string entry.path);
      write entry.hash;
      write (Int.to_string entry.size));
  let sorted_exports =
    List.sort
      exports
      ~compare:(fun (left: export_entry) (right: export_entry) ->
        match String.compare left.name right.name with
        | Order.EQ -> String.compare (Path.to_string left.path) (Path.to_string right.path)
        | order -> order)
  in
  List.for_each
    sorted_exports
    ~fn:(fun (entry: export_entry) ->
      write entry.name;
      write (Path.to_string entry.path);
      write entry.action_hash);
  Std.Crypto.Sha256.finish hasher

(** Create a manifest for stored files *)
let create = fun ?base_dir ?(ocamlc_warnings = []) ?(exports = []) () ~package ~input_hash ~files ->
  let timestamp = Time.SystemTime.now () in
  let file_entries =
    List.filter_map
      files
      ~fn:(fun (path, size) ->
        let readable_path =
          if Path.is_absolute path then
            path
          else
            match base_dir with
            | Some dir -> Path.(dir / path)
            | None -> path
        in
        (* Calculate hash of the file *)
        match Std.Fs.read_to_string readable_path with
        | Ok file ->
            let hash = Std.Crypto.Digest.hex (Std.Crypto.Sha512.hash_string file) in
            Some { path; hash; size }
        | Error _ -> None)
  in
  let output_hash = compute_output_hash ~package ~files:file_entries ~exports in
  let size_bytes = file_entries_size_bytes file_entries in
  ({
    version = V2;
    package;
    input_hash;
    output_hash = Std.Crypto.Digest.hex output_hash;
    timestamp;
    size_bytes;
    files = file_entries;
    ocamlc_warnings;
    exports;
  }: manifest)
