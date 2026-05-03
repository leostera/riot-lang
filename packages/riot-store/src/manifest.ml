(** Manifest for tracking stored build artifacts *)
open Std
open Std.Data
open Std.Collections

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
  files: file_entry list;
  ocamlc_warnings: string list;
  exports: export_entry list;
}

(** Convert manifest to JSON *)
let to_json manifest =
  let version_to_string = fun __tmp1 ->
    match __tmp1 with
    | V2 -> "v2"
  in
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
    ("files", Data.Json.Array (List.map manifest.files ~fn:file_entry_to_json));
    (
      "ocamlc_warnings",
      Data.Json.Array (List.map manifest.ocamlc_warnings ~fn:(fun msg -> Data.Json.String msg))
    );
    ("exports", Data.Json.Array (List.map manifest.exports ~fn:export_entry_to_json));
  ]

(** Parse manifest from JSON *)
let of_json = fun json ->
  let open Result in
  let open Data.Json in
  try
    match json with
    | Object fields -> (
        let get_field name =
          List.find fields ~fn:(fun (field_name, _) -> String.equal field_name name)
          |> Option.map ~fn:(fun (_, value) -> value)
        in
        let version =
          match get_field "version" with
          | Some (String "v2") -> Ok V2
          | _ -> Error "Invalid or missing version"
        in
        let package =
          match get_field "package" with
          | Some (String p) -> Ok p
          | _ -> Error "Invalid or missing package"
        in
        let input_hash =
          match get_field "input_hash" with
          | Some (String h) -> Ok h
          | _ -> Error "Invalid or missing input_hash"
        in
        let output_hash =
          match get_field "output_hash" with
          | Some (String h) -> Ok h
          | _ -> Error "Invalid or missing output_hash"
        in
        let timestamp =
          match get_field "timestamp" with
          | Some (Int t) -> Ok (Time.SystemTime.from_unix_timestamp t)
          | _ -> Error "Invalid or missing timestamp"
        in
        let files =
          match get_field "files" with
          | Some (Array entries) ->
              let parse_entry = fun __tmp1 ->
                match __tmp1 with
                | Object entry_fields -> (
                    let get_entry_field name =
                      List.find
                        entry_fields
                        ~fn:(fun (field_name, _) -> String.equal field_name name)
                      |> Option.map ~fn:(fun (_, value) -> value)
                    in
                    match (get_entry_field "path", get_entry_field "hash", get_entry_field "size") with
                    | (Some (String path), Some (String hash), Some (Int size)) ->
                        Ok { path = Path.v path; hash; size }
                    | _ -> Error "Invalid file entry"
                  )
                | _ -> Error "File entry must be an object"
              in
              List.fold_left
                entries
                ~init:(Ok [])
                ~fn:(fun acc entry ->
                  match (acc, parse_entry entry) with
                  | (Ok entries, Ok e) -> Ok (e :: entries)
                  | (Error e, _)
                  | (_, Error e) -> Error e)
              |> Result.map ~fn:List.reverse
          | _ -> Error "Invalid or missing files"
        in
        let ocamlc_warnings =
          match get_field "ocamlc_warnings" with
          | None -> Ok []
          | Some (Array entries) ->
              List.fold_left
                entries
                ~init:(Ok [])
                ~fn:(fun acc entry ->
                  match (acc, entry) with
                  | (Ok messages, String msg) -> Ok (msg :: messages)
                  | (Ok _, _) -> Error "Invalid ocamlc warning entry"
                  | (Error e, _) -> Error e)
              |> Result.map ~fn:List.reverse
          | Some _ -> Error "Invalid ocamlc_warnings"
        in
        let exports =
          match get_field "exports" with
          | None -> Ok []
          | Some (Array entries) ->
              let parse_entry = fun __tmp1 ->
                match __tmp1 with
                | Object entry_fields -> (
                    let get_entry_field name =
                      List.find
                        entry_fields
                        ~fn:(fun (field_name, _) -> String.equal field_name name)
                      |> Option.map ~fn:(fun (_, value) -> value)
                    in
                    match (
                      get_entry_field "name",
                      get_entry_field "path",
                      get_entry_field "action_hash"
                    ) with
                    | (Some (String name), Some (String path), Some (String action_hash)) ->
                        Ok { name; path = Path.v path; action_hash }
                    | _ -> Error "Invalid export entry"
                  )
                | _ -> Error "Export entry must be an object"
              in
              List.fold_left
                entries
                ~init:(Ok [])
                ~fn:(fun acc entry ->
                  match (acc, parse_entry entry) with
                  | (Ok parsed, Ok export) -> Ok (export :: parsed)
                  | (Error e, _)
                  | (_, Error e) -> Error e)
              |> Result.map ~fn:List.reverse
          | Some _ -> Error "Invalid exports"
        in
        match (
          version,
          package,
          input_hash,
          output_hash,
          timestamp,
          files,
          ocamlc_warnings,
          exports
        ) with
        | (Ok v, Ok p, Ok input_hash, Ok output_hash, Ok t, Ok f, Ok warnings, Ok exports) ->
            Ok {
              version = v;
              package = p;
              input_hash;
              output_hash;
              timestamp = t;
              files = f;
              ocamlc_warnings = warnings;
              exports;
            }
        | (Error e, _, _, _, _, _, _, _)
        | (_, Error e, _, _, _, _, _, _)
        | (_, _, Error e, _, _, _, _, _)
        | (_, _, _, Error e, _, _, _, _)
        | (_, _, _, _, Error e, _, _, _)
        | (_, _, _, _, _, Error e, _, _)
        | (_, _, _, _, _, _, Error e, _)
        | (_, _, _, _, _, _, _, Error e) -> Error e
      )
    | _ -> Error "Manifest must be a JSON object"
  with
  | Not_found -> Error "Missing required field"
  | _ -> Error "Failed to parse manifest"

(** Write manifest to file *)
let save = fun manifest ~path ->
  let json = to_json manifest in
  let content = Data.Json.to_string json in
  match Std.Fs.write content path with
  | Ok () -> Ok ()
  | Error _ -> Error "Failed to write manifest"

(** Read manifest from file *)
let load = fun ~path ->
  match Std.Fs.read path with
  | Ok content -> (
      match Data.Json.of_string content with
      | Ok json -> of_json json
      | Error _ -> Error "Failed to parse JSON"
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
  {
    version = V2;
    package;
    input_hash;
    output_hash = Std.Crypto.Digest.hex output_hash;
    timestamp;
    files = file_entries;
    ocamlc_warnings;
    exports;
  }
