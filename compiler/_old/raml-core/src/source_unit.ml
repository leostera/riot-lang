open Std
open Std.Data

type kind =
  | Implementation
  | Interface

type t = {
  relpath: Path.t;
  unit_name: string;
  kind: kind;
  source_bytes: int;
  source_lines: int;
  nonempty_lines: int;
  has_trailing_newline: bool;
}

let kind_of_relpath = fun relpath ->
  match Path.extension relpath with
  | Some ".ml" -> Ok Implementation
  | Some ".mli" -> Ok Interface
  | Some ext -> Error (format Format.[ str "unsupported source unit extension: "; str ext ])
  | None -> Error "source unit path is missing an extension"

let unit_name_of_relpath = fun relpath ->
  relpath |> Path.remove_extension |> Path.basename |> String.capitalize_ascii

let split_source_lines = fun source ->
  if String.equal source "" then
    []
  else
    let parts = String.split_on_char '\n' source in
    if String.ends_with ~suffix:"\n" source then
      match List.rev parts with
      | "" :: rest -> List.rev rest
      | _ -> parts
    else
      parts

let from_source = fun ~relpath ~source ->
  match kind_of_relpath relpath with
  | Error _ as error -> error
  | Ok kind ->
      let lines = split_source_lines source in
      let nonempty_lines =
        lines
        |> List.filter
          ~fn:(fun line ->
            if String.equal (String.trim line) "" then
              false
            else
              true)
        |> List.length
      in
      Ok {
        relpath;
        unit_name = unit_name_of_relpath relpath;
        kind;
        source_bytes = String.length source;
        source_lines = List.length lines;
        nonempty_lines;
        has_trailing_newline = String.ends_with ~suffix:"\n" source;
      }

let kind_to_string = fun kind ->
  match kind with
  | Implementation -> "implementation"
  | Interface -> "interface"

let to_json = fun unit_ ->
  Json.obj
    [
      ("relpath", Json.string (Path.to_string unit_.relpath));
      ("unit_name", Json.string unit_.unit_name);
      ("kind", Json.string (kind_to_string unit_.kind));
      ("source_bytes", Json.int unit_.source_bytes);
      ("source_lines", Json.int unit_.source_lines);
      ("nonempty_lines", Json.int unit_.nonempty_lines);
      ("has_trailing_newline", Json.bool unit_.has_trailing_newline);
    ]
