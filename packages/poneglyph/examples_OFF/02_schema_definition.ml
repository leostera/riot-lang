(** Example 2: Schema Definition - Using the fluent schema API *)

open Std
open Poneglyph

(* Define a schema for a build system *)
module BuildSchema = struct
  open Schema

  let ns = namespace "build"

  (* Define kinds (entity types) *)
  let file =
    kind ~ns "file" |> doc "A source file in the project"

  let package =
    kind ~ns "package" |> doc "A build package"

  let artifact =
    kind ~ns "artifact" |> doc "A compiled artifact"

  (* Define fields (attributes) *)
  let content_hash =
    field ~ns "content_hash"
    |> used_on file
    |> value_type Type.string
    |> doc "SHA256 hash of file content"

  let formatted =
    field ~ns "formatted"
    |> used_on file
    |> value_type Type.bool
    |> doc "Whether file has been formatted"

  let depends_on =
    field ~ns "depends_on"
    |> used_on package
    |> value_type Type.uri
    |> cardinality "many"
    |> doc "Package dependencies"

  let produced_by =
    field ~ns "produced_by"
    |> used_on artifact
    |> value_type Type.uri
    |> doc "Which package produced this artifact"

  let all_defs = [ file; package; artifact; content_hash; formatted; depends_on; produced_by ]

  (* Fact builder helpers *)
  let content_hash_fact ~hash = string_value ~field:content_hash ~value:hash
  let formatted_fact ~value = bool_value ~field:formatted ~value
  let depends_on_fact ~pkg = uri_value ~field:depends_on ~value:pkg
  let produced_by_fact ~pkg = uri_value ~field:produced_by ~value:pkg
end

let () =
  Log.info "=== Example 2: Schema Definition ===";

  let graph = create () in
  
  (* Register the schema *)
  register_schema graph BuildSchema.all_defs;
  Log.info "Registered BuildSchema";

  (* Create entities using the schema *)
  let file_uri = Uri.make Uri.[ ns "build"; kind "file"; id "src/main.ml" ] in
  let package_uri = Uri.make Uri.[ ns "build"; kind "package"; id "myapp" ] in

  (* State facts using the schema helpers *)
  let facts =
    Fact.for_entity file_uri
      [
        BuildSchema.content_hash_fact ~hash:"abc123";
        BuildSchema.formatted_fact ~value:true;
      ]
  in

  let tx_id = state graph facts in
  Log.info ("Stated facts for file in tx " ^ string_of_int tx_id);

  (* Link file to package *)
  let package_facts =
    [
      Fact.make ~entity:file_uri
        ~attribute:(Uri.of_string "build:belongs_to")
        ~value:(Fact.Uri package_uri)
        ~stated_at:(Datetime.now ()) ~tx_id:0;
    ]
  in
  let _ = state graph package_facts in

  (* Query using the schema *)
  (match get graph ~entity:file_uri ~attr:(fst BuildSchema.formatted) with
  | Some (Fact.Bool true) -> Log.info "File is formatted ✓"
  | Some (Fact.Bool false) -> Log.info "File needs formatting"
  | _ -> Log.warn "Formatting status unknown");

  (* List registered schemas *)
  let schemas = list_schemas graph in
  Log.info ("Registered schemas: " ^ string_of_int (List.length schemas));

  Log.info "=== Example 2 Complete ==="
