open Std

(** File entity representing a source file in the codebase 
    
    File entities are identified by path + SHA256 hash, allowing us to:
    - Deduplicate files across the codebase
    - Track file changes over time
    - Query which symbols are defined in a file
    - Add file-level metadata (size, modified_at, etc.)
*)
type t = {
  path : Path.t;
  sha256 : string;
  size : int option;
  modified_at : Datetime.t option;
}

let make ~path ~sha256 ?size ?modified_at () =
  { path; sha256; size; modified_at }

(** Generate entity URI for this file
    Format: codedb:file:<path>#<sha256>
    Examples:
      codedb:file:packages/std/src/list.ml#a1b2c3d4e5...
      codedb:file:src/main.ml#f6e7d8c9b0...
*)
let entity_uri t =
  let path_str = Path.to_string t.path in
  Poneglyph.Uri.of_string (String.concat "" ["codedb:file:"; path_str; "#"; t.sha256])

(** Convert file to Poneglyph facts
    Creates 2-4 facts per file using Schema.Codedb attributes:
    - codedb:attr:path: file path (required)
    - codedb:attr:sha256: SHA256 hash (required)
    - codedb:attr:size: file size in bytes (optional)
    - codedb:attr:modified_at: last modified timestamp (optional)
    
    @param tx_id Transaction ID to group these facts with others
    @param stated_at Optional timestamp for when facts are stated (defaults to now)
    @param t The file to convert
*)
let to_facts ~tx_id ?(stated_at = Datetime.now ()) t =
  let entity = entity_uri t in
  
  let required_facts = [
    Poneglyph.fact ~source:Schema.Codedb.source ~entity
      ~attribute:Schema.Codedb.path
      ~value:(Poneglyph.Fact.String (Path.to_string t.path))
      ~stated_at ~tx_id;
      
    Poneglyph.fact ~source:Schema.Codedb.source ~entity
      ~attribute:Schema.Codedb.sha256
      ~value:(Poneglyph.Fact.String t.sha256)
      ~stated_at ~tx_id;
  ] in
  
  let size_fact = match t.size with
    | Some size ->
        [Poneglyph.fact ~source:Schema.Codedb.source ~entity
          ~attribute:Schema.Codedb.size
          ~value:(Poneglyph.Fact.Int size)
          ~stated_at ~tx_id]
    | None -> []
  in
  
  let modified_at_fact = match t.modified_at with
    | Some dt ->
        [Poneglyph.fact ~source:Schema.Codedb.source ~entity
          ~attribute:Schema.Codedb.modified_at
          ~value:(Poneglyph.Fact.String (Datetime.to_iso8601 dt))
          ~stated_at ~tx_id]
    | None -> []
  in
  
  required_facts @ size_fact @ modified_at_fact

let to_json t =
  let base_fields = [
    ("path", Data.Json.String (Path.to_string t.path));
    ("sha256", Data.Json.String t.sha256);
  ] in
  
  let with_size = match t.size with
    | Some size -> ("size", Data.Json.Int size) :: base_fields
    | None -> base_fields
  in
  
  let with_modified_at = match t.modified_at with
    | Some dt -> ("modified_at", Data.Json.String (Datetime.to_iso8601 dt)) :: with_size
    | None -> with_size
  in
  
  Data.Json.Object with_modified_at

let from_json json =
  match json with
  | Data.Json.Object fields ->
      (match List.assoc_opt "path" fields, List.assoc_opt "sha256" fields with
       | Some (Data.Json.String path), Some (Data.Json.String sha256) ->
           let size = match List.assoc_opt "size" fields with
             | Some (Data.Json.Int s) -> Some s
             | _ -> None
           in
           let modified_at = match List.assoc_opt "modified_at" fields with
             | Some (Data.Json.String dt_str) ->
                 (match Datetime.parse dt_str with
                  | Ok dt -> Some dt
                  | Error _ -> None)
             | _ -> None
           in
           Ok { path = Path.v path; sha256; size; modified_at }
       | _ -> Error "Missing required fields 'path' or 'sha256'")
  | _ -> Error "Expected object for file"

(** Quick hack: convert file to module symbol if it's a .ml file in packages/ *)
