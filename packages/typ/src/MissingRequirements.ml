open Std

type requirement =
  | MissingRootSource of { source_id: SourceId.t }
  | MissingModuleSummary of { module_name: string; requested_by: SourceId.t list }

type t = requirement list

let of_list = fun requirements -> requirements

let requirements = fun missing -> missing

let is_empty = fun missing -> List.is_empty missing

let requirement_to_json = function
  | MissingRootSource { source_id } ->
      Data.Json.Object [
        ("tag", Data.Json.String "missing_root_source");
        ("source_id", Data.Json.Int (SourceId.to_int source_id));
      ]
  | MissingModuleSummary { module_name; requested_by } ->
      Data.Json.Object [
        ("tag", Data.Json.String "missing_module_summary");
        ("module_name", Data.Json.String module_name);
        ("requested_by", Data.Json.Array (requested_by |> List.map (fun source_id -> Data.Json.Int (SourceId.to_int source_id))));
      ]

let to_json = fun missing ->
  Data.Json.Array (List.map requirement_to_json missing)
