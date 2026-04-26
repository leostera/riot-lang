open Std
open Model

type requirement =
  | MissingRootSource of {
      source_id: SourceId.t;
    }
  | MissingModuleSummary of {
      module_name: string;
      requested_by: SourceId.t list;
    }
  | LocalModuleCycle of {
      module_names: string list;
      source_ids: SourceId.t list;
    }

type t = requirement list

let compare_source_ids = fun left right ->
  Int.compare (SourceId.to_int left) (SourceId.to_int right)

let normalize_requesters = fun requested_by -> List.sort_uniq compare_source_ids requested_by

let compare_string_lists = fun left right ->
  let rec loop left right =
    match (left, right) with
    | ([], []) -> 0
    | ([], _) -> (-1)
    | (_, []) -> 1
    | (left_head :: left_tail, right_head :: right_tail) ->
        let head_compare = String.compare left_head right_head in
        if Int.equal head_compare 0 then
          loop left_tail right_tail
        else
          head_compare
  in
  loop left right

let add_module_requirement = fun modules module_name requested_by ->
  let requested_by = normalize_requesters requested_by in
  match List.assoc_opt module_name modules with
  | None -> (module_name, requested_by) :: modules
  | Some existing ->
      let merged_requested_by = normalize_requesters (existing @ requested_by) in
      (module_name, merged_requested_by) :: List.remove_assoc module_name modules

let normalize_cycle = fun ~module_names ~source_ids -> (
  module_names
  |> List.sort_uniq String.compare,
  source_ids
  |> List.sort_uniq compare_source_ids
)

let of_list = fun requirements ->
  let rec loop roots modules cycles = function
    | [] ->
        let root_requirements =
          roots
          |> List.sort_uniq compare_source_ids
          |> List.map (fun source_id -> MissingRootSource { source_id })
        in
        let module_requirements =
          modules
          |> List.sort (fun (left, _) (right, _) -> String.compare left right)
          |> List.map
            (fun (module_name, requested_by) ->
              MissingModuleSummary {
                module_name;
                requested_by = normalize_requesters requested_by;
              })
        in
        let cycle_requirements =
          cycles
          |> List.sort
            (fun (left_modules, left_source_ids) (right_modules, right_source_ids) ->
              let modules_compare = compare_string_lists left_modules right_modules in
              if Int.equal modules_compare 0 then
                compare_string_lists
                  (
                    left_source_ids
                    |> List.map SourceId.to_string
                  )
                  (
                    right_source_ids
                    |> List.map SourceId.to_string
                  )
              else
                modules_compare)
          |> List.map
            (fun (module_names, source_ids) -> LocalModuleCycle { module_names; source_ids })
        in
        (root_requirements @ module_requirements) @ cycle_requirements
    | (MissingRootSource { source_id }) :: tail -> loop (source_id :: roots) modules cycles tail
    | (MissingModuleSummary { module_name; requested_by }) :: tail ->
        loop
          roots
          (add_module_requirement modules module_name requested_by)
          cycles
          tail
    | (LocalModuleCycle { module_names; source_ids }) :: tail ->
        let cycle = normalize_cycle ~module_names ~source_ids in
        let cycles =
          if List.exists (fun existing -> existing = cycle) cycles then
            cycles
          else
            cycle :: cycles
        in
        loop roots modules cycles tail
  in
  loop [] [] [] requirements

let requirements = fun missing -> missing

let is_empty = List.is_empty

let requirement_to_json = fun value ->
  match value with
  | MissingRootSource { source_id } ->
      Data.Json.Object [
        ("tag", Data.Json.String "missing_root_source");
        ("source_id", Data.Json.Int (SourceId.to_int source_id));
      ]
  | MissingModuleSummary { module_name; requested_by } ->
      Data.Json.Object [
        ("tag", Data.Json.String "missing_module_summary");
        ("module_name", Data.Json.String module_name);
        (
          "requested_by",
          Data.Json.Array (
            requested_by
            |> List.map (fun source_id -> Data.Json.Int (SourceId.to_int source_id))
          )
        );
      ]
  | LocalModuleCycle { module_names; source_ids } ->
      Data.Json.Object [
        ("tag", Data.Json.String "local_module_cycle");
        (
          "module_names",
          Data.Json.Array (
            module_names
            |> List.map (fun module_name -> Data.Json.String module_name)
          )
        );
        (
          "source_ids",
          Data.Json.Array (
            source_ids
            |> List.map (fun source_id -> Data.Json.Int (SourceId.to_int source_id))
          )
        );
      ]

let to_json = fun missing -> Data.Json.Array (List.map requirement_to_json missing)
