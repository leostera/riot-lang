open Std

type counts = {
  total: int;
  cached: int;
  executed: int;
  failed: int;
}

type phase_totals = {
  dependency_hashing: Time.Duration.t;
  input_hashing: Time.Duration.t;
  store_lookup: Time.Duration.t;
  cache_promotion: Time.Duration.t;
  sandbox_prepare: Time.Duration.t;
  source_staging: Time.Duration.t;
  command_execution: Time.Duration.t;
  output_verification: Time.Duration.t;
  store_save: Time.Duration.t;
  total: Time.Duration.t;
}

type group = {
  label: string;
  counts: counts;
  phases: phase_totals;
}

type t = {
  counts: counts;
  phases: phase_totals;
  by_status: group list;
  by_action_kind: group list;
}

let empty_counts = {
  total = 0;
  cached = 0;
  executed = 0;
  failed = 0;
}

let empty_phases = {
  dependency_hashing = Time.Duration.zero;
  input_hashing = Time.Duration.zero;
  store_lookup = Time.Duration.zero;
  cache_promotion = Time.Duration.zero;
  sandbox_prepare = Time.Duration.zero;
  source_staging = Time.Duration.zero;
  command_execution = Time.Duration.zero;
  output_verification = Time.Duration.zero;
  store_save = Time.Duration.zero;
  total = Time.Duration.zero;
}

let add_duration = Time.Duration.add

let add_timing = fun (phases: phase_totals) (timing: Action_execution.timing) -> {
  dependency_hashing = add_duration phases.dependency_hashing timing.dependency_hashing;
  input_hashing = add_duration phases.input_hashing timing.input_hashing;
  store_lookup = add_duration phases.store_lookup timing.store_lookup;
  cache_promotion = add_duration phases.cache_promotion timing.cache_promotion;
  sandbox_prepare = add_duration phases.sandbox_prepare timing.sandbox_prepare;
  source_staging = add_duration phases.source_staging timing.source_staging;
  command_execution = add_duration phases.command_execution timing.command_execution;
  output_verification = add_duration phases.output_verification timing.output_verification;
  store_save = add_duration phases.store_save timing.store_save;
  total = add_duration phases.total timing.total;
}

let status_label = fun __tmp1 ->
  match __tmp1 with
  | Action_execution.Cached _ -> "cached"
  | Action_execution.Executed _ -> "executed"
  | Action_execution.Failed _ -> "failed"

let add_count = fun (counts: counts) status ->
  match status with
  | Action_execution.Cached _ -> { counts with total = counts.total + 1; cached = counts.cached + 1 }
  | Action_execution.Executed _ -> { counts with total = counts.total + 1; executed = counts.executed + 1 }
  | Action_execution.Failed _ -> { counts with total = counts.total + 1; failed = counts.failed + 1 }

let add_result_to_group = fun result (group: group) -> {
  group with
  counts = add_count group.counts result.Action_execution.status;
  phases = add_timing group.phases result.timing;
}

let add_group = fun label result groups ->
  let rec loop acc = fun __tmp1 ->
    match __tmp1 with
    | [] ->
        let group =
          add_result_to_group result ({
            label;
            counts = empty_counts;
            phases = empty_phases;
          }: group)
        in
        List.reverse (group :: acc)
    | group :: rest when String.equal group.label label ->
        (List.reverse acc) @ (add_result_to_group result group :: rest)
    | group :: rest -> loop (group :: acc) rest
  in
  loop [] groups

let sort_groups = fun groups ->
  List.sort groups ~compare:(fun left right -> String.compare left.label right.label)

let for_package = fun package results ->
  List.filter
    results
    ~fn:(fun result -> Riot_model.Package_name.equal result.Action_execution.ref_.package package)

let of_results = fun results ->
  let summary =
    List.fold_left
      results
      ~init:{
        counts = empty_counts;
        phases = empty_phases;
        by_status = [];
        by_action_kind = [];
      }
      ~fn:(fun summary result -> {
        counts = add_count summary.counts result.Action_execution.status;
        phases = add_timing summary.phases result.timing;
        by_status = add_group (status_label result.status) result summary.by_status;
        by_action_kind = add_group result.action_kind result summary.by_action_kind;
      })
  in
  {
    summary with
    by_status = sort_groups summary.by_status;
    by_action_kind = sort_groups summary.by_action_kind;
  }

let duration_json = fun duration ->
  Data.Json.obj [
    ("nanos", Data.Json.int (Int64.to_int (Time.Duration.to_nanos duration)));
    ("millis", Data.Json.float (Int64.to_float (Time.Duration.to_nanos duration) /. 1_000_000.0));
  ]

let counts_json = fun (counts: counts) ->
  Data.Json.obj [
    ("total", Data.Json.int counts.total);
    ("cached", Data.Json.int counts.cached);
    ("executed", Data.Json.int counts.executed);
    ("failed", Data.Json.int counts.failed);
  ]

let phases_json = fun phases ->
  Data.Json.obj [
    ("dependency_hashing", duration_json phases.dependency_hashing);
    ("input_hashing", duration_json phases.input_hashing);
    ("store_lookup", duration_json phases.store_lookup);
    ("cache_promotion", duration_json phases.cache_promotion);
    ("sandbox_prepare", duration_json phases.sandbox_prepare);
    ("source_staging", duration_json phases.source_staging);
    ("command_execution", duration_json phases.command_execution);
    ("output_verification", duration_json phases.output_verification);
    ("store_save", duration_json phases.store_save);
    ("total", duration_json phases.total);
  ]

let group_json = fun group ->
  Data.Json.obj [
    ("label", Data.Json.string group.label);
    ("counts", counts_json group.counts);
    ("phases", phases_json group.phases);
  ]

let to_json = fun summary ->
  Data.Json.obj [
    ("counts", counts_json summary.counts);
    ("phases", phases_json summary.phases);
    ("by_status", Data.Json.array (List.map summary.by_status ~fn:group_json));
    ("by_action_kind", Data.Json.array (List.map summary.by_action_kind ~fn:group_json));
  ]
