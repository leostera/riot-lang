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

val empty_counts: counts

val empty_phases: phase_totals

val for_package:
  Riot_model.Package_name.t ->
  Action_execution.result list ->
  Action_execution.result list

val of_results: Action_execution.result list -> t

val to_json: t -> Data.Json.t
