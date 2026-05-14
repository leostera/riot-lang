open Std

module Test = Std.Test

open Riot_build2

let package = fun name ->
  Riot_model.Package_name.from_string name
  |> Result.expect ~msg:("invalid package name: " ^ name)

let ref_ = fun package hash ->
  Action_execution.{
    package;
    profile = Riot_model.Profile.debug;
    target = Riot_model.Target.current;
    hash = Crypto.hash_string hash;
  }

let artifact = fun hash ->
  Riot_store.Artifact.{
    input_hash = Crypto.hash_string ("input:" ^ hash);
    output_hash = Crypto.hash_string ("output:" ^ hash);
    size_bytes = 0L;
    files = [];
    ocamlc_warnings = [];
    exports = [];
  }

let timing = fun
  ?(dependency_hashing = Time.Duration.zero)
  ?(input_hashing = Time.Duration.zero)
  ?(store_lookup = Time.Duration.zero)
  ?(cache_promotion = Time.Duration.zero)
  ?(sandbox_prepare = Time.Duration.zero)
  ?(source_staging = Time.Duration.zero)
  ?(command_execution = Time.Duration.zero)
  ?(output_verification = Time.Duration.zero)
  ?(store_save = Time.Duration.zero)
  ~total
  () ->
  {
    Action_execution.dependency_hashing = dependency_hashing;
    input_hashing;
    store_lookup;
    cache_promotion;
    sandbox_prepare;
    source_staging;
    command_execution;
    output_verification;
    store_save;
    total;
  }

let result = fun ~package ~hash ~action_kind ~status ~timing ->
  Action_execution.{
    ref_ = ref_ package hash;
    action_kind;
    status;
    ocamlc_warnings = [];
    timing;
  }

let group = fun label groups ->
  List.find
    groups
    ~fn:(fun (item: Action_timing_summary.group) -> String.equal item.label label)

let duration_equal = Time.Duration.equal

let test_summary_counts_statuses_and_action_kinds = fun _ctx ->
  let kernel = package "kernel" in
  let other = package "other" in
  let results = [
    result
      ~package:kernel
      ~hash:"compiled-source"
      ~action_kind:"CompileSource"
      ~status:(Action_execution.Executed (artifact "compiled-source"))
      ~timing:(timing
        ~command_execution:(Time.Duration.from_millis 7)
        ~store_save:(Time.Duration.from_millis 2)
        ~total:(Time.Duration.from_millis 10)
        ());
    result
      ~package:kernel
      ~hash:"cached-c"
      ~action_kind:"CompileC"
      ~status:(Action_execution.Cached (artifact "cached-c"))
      ~timing:(timing
        ~store_lookup:(Time.Duration.from_millis 1)
        ~cache_promotion:(Time.Duration.from_millis 3)
        ~total:(Time.Duration.from_millis 4)
        ());
    result
      ~package:kernel
      ~hash:"failed-source"
      ~action_kind:"CompileSource"
      ~status:(Action_execution.Failed "compile failed")
      ~timing:(timing
        ~command_execution:(Time.Duration.from_millis 5)
        ~total:(Time.Duration.from_millis 6)
        ());
    result
      ~package:other
      ~hash:"other"
      ~action_kind:"CompileC"
      ~status:(Action_execution.Executed (artifact "other"))
      ~timing:(timing ~total:(Time.Duration.from_millis 100) ());
  ]
  in
  let summary =
    results
    |> Action_timing_summary.for_package kernel
    |> Action_timing_summary.of_results
  in
  if
    not
      (Int.equal summary.counts.total 3
      && Int.equal summary.counts.cached 1
      && Int.equal summary.counts.executed 1
      && Int.equal summary.counts.failed 1)
  then
    Error "expected status counts for kernel action timing summary"
  else
    match (group "CompileSource" summary.by_action_kind, group "cached" summary.by_status) with
    | (Some compile_source, Some cached) ->
        if not (Int.equal compile_source.counts.total 2) then
          Error "expected two CompileSource action results"
        else if
          not
            (duration_equal compile_source.phases.command_execution (Time.Duration.from_millis 12))
        then
          Error "expected CompileSource command execution totals to be aggregated"
        else if
          not (duration_equal cached.phases.cache_promotion (Time.Duration.from_millis 3))
        then
          Error "expected cached action promotion totals to be aggregated"
        else if not (duration_equal summary.phases.total (Time.Duration.from_millis 20)) then
          Error "expected package filter to exclude non-kernel action timings"
        else
          Ok ()
    | _ -> Error "expected action kind and status groups"

let tests =
  Test.[
    case "summary counts statuses and action kinds" test_summary_counts_statuses_and_action_kinds;
  ]

let main ~args = Test.Cli.main ~name:"action_timing_summary" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
