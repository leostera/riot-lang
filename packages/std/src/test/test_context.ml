open Global
open Collections

type fixture = {
  path: Path.t;
  relpath: Path.t;
  name: string;
  snapshot_path: Path.t option;
}

type built_binary = {
  name: string;
  path: Path.t;
}

type snapshot_mode =
  | External
  | Inline

type snapshot_format =
  | Text
  | Json

type snapshot_mismatch_reason =
  | Missing_approved
  | Pending_exists
  | Mismatch

type progress =
  | PropertyIterationPassed of { current: int; total: int; size: int }
  | PropertyAssumptionRejected of { current: int; total: int; size: int; rejected_count: int }
  | PropertyCounterExampleFound of { current: int; total: int; size: int }
  | PropertyShrinkStep of { current: int; total: int; step: int; max_steps: int }
  | SnapshotAssertionStarted of {
      mode: snapshot_mode;
      format: snapshot_format;
      approved_path: Path.t option;
      pending_path: Path.t option;
    }
  | SnapshotAssertionMatched of {
      mode: snapshot_mode;
      format: snapshot_format;
      approved_path: Path.t option;
    }
  | SnapshotAssertionMismatch of {
      mode: snapshot_mode;
      format: snapshot_format;
      approved_path: Path.t option;
      pending_path: Path.t option;
      reason: snapshot_mismatch_reason;
    }

type progress_handler = progress -> unit

module Store = struct
  type t = TypedKeyHashMap.t

  type 'a key = 'a TypedKeyHashMap.key

  let create = TypedKeyHashMap.create

  let key = TypedKeyHashMap.key

  let insert = fun store key value -> TypedKeyHashMap.insert store ~key ~value

  let get = fun store key -> TypedKeyHashMap.get store ~key

  let remove = fun store key -> TypedKeyHashMap.remove store ~key
end

type 'a key = 'a Store.key

let key = Store.key

type t = {
  suite_name: string;
  context_store: Store.t;
  test_name: string;
  test_index: int;
  source_file: Path.t option;
  binary_path: Path.t option;
  built_binaries: built_binary list;
  workspace_root: Path.t option;
  package_name: string option;
  fixture: fixture option;
  progress_handler: progress_handler;
}

let no_progress_handler: progress_handler = fun _ -> ()

let with_fixture = fun ctx fixture -> { ctx with fixture = Some fixture }

let with_progress_handler = fun ctx progress_handler -> { ctx with progress_handler }

let emit_progress = fun ctx progress -> ctx.progress_handler progress

let get = fun ctx key -> Store.get ctx.context_store key

let find_binary = fun ctx name ->
  List.find ctx.built_binaries ~fn:(fun (binary: built_binary) -> String.equal binary.name name)
  |> Option.map ~fn:(fun binary -> binary.path)

let require_binary = fun ctx name ->
  match find_binary ctx name with
  | Some path -> Ok path
  | None ->
      let package_prefix =
        match ctx.package_name with
        | Some package_name -> "package '" ^ package_name ^ "' "
        | None -> ""
      in
      let available =
        match List.map ctx.built_binaries ~fn:(fun (binary: built_binary) -> binary.name) with
        | [] -> "none"
        | names -> String.concat ", " names
      in
      Error ("required built binary '"
      ^ name
      ^ "' was not available for "
      ^ package_prefix
      ^ "(available: "
      ^ available
      ^ ")")
