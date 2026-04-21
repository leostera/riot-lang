open Global

type fixture = {
  path: Path.t;
  relpath: Path.t;
  name: string;
  snapshot_path: Path.t option;
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
      pending_path: Path.t option
    }
  | SnapshotAssertionMatched of {
      mode: snapshot_mode;
      format: snapshot_format;
      approved_path: Path.t option
    }
  | SnapshotAssertionMismatch of {
      mode: snapshot_mode;
      format: snapshot_format;
      approved_path: Path.t option;
      pending_path: Path.t option;
      reason: snapshot_mismatch_reason
    }

type progress_handler = progress -> unit

type t = {
  suite_name: string;
  test_name: string;
  test_index: int;
  source_file: Path.t option;
  binary_path: Path.t option;
  workspace_root: Path.t option;
  package_name: string option;
  fixture: fixture option;
  progress_handler: progress_handler;
}

let no_progress_handler: progress_handler = fun _ -> ()

let with_fixture = fun ctx fixture -> { ctx with fixture = Some fixture }

let with_progress_handler = fun ctx progress_handler -> { ctx with progress_handler }

let emit_progress = fun ctx progress -> ctx.progress_handler progress
