open Global

type fixture = {
  path: Path.t;
  relpath: Path.t;
  name: string;
  snapshot_path: Path.t option;
}

type t = {
  suite_name: string;
  test_name: string;
  test_index: int;
  source_file: Path.t option;
  binary_path: Path.t option;
  workspace_root: Path.t option;
  package_name: string option;
  fixture: fixture option;
}

let with_fixture = fun ctx fixture -> { ctx with fixture = Some fixture }
