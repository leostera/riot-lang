open Global

type suite_info = {
  name: string;
  source_file: Path.t option;
  binary_path: Path.t option;
  workspace_root: Path.t option;
  package_name: string option;
  built_binaries: Test_context.built_binary list;
}

module type Intf = sig
  val init: suite_info -> int -> unit

  val on_result: int -> Test_result.t -> unit

  val warn: string -> unit

  val finalize: Test_result.summary -> unit
end
