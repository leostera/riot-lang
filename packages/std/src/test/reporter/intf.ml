open Global

type suite_info = {
  name: string;
  source_file: string option;
  binary_path: string option;
}

module type Intf = sig
  val init: suite_info -> int -> unit

  val on_result: int -> Test_result.t -> unit

  val finalize: Test_result.summary -> unit
end
