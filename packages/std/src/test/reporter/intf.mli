(** Metadata describing a test suite. *)
type suite_info = {
  (** Human-readable suite name. *)
  name: string;
  (** Source file for the suite, when known. *)
  source_file: Path.t option;
  (** Built test binary path, when known. *)
  binary_path: Path.t option;
  (** Workspace root for the suite, when known. *)
  workspace_root: Path.t option;
  (** Owning package name for the suite, when known. *)
  package_name: string option;
  (** Built runtime binaries for the owning package, when any. *)
  built_binaries: Test_context.built_binary list;
}

(**
   Reporter interface used by the test runner.

   ## Example

   ```ocaml
   module Reporter : Test.Reporter.Intf.Intf = struct
     let init suite_info total =
       Log.info "running %d tests for %s" total suite_info.name

     let on_result _index _result = ()

     let warn message = Log.warn message

     let finalize _summary = ()
   end
   ```
*)
module type Intf = sig
  (** Called once before any tests are executed. *)
  val init: suite_info -> int -> unit

  (** Called when a single test result is available. *)
  val on_result: int -> Test_result.t -> unit

  (** Called for non-fatal suite-level warnings. *)
  val warn: string -> unit

  (** Called once after all tests have completed. *)
  val finalize: Test_result.summary -> unit
end
