(** Result of an individual test case. *)
type single_result =
  | Passed
  | Failed of string
  | Skipped
(** A test result tagged with its index and name. *)
type t = {
  (** Position of the test in the run. *)
  index: int;
  (** Human-readable test name. *)
  name: string;
  (** Test kind, such as unit or snapshot. *)
  test_type: Test_case.test_type;
  (** Outcome for this test. *)
  result: single_result;
  (** Time spent executing this test case. *)
  duration: Time.Duration.t;
}
(** Summary of all test results in a run. *)
type summary = {
  (** Total number of tests considered. *)
  total: int;
  (** Number of passing tests. *)
  passed: int;
  (** Number of failing tests. *)
  failed: int;
  (** Number of skipped tests. *)
  skipped: int;
  (** Original per-test results included in the summary. *)
  results: t list;
  (** Total wall-clock execution time across matched tests. *)
  duration: Time.Duration.t;
}

(** [make_summary results] creates a summary from individual test results.

    ## Example

    ```ocaml
    let summary = Test_result.make_summary results
    ```
*)
val make_summary: t list -> summary
