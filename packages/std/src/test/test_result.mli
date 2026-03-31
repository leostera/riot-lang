type single_result =
  | Passed
  | Failed of string
  | Skipped
type t = {
  index: int;
  name: string;
  test_type: Test_case.test_type;
  result: single_result;
}
type summary = {
  total: int;
  passed: int;
  failed: int;
  skipped: int;
  results: t list;
}
val make_summary: t list -> summary
