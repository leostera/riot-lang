open Std

type result = {
  initial: Source_runner.result;
  fixed_source: string option;
  applied_fixes: Fix.fix list;
  after: Source_runner.result option;
}
val run: rules:Rule.t list -> ?filename:Path.t -> string -> (result, string) Result.t

val run_rule: rule:Rule.t -> ?filename:Path.t -> string -> (result, string) Result.t
