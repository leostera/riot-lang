open Std

(** Repository-local operational config loaded from [.riot/config.toml]. *)
type cache_policy = {
  keep_generations: int;
  max_size_bytes: int64;
}
type test_policy = {
  small_test_timeout: Time.Duration.t option;
  flaky_max_retries: int;
}
type t = {
  cache: cache_policy;
  test: test_policy;
}
type value_error =
  | MissingNumberPrefix
  | UnsupportedUnit of string
  | InvalidNumber of string
  | NegativeValue
type cache_error =
  | KeepGenerationsMustBePositiveInt
  | MaxSizeMustBeString
  | InvalidMaxSize of value_error
type test_error =
  | SmallTestTimeoutMustBeDurationString
  | SmallTestTimeoutMustBeNonNegativeInt
  | InvalidSmallTestTimeout of value_error
  | FlakyMaxRetriesMustBeNonNegativeInt
  | FlakyMaxRetriesMustBeInt
type invalid_config_error =
  | RiotMustBeTable
  | RiotCacheMustBeTable
  | RiotTestMustBeTable
  | CacheConfig of cache_error
  | TestConfig of test_error
type error =
  | ReadFailed of { path: Path.t; error: IO.error }
  | ParseFailed of { path: Path.t; error: Std.Data.Toml.error }
  | InvalidConfig of { path: Path.t; error: invalid_config_error }
val default_cache_policy: cache_policy

val default_test_policy: test_policy

val default: t

val message: error -> string

val load: workspace_root:Path.t -> (t, error) result
