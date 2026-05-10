(**
   # Dotenv file loading

   Loads one or more dotenv files, optionally applying parsed values to the
   process environment.
*)

(**
   Return the candidate load order for a base dotenv path and optional
   environment profile.

   Profile paths are returned before the base path so they have higher
   precedence.
*)
val env_paths: ?path:Std.Path.t -> ?env:string -> unit -> Std.Path.t list

(**
   Parse dotenv files without applying them to the process environment.

   Missing files are skipped by default. Pass `~on_missing:Types.FailMissing`
   to fail on the first missing path.
*)
val parse_files:
  ?on_missing:Types.missing ->
  Std.Path.t list ->
  (Types.binding list, Types.error) Std.Result.t

(**
   Parse source text and apply the resulting bindings to the process
   environment.
*)
val load_string:
  ?on_existing:Types.existing ->
  string ->
  (Types.binding list, Types.error) Std.Result.t

(**
   Load dotenv files and apply their bindings.

   Files have first-file precedence. Missing files are skipped by default.
*)
val load_files:
  ?on_existing:Types.existing ->
  ?on_missing:Types.missing ->
  Std.Path.t list ->
  (Types.binding list, Types.error) Std.Result.t

(**
   Load `.env` and optional `.env.{ENV}` files.

   `load ()` requires `.env`. `load ~env ()` requires at least one candidate
   file and skips missing individual candidates.
*)
val load:
  ?path:Std.Path.t ->
  ?env:string ->
  ?on_existing:Types.existing ->
  unit ->
  (Types.binding list, Types.error) Std.Result.t

(**
   Load `.env` and optional `.env.{ENV}` files when present.

   Missing candidates are skipped and `Ok []` is returned when none exist.
*)
val load_if_exists:
  ?path:Std.Path.t ->
  ?env:string ->
  ?on_existing:Types.existing ->
  unit ->
  (Types.binding list, Types.error) Std.Result.t
