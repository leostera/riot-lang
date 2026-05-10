(**
   # Parse and load dotenv files

   This package reads dotenv-style configuration files, parses them into
   key/value bindings, and can apply those bindings to the process
   environment.

   ## Supported syntax

   - `KEY=value` assignments with optional spaces around the separator.
   - YAML-style `KEY: value` assignments.
   - `export KEY=value` prefixes, plus `export KEY` for keys already defined
     earlier in the file or in the process environment.
   - Single-quoted, double-quoted, unquoted, and multi-line quoted values.
   - Inline comments after whitespace, for example `KEY=value # comment`.
   - Variable substitution with `$KEY` and `${KEY}` in unquoted and
     double-quoted values.
   - UTF-8 BOM stripping and CRLF/CR line-ending normalization.

   Shell command substitution, such as `$(command)`, is deliberately not
   supported.

   The loader preserves existing process environment variables by default.
   Pass `~on_existing:Dotenv.OverwriteExisting` when dotenv values should replace
   existing values.
*)

(** A parsed dotenv assignment. *)
type binding = {
  (** Environment variable name, such as `DATABASE_URL`. *)
  key: string;
  (** Parsed value after unescaping and substitution. *)
  value: string;
  (** 1-based source line where the assignment started. *)
  line: int;
}
(** How loaders handle variables already present in the process environment. *)
type existing =
  (**
     Keep existing process environment values. Dotenv bindings for those
     keys are skipped when applying values.
  *)
  | PreserveExisting
  (** Replace process environment values with dotenv bindings. *)
  | OverwriteExisting
(** How file-loading functions handle missing files. *)
type missing =
  (** Skip missing files. Use this for optional local/profile files. *)
  | SkipMissing
  (** Return a `ReadError` for the first missing file. *)
  | FailMissing
(** Errors returned by parsing and loading functions. *)
type error =
  (**
     A dotenv file could not be read.

     - `path` is the requested path.
     - `reason` is a human-readable IO failure.
  *)
  | ReadError of {
      (** Requested dotenv path. *)
      path: Std.Path.t;
      (** Human-readable IO failure. *)
      reason: string;
    }
  (** Dotenv source text could not be parsed. *)
  | ParseError of {
      (** 1-based source line where parsing failed. *)
      line: int;
      (** Human-readable parse failure. *)
      message: string;
    }

(**
   Telemetry events emitted while parsing and loading dotenv files.
*)
module Events: module type of Events

(** Convert an error into a stable human-readable message. *)
val error_to_string: error -> string

(**
   Parse dotenv source text into bindings.

   `parse content` does not mutate the process environment. It may read the
   process environment to resolve substitutions. Values parsed earlier in
   `content` are also available to later substitutions, but process
   environment values take precedence during substitution.

   ```ocaml
   let bindings =
     match Dotenv.parse "HOST=localhost\nURL=http://$HOST:8080" with
     | Ok bindings -> bindings
     | Error error ->
         Std.eprintln (Dotenv.error_to_string error);
         []
   ```
*)
val parse: string -> (binding list, error) Std.Result.t

(**
   Parse dotenv files without applying them to the process environment.

   Files are parsed from left to right and the returned binding list preserves
   that order. Missing files are skipped by default; pass
   `~on_missing:Dotenv.FailMissing` to return a `ReadError` for the first missing file.
*)
val parse_files: ?on_missing:missing -> Std.Path.t list -> (binding list, error) Std.Result.t

(**
   Apply bindings to the process environment.

   Existing process environment variables are preserved by default, including
   variables set to the empty string. Pass `~on_existing:Dotenv.OverwriteExisting` to
   replace existing values.
*)
val apply: ?on_existing:existing -> binding list -> unit

(**
   Parse dotenv source text and apply the resulting bindings.

   The returned list contains the bindings that were actually applied. With
   the default `~on_existing:Dotenv.PreserveExisting`, bindings whose keys already
   exist in the process environment are skipped and omitted from the returned
   list.
*)
val load_string: ?on_existing:existing -> string -> (binding list, error) Std.Result.t

(**
   Load dotenv files and apply their bindings.

   Files have first-file precedence. For example:

   ```ocaml
   Dotenv.load_files [ Std.Path.v "important.env"; Std.Path.v ".env" ]
   ```

   That keeps values from `important.env` when both files define the same key.
   This remains true with `~on_existing:Dotenv.OverwriteExisting`; lower-priority files
   are applied first, then higher-priority files overwrite them.

   Missing files are skipped by default. Pass `~on_missing:Dotenv.FailMissing` to fail
   on the first missing file.

   The returned list contains bindings that were actually applied. When
   `~on_existing:Dotenv.OverwriteExisting` is used and multiple files define the same
   key, duplicate keys may appear because each assignment was applied in
   sequence.
*)
val load_files:
  ?on_existing:existing ->
  ?on_missing:missing ->
  Std.Path.t list ->
  (binding list, error) Std.Result.t

(**
   Return the load order for a base dotenv path and optional environment.

   `env_paths ()` returns:

   ```ocaml
   [ Std.Path.v ".env" ]
   ```

   `env_paths ~env:"test" ()` returns:

   ```ocaml
   [ Std.Path.v ".env.test"; Std.Path.v ".env" ]
   ```

   `env_paths ~path:(Std.Path.v "config/.env") ~env:"local" ()` returns:

   ```ocaml
   [ Std.Path.v "config/.env.local"; Std.Path.v "config/.env" ]
   ```

   Profile files have higher precedence than the base file.
*)
val env_paths: ?path:Std.Path.t -> ?env:string -> unit -> Std.Path.t list

(**
   Load dotenv files from the standard base/profile path set.

   `load ()` loads `.env` and returns a `ReadError` if it is missing.

   `load ~env:"test" ()` tries `.env.test` and `.env`, skipping individual
   missing files but requiring at least one of them to exist. Profile files
   have precedence over the base file.

   `path` changes the base path used to derive the optional profile path.
*)
val load:
  ?path:Std.Path.t ->
  ?env:string ->
  ?on_existing:existing ->
  unit ->
  (binding list, error) Std.Result.t

(**
   Load dotenv files from the standard base/profile path set if present.

   This is the non-failing variant of `load`. Missing files are skipped, and
   `Ok []` is returned when none of the candidate files exist.
*)
val load_if_exists:
  ?path:Std.Path.t ->
  ?env:string ->
  ?on_existing:existing ->
  unit ->
  (binding list, error) Std.Result.t
