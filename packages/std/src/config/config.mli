(**
   Unified configuration management.

   Type-safe, validated configuration loading for Riot applications with automatic
   registration and fail-fast semantics.

   ## Features

   - **Type-safe schema DSL** - Define configuration schemas with built-in validation
   - **Automatic registration** - Specs register themselves globally at module load time
   - **Fail-fast validation** - Configuration errors caught at startup, not runtime
   - **Environment-aware** - Automatically loads `./config/{dev,test,prod}.toml`
   - **TOML-based** - Human-friendly configuration format with `[app_name]` sections
   - **Rich type support** - String, Int, Bool, Float, URI, DateTime, Path, UUID, and nested maps

   ## Quick Start

   ```ocaml
   open Std

   (* 1. Define your config module - spec auto-registers! *)
   module MyAppConfig = struct
     let spec = Config.Spec.for_app ~app:"myapp" [
       Config.Spec.key "server" (Config.Spec.map [
         Config.Spec.string "host" ~default:"localhost" ~help:"Server hostname";
         Config.Spec.int "port" ~default:4000 ~help:"Server port number";
       ]);
       Config.Spec.string "log_level" ~default:"info" ~help:"Logging level";
     ]

     type server = { host : string; port : int }
     type t = { server : server; log_level : string }

     let get conf =
       let server_conf = Config.get_map conf "server" in
       let host = Config.get_string server_conf "host" in
       let port = Config.get_int server_conf "port" in
       let log_level = Config.get_string conf "log_level" in
       Ok { server = { host; port }; log_level }
   end

   (* 2. Add to your supervisor - loads ALL registered specs *)
   let children = [
     Config.child_spec ();
     (* other children *)
   ]

   (* 3. Retrieve config anywhere in your app *)
   let start () =
     match Config.get (module MyAppConfig) with
     | Ok config ->
         Log.info "Server: %s:%d" config.server.host config.server.port
     | Error err ->
         Log.error "Config error: %s" (Config.error_to_string err)
   ```

   ## Configuration Files

   Create `./config/dev.toml` (or `test.toml`, `prod.toml`):

   ```toml
   [myapp]
   log_level = "debug"

   [myapp.server]
   host = "0.0.0.0"
   port = 8080
   ```

   The environment is auto-detected from `RIOT_ENV` (or defaults to `dev`).
*)
open Global

type error =
  | NotFound of { app: string }
  (** Config section not found in TOML file *)
  | ValidationError of {
      app: string;
      errors: string list;
    }
  (** Configuration validation failed *)
  | ParseError of { path: string; message: string }
  (** TOML parsing error *)
  | FileNotFound of { path: string }

(** Configuration file not found *)
module Provider: sig
  type t

  (** Configuration source - environment-based path, explicit path, or static TOML string *)
  val env: ?env:Loader.env -> unit -> t

  (**
     Load from file based on environment (RIOT_ENV or default to dev).

     Example:
     ```ocaml
     Provider.env ()  (* Loads ./config/dev.toml based on RIOT_ENV *)
     Provider.env ~env:Loader.Prod ()  (* Explicitly load ./config/prod.toml *)
     ```
  *)
  val file: Path.t -> t

  (**
     Load from explicit file path.

     Example:
     ```ocaml
     Provider.file (Path.from_string "./my-custom-config.toml" |> Result.expect "invalid path")
     Provider.file (Path.from_string "/etc/myapp/config.toml" |> Result.expect "invalid path")
     ```
  *)
  val static: string -> t

  (**
     Load from inline TOML string (useful for tests and examples).

     Example:
     ```ocaml
     Provider.static {|
       [myapp]
       log_level = "debug"
       port = 8080
     |}
     ```
  *)
end

module Spec = Spec

(** Configuration schema DSL. See {!Spec} for the complete API. *)
module type ConfigSpec = sig
  val spec: Spec.t

  (** The configuration schema *)
  type t

  (** Your application's configuration type *)
  val get: Spec.value -> (t, error) result

  (**
     Extract your config type from validated values.
     Use the helper functions like {!get_string}, {!get_int}, etc.
  *)
end

(**
   Signature for user configuration modules.

   Example:
   ```ocaml
   module MyConfig : Config.ConfigSpec = struct
     let spec = Config.Spec.for_app ~app:"myapp" [...]
     type t = { host: string; port: int }
     let get conf = ...
   end
   ```
*)
val load: ?provider:Provider.t -> unit -> unit

(**
   Load configuration into the global config state.

   Defaults to loading from file based on RIOT_ENV environment variable.
   Must be called before {!get} can be used.

   Example:
   ```ocaml
   (* Load with inline config *)
   Config.load ~provider:(Config.Provider.static {|
     [myapp]
     debug = true
     port = 8080
   |}) ()

   (* Or load from file *)
   Config.load ()  (* Uses RIOT_ENV - loads ./config/dev.toml *)

   (* Or load from custom path *)
   let path = Path.from_string "/etc/myapp/config.toml" |> Result.expect "path" in
   Config.load ~provider:(Config.Provider.file path) ()
   ```
*)
val load_string: string -> unit

(**
   Load configuration from inline TOML string.

   Convenience function equivalent to [load ~provider:(Provider.static str) ()].

   Example:
   ```ocaml
   Config.load_string {|
     [myapp]
     port = 8080
   |}
   ```
*)
val load_file: Path.t -> unit

(**
   Load configuration from specific file path.

   Convenience function equivalent to [load ~provider:(Provider.file path) ()].

   Example:
   ```ocaml
   let path = Path.from_string "./my-config.toml" |> Result.expect "path" in
   Config.load_file path
   ```
*)
val get: (module ConfigSpec with type t = 'a) -> ('a, error) result

(**
   Retrieve validated configuration from the server.

   This should be called after the config server has started (typically
   in your application's [start] function or process initialization).

   Example:
   ```ocaml
   match Config.get (module MyAppConfig) with
   | Ok config ->
       (* Use your typed config *)
       Log.info "Port: %d" config.port
   | Error (NotFound { app }) ->
       Log.error "Config for '%s' not found" app
   | Error err ->
       Log.error "%s" (Config.error_to_string err)
   ```

   Returns [NotFound] if the config wasn't loaded (check your [child_spec]).
*)
val error_to_string: error -> string

(**
   Convert a configuration error to a human-readable string.

   Example:
   ```ocaml
   | Error err -> Log.error "Config error: %s" (Config.error_to_string err)
   ```
*)
val reload: ?provider:Provider.t -> unit -> (unit, string) result

(**
   Hot reload configuration at runtime.

   If [provider] is given, uses that source. Otherwise reloads from
   the same provider used at startup.

   This is useful for:
   - Reloading configuration after file changes
   - Switching configuration sources dynamically
   - Testing different configurations

   Example:
   ```ocaml
   (* Reload from file *)
   Config.reload () |> Result.expect "reload failed"

   (* Reload from new static config *)
   Config.reload ~provider:(Provider.static {|
     [myapp]
     log_level = "debug"
   |}) () |> Result.expect "reload failed"
   ```

   Returns [Error] if the config server is not running.
*)
val patch: app:string -> (string * Spec.value) list -> (unit, string) result

(**
   Patch specific config values at runtime (useful for testing/debugging).

   This allows you to override specific configuration values without
   reloading the entire configuration. Only works on Map values.

   Example:
   ```ocaml
   (* Temporarily increase log verbosity *)
   Config.patch ~app:"myapp" [
     ("log_level", Spec.String "debug");
     ("port", Spec.Int 9999);
   ] |> Result.expect "patch failed"
   ```

   Returns [Error] if:
   - The config server is not running
   - The app is not found
   - The config value is not a Map

   @raise Panic if attempting to patch a non-Map value
*)
(**
   {1 Value Extraction Helpers}

   These functions extract values from {!Spec.value} in your [ConfigSpec.get]
   implementation. They **panic** on type mismatches or missing keys, ensuring
   configuration errors are caught at startup rather than runtime.

   ## Extraction Patterns

   **Map fields:**
   ```ocaml
   let host = Config.get_string conf "host" in
   let port = Config.get_int conf "port" in
   ```

   **Nested maps:**
   ```ocaml
   let server = Config.get_map conf "server" in
   let host = Config.get_string server "host" in
   ```

   **Direct value extraction:**
   ```ocaml
   let port = Config.as_int port_value in
   ```
*)

val get_string: Spec.value -> string -> string

(**
   Extract a string value from a map.

   Example: [let host = Config.get_string conf "host"]

   @raise Panic if key not found or value is not a string
*)
val get_char: Spec.value -> string -> char

(**
   Extract a char value from a map.

   Example: [let delimiter = Config.get_char conf "delimiter"]

   @raise Panic if key not found or value is not a char
*)
val get_int: Spec.value -> string -> int

(**
   Extract an int value from a map.

   Example: [let port = Config.get_int conf "port"]

   @raise Panic if key not found or value is not an int
*)
val get_int32: Spec.value -> string -> int32

(**
   Extract an int32 value from a map.

   Example: [let size = Config.get_int32 conf "max_size"]

   @raise Panic if key not found or value is not an int32
*)
val get_int64: Spec.value -> string -> int64

(**
   Extract an int64 value from a map.

   Example: [let timestamp = Config.get_int64 conf "created_at"]

   @raise Panic if key not found or value is not an int64
*)
val get_bool: Spec.value -> string -> bool

(**
   Extract a bool value from a map.

   Example: [let debug = Config.get_bool conf "debug"]

   @raise Panic if key not found or value is not a bool
*)
val get_float: Spec.value -> string -> float

(**
   Extract a float value from a map.

   Example: [let rate = Config.get_float conf "sample_rate"]

   @raise Panic if key not found or value is not a float
*)
val get_uri: Spec.value -> string -> Net.Uri.t

(**
   Extract a URI value from a map.

   Example: [let api_url = Config.get_uri conf "api_endpoint"]

   @raise Panic if key not found or value is not a URI
*)
val get_datetime: Spec.value -> string -> DateTime.t

(**
   Extract a datetime value from a map.

   Example: [let created = Config.get_datetime conf "created_at"]

   @raise Panic if key not found or value is not a datetime
*)
val get_path: Spec.value -> string -> Path.t

(**
   Extract a path value from a map.

   Example: [let config_dir = Config.get_path conf "config_dir"]

   @raise Panic if key not found or value is not a path
*)
val get_uuid: Spec.value -> string -> Uuid.t

(**
   Extract a UUID value from a map.

   Example: [let instance_id = Config.get_uuid conf "instance_id"]

   @raise Panic if key not found or value is not a UUID
*)
val get_list: Spec.value -> string -> Spec.value list

(**
   Extract a list value from a map.

   Example:
   ```ocaml
   let handlers = Config.get_list conf "handlers" in
   List.map parse_handler handlers
   ```

   @raise Panic if key not found or value is not a list
*)
val get_discriminated_union: Spec.value -> string -> string * string * (string * Spec.value) list

(**
   Extract a discriminated union from a map by key.

   Returns [(discriminant_name, variant_value, fields)].

   Example:
   ```ocaml
   let (_, variant, fields) = Config.get_discriminated_union conf "handler" in
   match variant with
   | "console" -> parse_console fields
   | "file" -> parse_file fields
   | v -> panic ("Unknown handler type: " ^ v)
   ```

   @raise Panic if key not found or value is not a discriminated union
*)
val get_map: Spec.value -> string -> Spec.value

(**
   Extract a nested map value from a map.

   Example:
   ```ocaml
   let server = Config.get_map conf "server" in
   let host = Config.get_string server "host"
   ```

   @raise Panic if key not found or value is not a map
*)
(**
   {2 Direct Value Extractors}

   These functions extract values directly without looking up a key.
   Useful when you already have a {!Spec.value} to unwrap.
*)

val as_string: Spec.value -> string

(**
   Extract the string from a String value.

   @raise Panic if the value is not a string
*)
val as_char: Spec.value -> char

(**
   Extract the char from a Char value.

   @raise Panic if the value is not a char
*)
val as_int: Spec.value -> int

(**
   Extract the int from an Int value.

   @raise Panic if the value is not an int
*)
val as_int32: Spec.value -> int32

(**
   Extract the int32 from an Int32 value.

   @raise Panic if the value is not an int32
*)
val as_int64: Spec.value -> int64

(**
   Extract the int64 from an Int64 value.

   @raise Panic if the value is not an int64
*)
val as_bool: Spec.value -> bool

(**
   Extract the bool from a Bool value.

   @raise Panic if the value is not a bool
*)
val as_float: Spec.value -> float

(**
   Extract the float from a Float value.

   @raise Panic if the value is not a float
*)
val as_uri: Spec.value -> Net.Uri.t

(**
   Extract the URI from a Uri value.

   @raise Panic if the value is not a URI
*)
val as_datetime: Spec.value -> DateTime.t

(**
   Extract the datetime from a DateTime value.

   @raise Panic if the value is not a datetime
*)
val as_path: Spec.value -> Path.t

(**
   Extract the path from a Path value.

   @raise Panic if the value is not a path
*)
val as_uuid: Spec.value -> Uuid.t

(**
   Extract the UUID from a Uuid value.

   @raise Panic if the value is not a UUID
*)
val as_list: Spec.value -> Spec.value list

(**
   Extract the items from a List value.

   @raise Panic if the value is not a List
*)
val as_discriminated_union: Spec.value -> string * string * (string * Spec.value) list

(**
   Extract (discriminant_name, variant_value, fields) from a DiscriminatedUnion.

   Example:
   ```ocaml
   match Config.as_discriminated_union handler_value with
   | (_, "console", fields) -> parse_console fields
   | (_, "file", fields) -> parse_file fields
   | (_, variant, _) -> panic ("Unknown handler type: " ^ variant)
   ```

   @raise Panic if value is not a DiscriminatedUnion
*)
val as_map: Spec.value -> (string * Spec.value) list

(**
   Extract the field list from a Map value.

   @raise Panic if the value is not a map
*)
(**
   {1 Internal Modules}

   These modules are exposed for testing and advanced use cases.
   Most users won't need to interact with them directly.
*)

module Loader = Loader

(** Configuration file loading and environment detection *)
module Validator = Validator

(** Configuration validation against specs *)
