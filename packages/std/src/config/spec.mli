(**
   # Config.Spec - Configuration Schema DSL

   Type-safe schema definitions for configuration validation with support for
   defaults, required fields, nested maps, and comprehensive documentation.

   ## Overview

   The Spec DSL allows you to define what your configuration should look like,
   including types, defaults, required fields, and help text. Specs are automatically
   registered globally when created, so you just define them and the Config system
   handles the rest.

   ## Supported Types

   - **Primitives**: `string`, `char`, `int`, `int32`, `int64`, `bool`, `float`
   - **Rich types**: `uri`, `datetime`, `path`, `uuid`
   - **Composite**: `map` for nested configuration objects

   ## Example

   ```ocaml
   let spec = Config.Spec.for_app ~app:"myapp" [
     Config.Spec.string "env" ~default:"development" ~help:"Environment name";

     Config.Spec.key "database" (Config.Spec.map [
       Config.Spec.uri "url" ~required:true ~help:"Database connection URL";
       Config.Spec.int "pool_size" ~default:10 ~help:"Connection pool size";
       Config.Spec.int "timeout" ~default:5000 ~help:"Query timeout in ms";
     ]);

     Config.Spec.key "server" (Config.Spec.map [
       Config.Spec.string "host" ~default:"0.0.0.0";
       Config.Spec.int "port" ~default:4000;
       Config.Spec.bool "ssl" ~default:false;
     ]);

     Config.Spec.path "log_dir" ~default:(Path.of_string "./logs" |> Result.get_ok);
     Config.Spec.string "log_level" ~default:"info";
   ]
   ```

   ## TOML Configuration

   The above spec expects configuration like:

   ```toml
   [myapp]
   env = "production"
   log_level = "warn"
   log_dir = "/var/log/myapp"

   [myapp.database]
   url = "postgresql://localhost/myapp"
   pool_size = 20
   timeout = 10000

   [myapp.server]
   host = "127.0.0.1"
   port = 443
   ssl = true
   ```

   ## Automatic Registration

   When you call `for_app`, the spec is automatically registered in a global registry.
   Later, when `Config.child_spec ()` is called, all registered specs are loaded
   and validated. This "automagic" behavior eliminates boilerplate!

   ## Validation

   - **Type checking**: Values are validated against their declared types
   - **Required fields**: Missing required fields cause startup failure
   - **Defaults**: Applied when fields are missing
   - **Nested maps**: Recursively validated
*)

open Global

(** {1 Configuration Values} *)

type value =
  | String of string
  (** UTF-8 text value *)
  | Char of char
  (** Single character *)
  | Int of int
  (** Native integer (31/63-bit depending on platform) *)
  | Int32 of int32
  (** 32-bit signed integer *)
  | Int64 of int64
  (** 64-bit signed integer *)
  | Bool of bool
  (** Boolean value *)
  | Float of float
  (** IEEE 754 double-precision floating point *)
  | Uri of Net.Uri.t
  (** Parsed URI (e.g., "https://api.example.com/v1") *)
  | DateTime of DateTime.t
  (** Parsed datetime (ISO 8601 format) *)
  | Path of Path.t
  (** File system path *)
  | Uuid of Uuid.t
  (** UUID (e.g., "550e8400-e29b-41d4-a716-446655440000") *)
  | List of value list
  (** Array of homogeneous values *)
  | DiscriminatedUnion of {
      discriminant: string;
      variant: string;
      fields: (string * value) list;
    }
  (** Discriminated union - structure determined by discriminant field *)
  | Map of (string * value) list

(** Nested configuration object *)
(** {1 Schema Definition} *)

(** A field specification with type, defaults, and constraints *)
type field_spec
(** A complete configuration specification for an application *)
type t

(**
   Create and **automatically register** a configuration spec for an application.

   The [app] parameter must match the TOML section name (e.g., [\[myapp\]]).

   Example:
   ```ocaml
   let spec = Config.Spec.for_app ~app:"myapp" [
     Config.Spec.string "host" ~default:"localhost";
     Config.Spec.int "port" ~default:4000;
   ]
   ```

   Registration happens at module load time, so this should be a top-level
   definition in your config module.

   @param app The application name (must match TOML section)
   @param fields List of field specifications
   @return A registered configuration spec
*)
val for_app: app:string -> field_spec list -> t

(** {1 Field Types} *)

(**
   Define a string field.

   Strings can contain any UTF-8 text. In TOML, they're written as:
   - Basic strings: ["hello"]
   - Multi-line strings: ["""hello\nworld"""]

   Example:
   ```ocaml
   string "host" ~default:"localhost" ~help:"Server hostname or IP address"
   ```

   @param default Default value if field is missing
   @param required If [true], field must be present (default: [false])
   @param help Human-readable description for documentation
   @param name Field name in the configuration
*)
val string: ?default:string -> ?required:bool -> ?help:string -> string -> field_spec

(**
   Define a single character field.

   In TOML, written as a single-character string: ["x"]

   Example:
   ```ocaml
   char "delimiter" ~default:',' ~help:"CSV delimiter character"
   ```

   @param default Default character if field is missing
   @param required If [true], field must be present (default: [false])
   @param help Human-readable description
   @param name Field name
*)
val char: ?default:char -> ?required:bool -> ?help:string -> string -> field_spec

(**
   Define an integer field (native int: 31-bit on 32-bit systems, 63-bit on 64-bit).

   In TOML: [port = 8080]

   Example:
   ```ocaml
   int "port" ~default:4000 ~help:"Server port number (1-65535)"
   ```

   @param default Default value if field is missing
   @param required If [true], field must be present (default: [false])
   @param help Human-readable description
   @param name Field name
*)
val int: ?default:int -> ?required:bool -> ?help:string -> string -> field_spec

(**
   Define a 32-bit signed integer field.

   Useful for values that must fit in 32 bits for interop or wire formats.

   In TOML: [max_size = 2147483647]

   Example:
   ```ocaml
   int32 "max_connections" ~default:1000000l ~help:"Maximum concurrent connections"
   ```

   @param default Default value if field is missing
   @param required If [true], field must be present (default: [false])
   @param help Human-readable description
   @param name Field name
*)
val int32: ?default:int32 -> ?required:bool -> ?help:string -> string -> field_spec

(**
   Define a 64-bit signed integer field.

   Useful for large numbers like timestamps, file sizes, or database IDs.

   In TOML: [max_file_size = 10737418240]

   Example:
   ```ocaml
   int64 "max_bytes" ~default:10_737_418_240L ~help:"Maximum file size in bytes (10GB)"
   ```

   @param default Default value if field is missing
   @param required If [true], field must be present (default: [false])
   @param help Human-readable description
   @param name Field name
*)
val int64: ?default:int64 -> ?required:bool -> ?help:string -> string -> field_spec

(**
   Define a boolean field.

   In TOML: [debug = true] or [ssl = false]

   Example:
   ```ocaml
   bool "debug" ~default:false ~help:"Enable debug logging"
   ```

   @param default Default value if field is missing
   @param required If [true], field must be present (default: [false])
   @param help Human-readable description
   @param name Field name
*)
val bool: ?default:bool -> ?required:bool -> ?help:string -> string -> field_spec

(**
   Define a floating-point field (IEEE 754 double-precision).

   In TOML: [rate = 0.95] or [pi = 3.14159]

   Example:
   ```ocaml
   float "sample_rate" ~default:1.0 ~help:"Sampling rate (0.0-1.0)"
   ```

   @param default Default value if field is missing
   @param required If [true], field must be present (default: [false])
   @param help Human-readable description
   @param name Field name
*)
val float: ?default:float -> ?required:bool -> ?help:string -> string -> field_spec

(**
   Define a URI field.

   URIs are parsed and validated. Supports HTTP, HTTPS, and other schemes.

   In TOML: [api_url = "https://api.example.com/v1"]

   Example:
   ```ocaml
   uri "api_endpoint" ~required:true ~help:"API base URL (must be HTTPS)"
   ```

   @param default Default URI if field is missing
   @param required If [true], field must be present (default: [false])
   @param help Human-readable description
   @param name Field name
*)
val uri: ?default:Net.Uri.t -> ?required:bool -> ?help:string -> string -> field_spec

(**
   Define a datetime field.

   Datetimes must be in ISO 8601 format: ["2025-11-21T19:30:00Z"]

   In TOML: [created_at = "2025-11-21T19:30:00Z"]

   Example:
   ```ocaml
   datetime "deployed_at" ~required:true ~help:"Deployment timestamp (ISO 8601)"
   ```

   @param default Default datetime if field is missing
   @param required If [true], field must be present (default: [false])
   @param help Human-readable description
   @param name Field name
*)
val datetime: ?default:DateTime.t -> ?required:bool -> ?help:string -> string -> field_spec

(**
   Define a file system path field.

   Paths can be relative or absolute. Relative paths are resolved from the
   application's working directory.

   In TOML: [log_dir = "/var/log/myapp"] or [config_dir = "./config"]

   Example:
   ```ocaml
   path "data_dir" ~default:(Path.of_string "./data" |> Result.get_ok)
     ~help:"Data storage directory"
   ```

   @param default Default path if field is missing
   @param required If [true], field must be present (default: [false])
   @param help Human-readable description
   @param name Field name
*)
val path: ?default:Path.t -> ?required:bool -> ?help:string -> string -> field_spec

(**
   Define a UUID field.

   UUIDs are validated for correct format (RFC 4122).

   In TOML: [instance_id = "550e8400-e29b-41d4-a716-446655440000"]

   Example:
   ```ocaml
   uuid "request_id" ~required:true ~help:"Unique request identifier"
   ```

   @param default Default UUID if field is missing
   @param required If [true], field must be present (default: [false])
   @param help Human-readable description
   @param name Field name
*)
val uuid: ?default:Uuid.t -> ?required:bool -> ?help:string -> string -> field_spec

(**
   Define an array field where all items have the same structure.

   Lists can contain any type of value - primitives, objects, or even nested lists.
   In TOML, simple arrays use bracket syntax, and arrays of tables use [[array]] syntax.

   In TOML (simple values): [tags = ["tag1", "tag2", "tag3"]]

   In TOML (array of tables):
   ```toml
   [[handlers]]
   type = "console"
   id = "main"

   [[handlers]]
   type = "file"
   path = "error.log"
   ```

   Example (simple string array):
   ```ocaml
   list (string "" ~default:"") "tags" ~default:[]
   ```

   Example (array of objects):
   ```ocaml
   list (map [
     string "name" ~required:true;
     string "email" ~required:true;
     int "age" ~default:18;
   ]) "users" ~default:[]
   ```

   Example (combined with discriminated union - see below):
   ```ocaml
   list (discriminated_union ~discriminant:"type" ~cases:[...]) "handlers" ~default:[]
   ```

   @param item_spec The specification for each item in the array
   @param default Default value if field is missing (typically empty list)
   @param required If [true], field must be present (default: [false])
   @param help Human-readable description
   @param name Field name in the configuration
*)
val list:
  field_spec ->
  ?default:value list ->
  ?required:bool ->
  ?help:string ->
  string ->
  field_spec

(**
   Define a discriminated union where structure depends on a discriminant field.

   The discriminant field (e.g., "type") determines which case to validate against.
   Each case has its own field specifications. In TOML, use [[array]] syntax with
   a discriminant field.

   In TOML:
   ```toml
   [[log.handlers]]
   type = "console"
   id = "main"
   stdout_levels = ["info", "debug"]

   [[log.handlers]]
   type = "file"
   id = "errors"
   path = "error.log"
   levels = ["error"]
   ```

   Example:
   ```ocaml
   discriminated_union ~discriminant:"type" ~cases:[
     ("console", [
       string "id" ~required:true;
       list (string "" ~default:"info") "stdout_levels" ~default:[];
       string "format" ~default:"full";
     ]);

     ("file", [
       string "id" ~required:true;
       path "path" ~required:true;
       list (string "" ~default:"info") "levels" ~default:[];
       int64 "max_bytes" ~default:10485760L;
     ]);

     ("syslog", [
       string "id" ~required:true;
       string "host" ~required:true;
       int "port" ~default:514;
     ]);
   ]
   ```

   Typically used with {!list} for arrays of heterogeneous objects:
   ```ocaml
   list (discriminated_union ~discriminant:"type" ~cases:[...]) "handlers" ~default:[]
   ```

   @param discriminant The field name used to discriminate types (e.g., "type")
   @param cases List of (variant_value, field_specs) pairs
   @return A field spec for discriminated unions (typically wrapped with {!key})
*)
val discriminated_union: discriminant:string -> cases:(string * field_spec list) list -> field_spec

(**
   Restrict a field to a set of allowed values (enum combinator).

   This combinator works with ANY field type - string, int, uuid, etc.
   The value must be exactly one of the provided choices.

   In TOML: [log_level = "debug"] or [status_code = 200]

   Example (string enum):
   ```ocaml
   enum
     (string "log_level" ~default:"info" ~help:"Logging level")
     [String "debug"; String "info"; String "warn"; String "error"]
   ```

   Example (int enum):
   ```ocaml
   enum
     (int "status_code" ~default:200 ~help:"HTTP status code")
     [Int 200; Int 201; Int 400; Int 404; Int 500]
   ```

   Example (uuid enum):
   ```ocaml
   enum
     (uuid "instance_id" ~default:uuid1)
     [Uuid uuid1; Uuid uuid2; Uuid uuid3]
   ```

   @param field_spec The field specification to restrict
   @param choices List of allowed values (must match the field's type)
   @return A field spec with enum restrictions applied
*)
val enum: field_spec -> value list -> field_spec

(**
   Define a nested configuration object.

   Maps allow you to group related configuration fields. In TOML, they
   correspond to sections: [\[app.subsection\]]

   Example:
   ```ocaml
   key "database" (map [
     string "host" ~default:"localhost";
     int "port" ~default:5432;
     string "name" ~required:true;
   ])
   ```

   TOML:
   ```toml
   [myapp.database]
   host = "db.example.com"
   port = 5432
   name = "production"
   ```

   @param fields List of field specifications for the nested object
   @return A map field spec (usually wrapped with {!key})
*)
val map: field_spec list -> field_spec

(**
   Name a field (typically a map).

   This associates a name with a field spec, especially useful for nested maps.

   Example:
   ```ocaml
   key "server" (map [
     string "host" ~default:"localhost";
     int "port" ~default:4000;
   ])
   ```

   @param name Field name in the configuration
   @param spec The field specification to name
   @return A named field spec
*)
val key: string -> field_spec -> field_spec

(** {1 Introspection} *)

(**
   Get the application name from a spec.

   Example:
   ```ocaml
   let name = Config.Spec.app_name my_spec  (* "myapp" *)
   ```
*)
val app_name: t -> string

(**
   Get all registered specs.

   This is used internally by {!Config.child_spec} to load all configurations.
   You typically don't need to call this directly.

   @return List of all specs registered via {!for_app}
*)
val all_specs: unit -> t list

(**
   {1 Internal Types}

   These types are exposed for validation and testing but are not typically
   used directly in application code.
*)

(** Internal representation of field types with default values *)
type field_type =
  | String of {
      default: string option;
    }
  | Char of {
      default: char option;
    }
  | Int of {
      default: int option;
    }
  | Int32 of {
      default: int32 option;
    }
  | Int64 of {
      default: int64 option;
    }
  | Bool of {
      default: bool option;
    }
  | Float of {
      default: float option;
    }
  | Uri of {
      default: Net.Uri.t option;
    }
  | DateTime of {
      default: DateTime.t option;
    }
  | Path of {
      default: Path.t option;
    }
  | Uuid of {
      default: Uuid.t option;
    }
  | List of {
      item_spec: field;
      default: value list option;
    }
  | DiscriminatedUnion of {
      discriminant: string;
      cases: (string * field list) list;
    }
  | Map of field list

(** Internal representation of a field with all metadata *)
and field = {
  name: string;
  (** Field name *)
  field_type: field_type;
  (** Type and default value *)
  required: bool;
  (** Whether the field must be present *)
  help: string option;
  (** Optional help text for documentation *)
  allowed_values: value list option;
  (** Optional list of allowed values (enum restriction) *)
}

(**
   Get the list of fields from a spec.

   Used internally by the validator.
*)
val get_fields: t -> field list

(**
   Get the name of a field.

   Used internally by the validator.
*)
val field_name: field_spec -> string

(**
   Get the type of a field.

   Used internally by the validator.
*)
val field_type: field_spec -> field_type
