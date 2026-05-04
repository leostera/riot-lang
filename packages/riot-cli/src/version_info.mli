open Std

(**
   Installed Riot release metadata.

   Use this module when the CLI needs to report its own release identity,
   compare installed versions, or stamp outbound requests with version-aware
   agent strings.
*)
type t = {
  (** Stable release identifier, such as a version tag. *)
  release_id: string;
  (** Source revision used for the build. *)
  build_sha: string;
  (** Optional URL for release notes. *)
  notes_url: string option;
  (** Optional URL comparing this release to another baseline. *)
  compare_url: string option;
  (** Optional issue tracker URL for this release line. *)
  issues_url: string option;
}

(** Resolve the on-disk path used to store installed version metadata. *)
val metadata_path: unit -> (Path.t, string) result

(** Load version metadata from a JSON file. *)
val from_path: Path.t -> (t, string) result

(** Parse version metadata from a JSON string. *)
val from_json_string: string -> (t, string) result

(**
   Parse a plain version string into release metadata when possible.

   Use this for lightweight version inputs that do not come from a full JSON
   metadata file.
*)
val from_version_string: string -> t option

(** Write version metadata to [path]. *)
val write_path: path:Path.t -> t -> (unit, string) result

(** Return [true] when two version records refer to the same release identity. *)
val same_identity: t -> t -> bool

(** Read the currently installed Riot version metadata, if present. *)
val read_installed: unit -> t option

(** Persist installed Riot version metadata. *)
val write_installed: t -> (unit, string) result

(** Render a compact version string for a specific release record. *)
val version_string_of: t -> string

(** Render the version string for the currently running CLI. *)
val version_string: unit -> string

(**
   Render a human-facing release label.

   Example return values include strings like ["0.1.0"] or a dev label with a
   short build identifier when installed metadata is incomplete.
*)
val release_label: t -> string

(** Render the default [X-Riot-Agent] string for outbound requests. *)
val agent_string: unit -> string
