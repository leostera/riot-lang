(** # Version - Semantic versioning
    
    Semantic versioning (SemVer 2.0) parsing, comparison, and requirement
    matching for dependency management and version constraints.
    
    ## Examples
    
    Parsing versions:
    
    ```ocaml
    open Std
    
    match Version.parse "1.2.3" with
    | Ok v ->
        Printf.printf "Version: %d.%d.%d\n" v.major v.minor v.patch
    | Error (Invalid_format msg) ->
        Log.error "Parse error: %s" msg
    
    (* Pre-release versions *)
    Version.parse "1.0.0-alpha.1"
    (* Ok {major=1; minor=0; patch=0; pre=[Alphanumeric "alpha"; Numeric 1]; build=None} *)
    
    (* Build metadata *)
    Version.parse "1.0.0+build.123"
    (* Ok {major=1; minor=0; patch=0; pre=[]; build=Some "build.123"} *)
    ```
    
    Comparing versions:
    
    ```ocaml
    let v1 = Version.parse "1.2.3" |> Result.unwrap in
    let v2 = Version.parse "1.3.0" |> Result.unwrap in
    
    Version.compare v1 v2  (* Lt *)
    Version.lt v1 v2  (* true *)
    Version.gte v2 v1  (* true *)
    
    (* Pre-release versions have lower precedence *)
    let stable = Version.parse "1.0.0" |> Result.unwrap in
    let pre = Version.parse "1.0.0-alpha" |> Result.unwrap in
    Version.lt pre stable  (* true *)
    ```
    
    Version requirements:
    
    ```ocaml
    (* Exact version *)
    let req = Version.parse_requirement "== 1.2.3" |> Result.unwrap in
    let v = Version.parse "1.2.3" |> Result.unwrap in
    Version.matches req v  (* true *)
    
    (* Range requirements *)
    let req = Version.parse_requirement ">= 1.2.0" |> Result.unwrap in
    Version.matches req (Version.parse "1.2.3" |> Result.unwrap)  (* true *)
    Version.matches req (Version.parse "1.1.0" |> Result.unwrap)  (* false *)
    
    (* Tilde operator - allows patch-level changes *)
    let req = Version.parse_requirement "~> 1.2.3" |> Result.unwrap in
    Version.matches req (Version.parse "1.2.4" |> Result.unwrap)  (* true *)
    Version.matches req (Version.parse "1.3.0" |> Result.unwrap)  (* false *)
    ```
    
    ## SemVer Rules
    
    Given a version MAJOR.MINOR.PATCH:
    
    - **MAJOR**: Incompatible API changes
    - **MINOR**: Backwards-compatible functionality
    - **PATCH**: Backwards-compatible bug fixes
    
    Pre-release versions (e.g. 1.0.0-alpha) have lower precedence than
    the corresponding release version.
    
    ## Use Cases
    
    - Package dependency resolution
    - Version constraint checking
    - Release management
    - Compatibility verification
    
    ## See Also
    
    - SemVer 2.0 specification: https://semver.org/
*)

(** {1 Types} *)

open Global

type pre_release_segment =
  | Numeric of int
  | Alphanumeric of string
(** Pre-release version segment - either numeric or alphanumeric *)
type t = {
  major: int;
  minor: int;
  patch: int;
  pre: pre_release_segment list;
  build: string option;
}
(** Semantic version with major.minor.patch, optional pre-release identifiers,
    and optional build metadata *)
type comparison =
  Lt
  | Eq
  | Gt
(** Requirement operator for version matching *)
type requirement_op =
  | ReqEq
  (** Equal to: == *)
  | ReqNeq
  (** Not equal to: != *)
  | ReqGt
  (** Greater than: > *)
  | ReqGte
  (** Greater than or equal: >= *)
  | ReqLt
  (** Less than: < *)
  | ReqLte
  (** Less than or equal: <= *)
  | ReqTilde
(** Tilde operator: ~> (allows patch-level changes) *)
type requirement
(** Version requirement specification (opaque) *)
type parse_error =
  | Invalid_format of string
  | Invalid_version_segment of string
  | Invalid_pre_release_segment of string
(** Version parsing errors *)
(** {1 Parsing} *)

val parse: string -> (t, parse_error) result
(** Parse a version string following SemVer format.

    Examples:
    - "1.2.3" -> Ok {major=1; minor=2; patch=3; pre=[]; build=None}
    - "1.0.0-alpha.1" -> Ok {major=1; minor=0; patch=0; pre=[Alphanumeric "alpha"; Numeric 1]; build=None}
    - "1.0.0+build.123" -> Ok {major=1; minor=0; patch=0; pre=[]; build=Some "build.123"}
    *)
val to_string: t -> string
(** Convert a version to its string representation *)
(** {1 Comparison} *)

val compare: t -> t -> comparison
(** Compare two versions according to SemVer precedence rules.

    - Versions are compared by major, then minor, then patch
    - Pre-release versions have lower precedence than normal versions
    - Pre-release identifiers are compared lexicographically
    - Build metadata is ignored in comparisons *)
val equal: t -> t -> bool
(** Check if two versions are equal *)
val lt: t -> t -> bool
(** Less than *)
val lte: t -> t -> bool
(** Less than or equal *)
val gt: t -> t -> bool
(** Greater than *)
val gte: t -> t -> bool
(** Greater than or equal *)
(** {1 Requirements} *)

val parse_requirement: string -> (requirement, parse_error) result
(** Parse a version requirement string.

    Supported operators:
    - "== 1.2.3" - exact match
    - "!= 1.2.3" - not equal
    - "> 1.2.3" - greater than
    - ">= 1.2.3" - greater than or equal
    - "< 1.2.3" - less than
    - "<= 1.2.3" - less than or equal
    - "~> 1.2.3" - allows patch-level changes (>= 1.2.3 and < 1.3.0)
    - "~> 1.2" - allows minor-level changes (>= 1.2.0 and < 2.0.0) *)
val matches: requirement -> t -> bool
(** Check if a version satisfies a requirement *)
(** {1 Constructors} *)

val make:
  major:int -> minor:int -> patch:int -> ?pre:pre_release_segment list -> ?build:string -> unit -> t
(** Create a version with no pre-release or build metadata *)
