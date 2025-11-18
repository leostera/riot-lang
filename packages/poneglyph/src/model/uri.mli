open Std

(** {1 URI - Content-Addressed Identifiers}
    
    URIs are the primary identifiers in Poneglyph. They use SHA-256 hashing
    for deterministic, content-addressed identification.
    
    URI Format: [namespace:type:identifier[:version]]
    
    Examples:
    - [tusk:file:src/main.ml:abc123] - File with content hash
    - [tusk:package:kernel] - Package entity  
    - [ocaml:module:Std.Path] - OCaml module
    - [@field:doc] - Shorthand for [poneglyph:field:doc]
*)

type t = {
  uri : string;      (** Original normalized URI string *)
  sha256 : bytes;    (** Full SHA-256 hash (32 bytes) for storage/comparison *)
}
(** A URI contains both the string representation and its SHA-256 hash *)

type part = Ns of string | Kind of string | Id of string | Field of string
(** URI component types for construction *)

(** {2 Construction} *)

val of_string : string -> t
(** Create a URI from a string. Same string always produces same SHA-256 hash.
    
    {[
      let uri = Uri.of_string "tusk:file:main.ml"
    ]}
*)

val make : part list -> t
(** Construct a URI from parts.
    
    {[
      let uri = Uri.make Uri.[
        ns "tusk";
        kind "file";
        id "src/main.ml"
      ]
      (* Results in: "tusk:file:src/main.ml" *)
    ]}
*)

(** {2 Conversion} *)

val to_string : t -> string
(** Convert URI to its string representation *)

(** {2 Comparison} *)

val equal : t -> t -> bool
(** Fast equality check (compares SHA-256 hashes) *)

val compare : t -> t -> int
(** Fast comparison for sorting (compares SHA-256 hashes) *)

(** {2 Shorthand Support} *)

val expand_shorthand : string -> string
(** Expand [@] prefix to [poneglyph:].
    
    {[
      expand_shorthand "@field:doc" 
      (* Returns: "poneglyph:field:doc" *)
    ]}
*)

(** {2 Part Builders} *)

val ns : string -> part
(** Namespace part *)

val kind : string -> part
(** Kind/type part *)

val id : string -> part
(** ID part. In practice this uses Printf.ksprintf internally but is exposed
    as a simple string for the signature.
    
    {[
      Uri.make Uri.[ns "app"; kind "user"; id "alice-42"]
    ]}
*)

val field : string -> part
(** Field part *)
