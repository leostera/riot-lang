open Std

(** {1 URI - Interned Identifiers}
    
    URIs are the primary identifiers in Poneglyph. They use string interning
    for fast comparison and minimal memory usage.
    
    URI Format: [namespace:type:identifier[:version]]
    
    Examples:
    - [tusk:file:src/main.ml:abc123] - File with content hash
    - [tusk:package:kernel] - Package entity  
    - [ocaml:module:Std.Path] - OCaml module
    - [@field:doc] - Shorthand for [poneglyph:field:doc]
*)

type t = int
(** A URI is represented as an integer ID (interned string) *)

type part = Ns of string | Kind of string | Id of string | Field of string
(** URI component types for construction *)

(** {2 Construction} *)

val of_string : string -> t
(** Create a URI from a string. Same string always returns same URI.
    
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
(** Convert URI back to string *)

(** {2 Comparison} *)

val equal : t -> t -> bool
(** Fast equality check (integer comparison) *)

val compare : t -> t -> int
(** Fast comparison for sorting *)

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
