open Std

(** {1 Fact - Entity-Attribute-Value Triples}
    
    Facts are the fundamental unit of data in Poneglyph. Each fact is a triple
    of (entity, attribute, value) plus metadata (transaction ID, timestamp, retraction status).
*)

(** {2 Value Types} *)

type value =
  | String of string  (** UTF-8 string *)
  | Int of int  (** Integer value *)
  | Bool of bool  (** Boolean value *)
  | Float of float  (** Floating point value *)
  | Uri of Uri.t  (** Reference to another entity *)
  | DateTime of Datetime.t  (** Timestamp *)

(** {2 Fact Structure} *)

type t = {
  fact_uri : Uri.t;  (** Unique ID for this fact *)
  source_uri : Uri.t;  (** Provenance - where did this fact come from? *)
  entity : Uri.t;  (** Subject - what entity is this about? *)
  attribute : Uri.t;  (** Predicate - what property? *)
  value : value;  (** Object - what is the value? *)
  stated_at : Datetime.t;  (** When was this fact stated? *)
  tx_id : int;  (** Which transaction? *)
  retracted : bool;  (** Has this fact been retracted? *)
}

(** {2 Construction} *)

val make :
  source:Uri.t ->
  entity:Uri.t ->
  attribute:Uri.t ->
  value:value ->
  stated_at:Datetime.t ->
  tx_id:int ->
  t
(** Create a new fact with provenance tracking. The fact_uri is auto-generated.
    
    The [source] parameter tracks where this fact came from:
    - ["tusk:build:12345"] - from a build
    - ["compiler:cmt:path.cmt"] - from compiler metadata
    - ["llm:session:abc"] - from LLM annotation
    - ["git:commit:def"] - from git history
    - ["manual"] - manually created
*)

val for_entity : Uri.t -> (Uri.t -> t) list -> t list
(** Helper for building multiple facts about one entity.
    
    {[
      let facts = Fact.for_entity file_uri [
        make_hash ~hash:"abc123";
        make_size ~bytes:4096;
      ]
    ]}
    
    This is equivalent to:
    {[
      [make_hash file_uri ~hash:"abc123"; 
       make_size file_uri ~bytes:4096]
    ]}
*)

(** {2 Utilities} *)

val value_to_string : value -> string
(** Convert a value to a human-readable string *)

val value_equal : value -> value -> bool
(** Check if two values are equal *)

val value_hash : value -> int
(** Hash a value for use in hash tables *)
