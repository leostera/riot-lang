open Std

(** Errors produced while decoding values through a [Serde] backend. *)
type error =
  [ `invalid_field_type
  | `missing_field
  | `no_more_data
  | `unimplemented
  | `invalid_tag
  | `Msg of string
  | `Io_error of IO.error ]

(** Internal exception used by format backends to abort decoding fast. *)
exception Decode_error of error

(** Precompiled field matchers used by record decoders. *)
module Fields: sig
  (** A field case maps an input key to a decoder tag. *)
  type 'tag case

  (** A compiled field matcher. *)
  type 'tag t

  (** Declare a single field case. *)
  val case : string -> 'tag -> 'tag case

  (** Extract the tag stored in a field case. *)
  val tag : 'tag case -> 'tag

  (** Match a borrowed slice against a compiled field set. *)
  val match_slice : 'tag t -> string -> offset:int -> length:int -> 'tag option

  (** Match buffered key contents against a compiled field set. *)
  val match_buffer : 'tag t -> IO.Buffer.t -> 'tag option

  (** Compile a list of fields into a matcher. *)
  val make : 'tag case list -> 'tag t
end

(** A format-agnostic decoder description. *)
type 'value t = { run: 'state. 'state backend -> 'state -> 'value }

(** A variant constructor description. *)
and 'value variant_case =
  | Unit : string * 'value -> 'value variant_case
  | Newtype : string * 'payload t * ('payload -> 'value) -> 'value variant_case

(** A list of variant constructor descriptions. *)
and 'value variant_cases = 'value variant_case list

(** The operations a concrete format backend must implement. *)
and 'state backend = {
  bool: 'state -> bool;
  string: 'state -> string;
  int: 'state -> int;
  int32: 'state -> int32;
  int64: 'state -> int64;
  float: 'state -> float;
  skip_any: 'state -> unit;
  option:
    'value.
    'state ->
    'value t ->
    'value option;
  list:
    'value.
    'state ->
    'value t ->
    'value list;
  record:
    'field 'acc 'value.
    'state ->
    fields:'field Fields.t ->
    init:'acc ->
    step:('acc -> 'field option -> 'acc) ->
    finish:('acc -> 'value) ->
    'value;
  variant:
    'value.
    'state ->
    'value variant_cases ->
    'value;
}

(** Reader passed into record steps so fields can decode nested values. *)
type reader = {
  read: 'value. 'value t -> 'value;
}

(** Variant constructor helpers. *)
module Variant: sig
  (** A single variant constructor case. *)
  type 'value case = 'value variant_case =
    | Unit : string * 'value -> 'value case
    | Newtype : string * 'payload t * ('payload -> 'value) -> 'value case

  (** A list of variant constructor cases. *)
  type 'value cases = 'value case list

  (** Match a unit constructor tag. *)
  val unit : string -> 'value -> 'value case

  (** Match a newtype constructor tag. *)
  val newtype : string -> 'payload t -> ('payload -> 'value) -> 'value case
end

(** Build a decoder that always returns a fixed value. *)
val return : 'value -> 'value t

(** Map over the result of a decoder. *)
val map : 'value t -> ('value -> 'next) -> 'next t

(** Sequence decoders monadically. *)
val bind : 'value t -> ('value -> 'next t) -> 'next t

(** Build a decoder that always fails. *)
val fail : error -> 'value t

(** Raise a decode error from inside decoder construction helpers. *)
val raise_error : error -> 'value

(** Raise the standard missing-field error. *)
val missing_field : unit -> 'value

(** Decode a nested value from a record reader. *)
val read : reader -> 'value t -> 'value

(** Run a decoder against a concrete backend and state. *)
val run : 'value t -> 'state backend -> 'state -> ('value, error) result

(** Monadic syntax helpers for decoder construction. *)
module Syntax: sig
  val ( let* ) : 'value t -> ('value -> 'next t) -> 'next t
  val ( let+ ) : 'value t -> ('value -> 'next) -> 'next t
end

(** Declare a field case for use with [fields]. *)
val field : string -> 'tag -> 'tag Fields.case

(** Compile a list of field cases into a matcher. *)
val fields : 'tag Fields.case list -> 'tag Fields.t

(** Decode a boolean value. *)
val bool : bool t

(** Decode a string value. *)
val string : string t

(** Decode an integer value. *)
val int : int t

(** Decode an [int32] value. *)
val int32 : int32 t

(** Decode an [int64] value. *)
val int64 : int64 t

(** Decode a floating-point value. *)
val float : float t

(** Skip the current value. *)
val skip_any : unit t

(** Decode an optional value. *)
val option : 'value t -> 'value option t

(** Decode a list of values. *)
val list : 'value t -> 'value list t

(** Decode an array of values. *)
val array : 'value t -> 'value array t

(** Decode a record-shaped value. *)
val record :
  fields:'field Fields.t ->
  init:'acc ->
  step:(reader -> 'acc -> 'field option -> 'acc) ->
  finish:('acc -> 'value) ->
  'value t

(** Decode a tagged variant value. *)
val variant : 'value Variant.cases -> 'value t

(** Error helpers for user-facing reporting. *)
module Error: sig
  (** The serde error type. *)
  type t = error

  (** Render a decode error as user-facing text. *)
  val to_string : t -> string
end
