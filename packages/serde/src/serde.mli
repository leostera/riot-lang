open Std

(** Errors produced while encoding or decoding values through a [Serde] backend. *)
type error = [
  | `invalid_field_type
  | `missing_field
  | `no_more_data
  | `unimplemented
  | `invalid_tag
  | `Msg of string
  | `Io_error of IO.error
]

(** Internal exception used by format backends to abort decoding fast. *)
exception Decode_error of error

(** Internal exception used by format backends to abort encoding fast. *)
exception Encode_error of error

(** Fast, format-agnostic deserializer descriptions. *)
module De: sig
  (** Precompiled field matchers used by record decoders. *)
  module Fields: sig
    (** A field case maps an input key to a decoder tag. *)
    type 'tag case
    (** A compiled field matcher. *)
    type 'tag t

    (** Declare a single field case. *)
    val case: string -> 'tag -> 'tag case

    (** Extract the tag stored in a field case. *)
    val tag: 'tag case -> 'tag

    (** Match a borrowed slice against a compiled field set. *)
    val match_slice: 'tag t -> string -> offset:int -> length:int -> 'tag option

    (** Match a borrowed I/O slice against a compiled field set. *)
    val match_ioslice: 'tag t -> IO.IoSlice.t -> offset:int -> length:int -> 'tag option

    (** Match a borrowed byte slice against a compiled field set. *)
    val match_bytes: 'tag t -> bytes -> offset:int -> length:int -> 'tag option

    (** Match buffered key contents against a compiled field set. *)
    val match_buffer: 'tag t -> IO.Buffer.t -> 'tag option

    (** Match a buffered key range against a compiled field set. *)
    val match_buffer_range: 'tag t -> IO.Buffer.t -> offset:int -> length:int -> 'tag option

    (** Compile a list of fields into a matcher. *)
    val make: 'tag case list -> 'tag t

    (** Return the number of declared fields in source order. *)
    val length: 'tag t -> int

    (** Return the tag for the field at the given source-order index. *)
    val tag_at: 'tag t -> int -> 'tag option

    (** Return the tag for the field at the given source-order index without bounds checks. *)
    val tag_at_unchecked: 'tag t -> int -> 'tag
  end

  (** A format-agnostic decoder description. *)
  type 'value t = {
    run: 'state. 'state backend -> 'state -> 'value;
  }

  (** A variant constructor description. *)
  and 'value variant_case =
    | Unit: string * 'value -> 'value variant_case
    | Newtype: string * 'payload t * ('payload -> 'value) -> 'value variant_case

  (** A list of variant constructor descriptions. *)
  and 'value variant_cases = 'value variant_case list

  (** A compiled array of variant constructor descriptions used by format backends. *)
  and 'value compiled_variant_cases = 'value variant_case array

  (** The operations a concrete format backend must implement. *)
  and 'state backend = {
    bool: 'state -> bool;
    string: 'state -> string;
    int: 'state -> int;
    int32: 'state -> int32;
    int64: 'state -> int64;
    float: 'state -> float;
    skip_any: 'state -> unit;
    option: 'value. 'state -> 'value t -> 'value option;
    list: 'value. 'state -> 'value t -> 'value vec;
    array: 'value. 'state -> 'value t -> 'value array;
    map: 'value. 'state -> 'value t -> (string * 'value) vec;
    record:
      'field 'acc 'value. 'state ->
      fields:'field Fields.t ->
      init:'acc ->
      step:('acc -> 'field option -> 'acc) ->
      finish:('acc -> 'value) ->
      'value;
    record_mut:
      'field 'builder 'value. 'state ->
      fields:'field Fields.t ->
      create:(unit -> 'builder) ->
      step:('builder -> 'field option -> unit) ->
      finish:('builder -> 'value) ->
      'value;
    variant: 'value. 'state -> 'value compiled_variant_cases -> 'value;
  }
  type reader = {
    read: 'value. 'value t -> 'value;
  }

  (** Variant constructor helpers. *)
  module Variant: sig
    (** A single variant constructor case. *)
    type 'value case = 'value variant_case =
      | Unit: string * 'value -> 'value case
      | Newtype: string * 'payload t * ('payload -> 'value) -> 'value case
    type 'value cases = 'value case list

    (** Match a unit constructor tag. *)
    val unit: string -> 'value -> 'value case

    (** Match a newtype constructor tag. *)
    val newtype: string -> 'payload t -> ('payload -> 'value) -> 'value case
  end

  (** Build a decoder that always returns a fixed value. *)
  val const: 'value -> 'value t

  (** Map over the result of a decoder. *)
  val map: 'value t -> ('value -> 'next) -> 'next t

  (** Sequence decoders monadically. *)
  val and_then: 'value t -> ('value -> 'next t) -> 'next t

  (** Build a decoder that always fails. *)
  val fail: error -> 'value t

  (** Raise a decode error from inside decoder construction helpers. *)
  val raise_error: error -> 'value

  (** Raise the standard missing-field error. *)
  val missing_field: unit -> 'value

  (** Decode a nested value from a record reader. *)
  val read: reader -> 'value t -> 'value

  (** Run a decoder against a concrete backend and state. *)
  val run: 'value t -> 'state backend -> 'state -> ('value, error) result

  (** Monadic syntax helpers for decoder construction. *)
  module Syntax: sig
    val ( let* ): 'value t -> ('value -> 'next t) -> 'next t

    val ( let+ ): 'value t -> ('value -> 'next) -> 'next t
  end

  (** Declare a field case for use with [fields]. *)
  val field: string -> 'tag -> 'tag Fields.case

  (** Compile a list of field cases into a matcher. *)
  val fields: 'tag Fields.case list -> 'tag Fields.t

  (** Decode a boolean value. *)
  val bool: bool t

  (** Decode a string value. *)
  val string: string t

  (** Decode an integer value. *)
  val int: int t

  (** Decode an [int32] value. *)
  val int32: int32 t

  (** Decode an [int64] value. *)
  val int64: int64 t

  (** Decode a floating-point value. *)
  val float: float t

  (** Skip the current value. *)
  val skip_any: unit t

  (** Decode an optional value. *)
  val option: 'value t -> 'value option t

  (** Decode a sequence of values into a vector. *)
  val list: 'value t -> 'value vec t

  (** Decode a sequence of values into an array. *)
  val array: 'value t -> 'value array t

  (** Decode a string-keyed map into key/value entries. *)
  val map: 'value t -> (string * 'value) vec t

  (** Decode a record-shaped value. *)
  val record:
    fields:'field Fields.t ->
    init:'acc ->
    step:(reader -> 'acc -> 'field option -> 'acc) ->
    finish:('acc -> 'value) ->
    'value t

  (** Decode a record-shaped value through a mutable builder. *)
  val record_mut:
    fields:'field Fields.t ->
    create:(unit -> 'builder) ->
    step:(reader -> 'builder -> 'field option -> unit) ->
    finish:('builder -> 'value) ->
    'value t

  (** Decode a tagged variant value. *)
  val variant: 'value Variant.cases -> 'value t
end

(** Error helpers for user-facing reporting. *)
module Error: sig
  (** The serde error type. *)
  type t = error

  (** Render a serde error as user-facing text. *)
  val to_string: t -> string
end

(** Fast, format-agnostic serializer descriptions. *)
module Ser: sig
  (** A format-agnostic serializer description. *)
  type 'value t = {
    run: 'state. 'state backend -> 'state -> 'value -> unit;
  }

  (** A single record field encoder. *)
  and 'value field =
    | Field: string * 'field t * ('value -> 'field) -> 'value field

  (** A compiled list of record field encoders. *)
  and 'value fields = 'value field array

  (** A single tagged variant encoder case. *)
  and 'value variant_case =
    | Unit: string * ('value -> bool) -> 'value variant_case
    | Newtype: string * 'payload t * ('value -> 'payload option) -> 'value variant_case

  (** A compiled list of tagged variant encoder cases. *)
  and 'value variant_cases = 'value variant_case array

  (** The operations a concrete format backend must implement. *)
  and 'state backend = {
    bool: 'state -> bool -> unit;
    string: 'state -> string -> unit;
    int: 'state -> int -> unit;
    int32: 'state -> int32 -> unit;
    int64: 'state -> int64 -> unit;
    float: 'state -> float -> unit;
    null: 'state -> unit;
    option: 'value. 'state -> 'value t -> 'value option -> unit;
    list: 'value. 'state -> 'value t -> 'value vec -> unit;
    array: 'value. 'state -> 'value t -> 'value array -> unit;
    map: 'value. 'state -> 'value t -> (string * 'value) vec -> unit;
    record: 'value. 'state -> 'value fields -> 'value -> unit;
    variant: 'value. 'state -> 'value variant_cases -> 'value -> unit;
  }

  module Field: sig
    (** Encode a named field by projecting its value out of the parent record. *)
    val make: string -> 'field t -> ('value -> 'field) -> 'value field
  end

  (** Tagged variant encoder helpers. *)
  module Variant: sig
    (** A single tagged variant encoder case. *)
    type 'value case = 'value variant_case =
      | Unit: string * ('value -> bool) -> 'value case
      | Newtype: string * 'payload t * ('value -> 'payload option) -> 'value case

    val unit: string -> ('value -> bool) -> 'value case

    (** Match a newtype constructor and project its payload. *)
    val newtype: string -> 'payload t -> ('value -> 'payload option) -> 'value case
  end

  (** Run a serializer against a concrete backend and state. *)
  val run: 'value t -> 'state backend -> 'state -> 'value -> (unit, error) result

  (** Contramap a serializer over an input projection. *)
  val contramap: ('value -> 'next) -> 'next t -> 'value t

  (** Build a serializer that always fails. *)
  val fail: error -> 'value t

  (** Serialize a boolean value. *)
  val bool: bool t

  (** Serialize a string value. *)
  val string: string t

  (** Serialize an integer value. *)
  val int: int t

  (** Serialize an [int32] value. *)
  val int32: int32 t

  (** Serialize an [int64] value. *)
  val int64: int64 t

  (** Serialize a floating-point value. *)
  val float: float t

  (** Serialize [()] as a backend null value. *)
  val null: unit t

  (** Serialize an optional value. *)
  val option: 'value t -> 'value option t

  (** Serialize a vector of values. *)
  val list: 'value t -> 'value vec t

  (** Serialize an array of values. *)
  val array: 'value t -> 'value array t

  (** Serialize key/value entries as a string-keyed map. *)
  val map: 'value t -> (string * 'value) vec t

  (** Declare a single record field encoder. *)
  val field: string -> 'field t -> ('value -> 'field) -> 'value field

  (** Compile a list of record field encoders. *)
  val fields: 'value field list -> 'value fields

  (** Serialize a record-shaped value. *)
  val record: 'value fields -> 'value t

  (** Serialize a tagged variant value. *)
  val variant: 'value Variant.case list -> 'value t
end
