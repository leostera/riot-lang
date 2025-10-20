open Std

module ProtofileFormat : sig
  type t

  val parse : string -> (t, string) Result.t
  val print : t -> string
  val to_json : t -> Data.Json.t
end

module DebugFormat : sig
  type t

  val parse : string -> (t, string) Result.t
  val print : t -> string
end

module WireFormat : sig
  type t

  val decode : bytes -> (t, string) Result.t
  val encode : t -> bytes
end
