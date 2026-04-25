(** JSON-RPC 2.0 Protocol Implementation *)
open Std
open Std.Data

(* Re-export all types and functions from Common *)
include Common

(* ApplicationProtocol module type needs to be defined here *)
module type ApplicationProtocol = sig
  type request
  type response
  val response_to_json: response -> Json.t

  val response_of_json: Json.t -> (response, Json.t) result

  val request_to_params: request -> prerequest

  val request_of_params: string -> params -> (request, Json.t) result
end

(* Client module *)
module Client = Client

(* Server module *)
module Server = Server

(* Helper functions - use the ones from Common instead *)
