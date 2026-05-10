open Std

type mode = Record_mode.t =
  | RecordOnce
  | ReplayOnly
  | RecordAll
  | NewEpisodes
type request_fingerprint = Recorder_error.request_fingerprint = {
  method_: string;
  url: string;
  body_sha256: string option;
}
type body = { sha256: string; bytes: string }
type stored_request = {
  method_: string;
  url: string;
  headers: (string * string) list;
  body: body option;
}
type stored_response = {
  status: int;
  headers: (string * string) list;
  body: body;
}
type interaction = {
  request: stored_request;
  response: stored_response;
}
type t

val make : name:string -> record_mode:mode -> unit -> t

val name : t -> string

val path_stem : t -> string

val sanitize_name : string -> string

val record_mode : t -> mode

val with_record_mode : t -> record_mode:mode -> t

val interactions : t -> interaction Collections.Vector.t

val request_fingerprint : Client.Request.t -> request_fingerprint

val from_blink_request :
  redact_headers:((string * string) list -> (string * string) list) ->
  Client.Request.t ->
  stored_request

val from_blink_response :
  redact_headers:((string * string) list -> (string * string) list) ->
  Client.Response.t ->
  stored_response

val response_to_blink : stored_response -> Client.Response.t

val find_interaction : t -> Client.Request.t -> interaction option

val to_json : t -> Data.Json.t

val from_json :
  fallback_name:string ->
  fallback_mode:mode ->
  Data.Json.t ->
  (t, Recorder_error.recording_violation) result

val push : t -> value:interaction -> unit
