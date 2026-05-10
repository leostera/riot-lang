open Std

type mode = Record_mode.t =
  | RecordOnce
  | ReplayOnly
  | RecordAll
  | NewEpisodes
type request_fingerprint = {
  method_: string;
  url: string;
  body_sha256: string option;
}
type recording_violation =
  | ExpectedObject
  | MissingField of string
  | InvalidField of string
  | InvalidMode of string
  | InvalidBodyEncoding of string
  | InvalidBase64Body
type error =
  | RecordingReadFailed of {
      path: Path.t;
      error: Fs.error;
    }
  | RecordingWriteFailed of {
      path: Path.t;
      error: Fs.error;
    }
  | RecordingDecodeFailed of {
      path: Path.t;
      error: Data.Json.error;
    }
  | RecordingInvalid of {
      path: Path.t;
      reason: recording_violation;
    }
  | ReplayOnlyMiss of {
      recording: string;
      request: request_fingerprint;
    }
  | RecordOnceMiss of {
      recording: string;
      request: request_fingerprint;
    }
  | UpstreamFailed of {
      recording: string;
      request: request_fingerprint;
      error: Client.error;
    }
type t

val make :
  ?mode:mode ->
  ?redact_headers:string list ->
  ?upstream_transport:Client.Config.transport ->
  library_dir:Path.t ->
  unit ->
  t

val library_dir : t -> Path.t

val mode : t -> mode

val use_recording : t -> name:string -> fn:(Client.t -> 'value) -> ('value, error) result

val error_to_json : error -> Data.Json.t

val error_to_string : error -> string
