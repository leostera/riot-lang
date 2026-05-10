open Std

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
type t =
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

val to_json : t -> Data.Json.t

val to_string : t -> string
