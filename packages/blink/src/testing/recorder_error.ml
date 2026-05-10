open Std

module Json = Data.Json

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
      error: Json.error;
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

let fingerprint_to_json = fun fingerprint ->
  Json.obj
    [
      ("method", Json.string fingerprint.method_);
      ("url", Json.string fingerprint.url);
      ("body_sha256", match fingerprint.body_sha256 with
      | Some value -> Json.string value
      | None -> Json.null);
    ]

let recording_violation_to_json = fun reason ->
  match reason with
  | ExpectedObject -> Json.obj [ ("type", Json.string "ExpectedObject") ]
  | MissingField field ->
      Json.obj [ ("type", Json.string "MissingField"); ("field", Json.string field) ]
  | InvalidField field ->
      Json.obj [ ("type", Json.string "InvalidField"); ("field", Json.string field) ]
  | InvalidMode mode -> Json.obj [ ("type", Json.string "InvalidMode"); ("mode", Json.string mode) ]
  | InvalidBodyEncoding encoding ->
      Json.obj [ ("type", Json.string "InvalidBodyEncoding"); ("encoding", Json.string encoding); ]
  | InvalidBase64Body -> Json.obj [ ("type", Json.string "InvalidBase64Body") ]

let io_error_to_json = fun error ->
  Json.obj
    [ ("type", Json.string "IoError"); ("message", Json.string (IO.error_message error)) ]

let json_error_to_json = fun error ->
  Json.obj
    [ ("type", Json.string "JsonError"); ("message", Json.string (Json.error_to_string error)); ]

let to_json = fun error ->
  match error with
  | RecordingReadFailed { path; error } ->
      Json.obj
        [
          ("type", Json.string "RecordingReadFailed");
          ("path", Json.string (Path.to_string path));
          ("error", io_error_to_json error);
        ]
  | RecordingWriteFailed { path; error } ->
      Json.obj
        [
          ("type", Json.string "RecordingWriteFailed");
          ("path", Json.string (Path.to_string path));
          ("error", io_error_to_json error);
        ]
  | RecordingDecodeFailed { path; error } ->
      Json.obj
        [
          ("type", Json.string "RecordingDecodeFailed");
          ("path", Json.string (Path.to_string path));
          ("error", json_error_to_json error);
        ]
  | RecordingInvalid { path; reason } ->
      Json.obj
        [
          ("type", Json.string "RecordingInvalid");
          ("path", Json.string (Path.to_string path));
          ("reason", recording_violation_to_json reason);
        ]
  | ReplayOnlyMiss { recording; request } ->
      Json.obj
        [
          ("type", Json.string "ReplayOnlyMiss");
          ("recording", Json.string recording);
          ("request", fingerprint_to_json request);
        ]
  | RecordOnceMiss { recording; request } ->
      Json.obj
        [
          ("type", Json.string "RecordOnceMiss");
          ("recording", Json.string recording);
          ("request", fingerprint_to_json request);
        ]
  | UpstreamFailed { recording; request; error } ->
      Json.obj
        [
          ("type", Json.string "UpstreamFailed");
          ("recording", Json.string recording);
          ("request", fingerprint_to_json request);
          ("error", Json.string (Client.error_to_string error));
        ]

let to_string = fun error -> Json.to_string (to_json error)
