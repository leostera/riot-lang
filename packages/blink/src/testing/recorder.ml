open Std

module BlinkError = Error
module Error = Recorder_error
module H = Client
module Json = Data.Json

type mode = Record_mode.t =
  | RecordOnce
  | ReplayOnly
  | RecordAll
  | NewEpisodes

type request_fingerprint = Error.request_fingerprint = {
  method_: string;
  url: string;
  body_sha256: string option;
}

type recording_violation = Error.recording_violation =
  | ExpectedObject
  | MissingField of string
  | InvalidField of string
  | InvalidMode of string
  | InvalidBodyEncoding of string
  | InvalidBase64Body

type error = Error.t =
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
      error: H.error;
    }

type t = {
  library_dir: Path.t;
  mode: mode;
  redact_headers: string list;
  upstream_transport: H.Config.transport option;
}

type session = {
  recorder: t;
  recording: Recording.t;
  path: Path.t;
  existed: bool;
  upstream: H.t;
}

let make = fun
  ?(mode = RecordOnce)
  ?(redact_headers = ["authorization"; "cookie"; "proxy-authorization"; "set-cookie"; "x-api-key";])
  ?upstream_transport
  ~library_dir
  () ->
  {
    library_dir;
    mode;
    redact_headers;
    upstream_transport;
  }

let mode = fun recorder -> recorder.mode

let library_dir = fun recorder -> recorder.library_dir

let error_to_json = Error.to_json

let error_to_string = Error.to_string

let normalize_header_name = fun name -> String.lowercase_ascii (String.trim name)

let header_is_redacted = fun recorder name ->
  let normalized = normalize_header_name name in
  List.exists
    (fun redacted -> String.equal (normalize_header_name redacted) normalized)
    recorder.redact_headers

let redact_headers = fun recorder headers ->
  List.map
    headers
    ~fn:(fun (name, value) ->
      if header_is_redacted recorder name then
        (name, "<REDACTED>")
      else
        (name, value))

let recording_path = fun recorder name ->
  Path.(recorder.library_dir / Path.v (Recording.sanitize_name name ^ ".json"))

let empty_recording = fun name mode -> Recording.make ~name ~record_mode:mode ()

let decode_recording = fun path name mode contents ->
  Json.from_string contents
  |> Result.map_err ~fn:(fun error -> RecordingDecodeFailed { path; error })
  |> Result.and_then
    ~fn:(fun json ->
      Recording.from_json ~fallback_name:name ~fallback_mode:mode json
      |> Result.map_err ~fn:(fun reason -> RecordingInvalid { path; reason })
      |> Result.map
        ~fn:(fun recording -> (path, true, Recording.with_record_mode recording ~record_mode:mode)))

let read_existing_recording = fun path name mode ->
  Fs.read path
  |> Result.map_err ~fn:(fun error -> RecordingReadFailed { path; error })
  |> Result.and_then ~fn:(decode_recording path name mode)

let load_recording = fun recorder name mode ->
  let path = recording_path recorder name in
  match Fs.exists path with
  | Error error -> Error (RecordingReadFailed { path; error })
  | Ok false -> Ok (path, false, empty_recording name mode)
  | Ok true when mode = RecordAll -> Ok (path, true, empty_recording name mode)
  | Ok true -> read_existing_recording path name mode

let ensure_parent_dir = fun path ->
  match Path.parent path with
  | None -> Ok ()
  | Some parent -> Fs.create_dir_all parent

let save_recording = fun session ->
  match ensure_parent_dir session.path with
  | Error error -> Error (RecordingWriteFailed { path = session.path; error })
  | Ok () -> (
      let payload =
        session.recording
        |> Recording.to_json
        |> Json.to_string_pretty
      in
      match Fs.write payload session.path with
      | Ok () -> Ok ()
      | Error error -> Error (RecordingWriteFailed { path = session.path; error })
    )

let make_upstream = fun recorder ->
  let config =
    match recorder.upstream_transport with
    | Some transport -> H.Config.make ~transport ()
    | None -> H.Config.make ()
  in
  H.make ~config ()

let recording_mode = fun session -> Recording.record_mode session.recording

let can_record = fun session mode ->
  match mode with
  | RecordAll
  | NewEpisodes -> true
  | RecordOnce -> not session.existed
  | ReplayOnly -> false

let miss_error = fun session mode request ->
  let request = Recording.request_fingerprint request in
  let recording = Recording.name session.recording in
  match mode with
  | RecordOnce when session.existed -> RecordOnceMiss { recording; request }
  | ReplayOnly -> ReplayOnlyMiss { recording; request }
  | RecordOnce
  | RecordAll
  | NewEpisodes -> ReplayOnlyMiss { recording; request }

let perform_upstream = fun session request ->
  match H.execute session.upstream request with
  | Ok (response, _telemetry) -> Ok response
  | Error error ->
      Error (UpstreamFailed {
        recording = Recording.name session.recording;
        request = Recording.request_fingerprint request;
        error;
      })

let record_interaction = fun session request response ->
  let redact_headers = redact_headers session.recorder in
  let interaction = {
    Recording.request = Recording.from_blink_request ~redact_headers request;
    response = Recording.from_blink_response ~redact_headers response;
  }
  in
  Recording.push session.recording ~value:interaction;
  save_recording session

let handle_request = fun session request ->
  let mode = recording_mode session in
  let replay =
    match mode with
    | RecordAll -> None
    | RecordOnce
    | ReplayOnly
    | NewEpisodes -> Recording.find_interaction session.recording request
  in
  match replay with
  | Some interaction -> Ok (Recording.response_to_blink interaction.response)
  | None ->
      if can_record session mode then
        perform_upstream session request
        |> Result.and_then
          ~fn:(fun response ->
            record_interaction session request response
            |> Result.map ~fn:(fun () -> response))
      else
        Error (miss_error session mode request)

let make_transport = fun session request ->
  match handle_request session request with
  | Ok response -> Ok response
  | Error error ->
      Error (BlinkError.ProtocolError (BlinkError.ApplicationTransportError (error_to_string error)))

let managed_client = fun session ->
  let config = H.Config.make ~transport:(make_transport session) () in
  H.make ~config ()

let shutdown_session = fun session ->
  H.shutdown session.upstream;
  Ok ()

let use_recording = fun recorder ~name ~fn ->
  load_recording recorder name recorder.mode
  |> Result.and_then
    ~fn:(fun (path, existed, recording) ->
      let session = {
        recorder;
        recording;
        path;
        existed;
        upstream = make_upstream recorder;
      }
      in
      let client = managed_client session in
      try
        let value = fn client in
        H.shutdown client;
        shutdown_session session
        |> Result.map ~fn:(fun () -> value)
      with
      | exn ->
          H.shutdown client;
          let _ = shutdown_session session in
          raise exn)
