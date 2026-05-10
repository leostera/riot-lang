open Std

module Json = Data.Json

type config_error =
  | Missing_env of string
  | Invalid_postgres_url of { env: string; message: string }
  | Invalid_mysql_url of { env: string; message: string }
  | Unsupported_backend of string

type expected_field =
  | ExpectedText
  | ExpectedInt

type missing_field =
  | FieldMissing of string
  | FieldTypeMismatch of {
      field: string;
      expected: expected_field;
      actual: string;
    }
  | JobRowMissing of Job_id.t
  | ActiveUniqueKeyRowMissing of Unique_key.t

type t =
  | Encode_payload of {
      queue: Queue_id.t;
      error: Serde.error;
    }
  | Decode_payload of {
      queue: Queue_id.t;
      job_id: Job_id.t;
      error: Serde.error;
    }
  | Invalid_state of string
  | Missing_field of missing_field
  | Not_started
  | Config of config_error
  | Sqlx of Sqlx.error
  | Migration of Sqlx.Migrate.error

let config_error_to_json = fun error ->
  match error with
  | Missing_env env -> Json.obj [ ("kind", Json.string "missing_env"); ("env", Json.string env); ]
  | Invalid_postgres_url { env; message } ->
      Json.obj
        [
          ("kind", Json.string "invalid_postgres_url");
          ("env", Json.string env);
          ("message", Json.string message);
        ]
  | Invalid_mysql_url { env; message } ->
      Json.obj
        [
          ("kind", Json.string "invalid_mysql_url");
          ("env", Json.string env);
          ("message", Json.string message);
        ]
  | Unsupported_backend backend ->
      Json.obj [ ("kind", Json.string "unsupported_backend"); ("backend", Json.string backend); ]

let expected_field_to_string = fun expected ->
  match expected with
  | ExpectedText -> "text"
  | ExpectedInt -> "int"

let missing_field_to_json = fun missing ->
  match missing with
  | FieldMissing field ->
      Json.obj [ ("kind", Json.string "field_missing"); ("field", Json.string field); ]
  | FieldTypeMismatch { field; expected; actual } ->
      Json.obj
        [
          ("kind", Json.string "field_type_mismatch");
          ("field", Json.string field);
          ("expected", Json.string (expected_field_to_string expected));
          ("actual", Json.string actual);
        ]
  | JobRowMissing job_id ->
      Json.obj
        [
          ("kind", Json.string "job_row_missing");
          ("job_id", Json.string (Job_id.to_string job_id));
        ]
  | ActiveUniqueKeyRowMissing unique_key ->
      Json.obj
        [
          ("kind", Json.string "active_unique_key_row_missing");
          ("unique_key", Json.string (Unique_key.to_string unique_key));
        ]

let to_json = fun error ->
  match error with
  | Encode_payload { queue; error } ->
      Json.obj
        [
          ("kind", Json.string "encode_payload");
          ("queue", Json.string (Queue_id.to_string queue));
          ("message", Json.string (Serde.Error.to_string error));
        ]
  | Decode_payload { queue; job_id; error } ->
      Json.obj
        [
          ("kind", Json.string "decode_payload");
          ("queue", Json.string (Queue_id.to_string queue));
          ("job_id", Json.string (Job_id.to_string job_id));
          ("message", Json.string (Serde.Error.to_string error));
        ]
  | Invalid_state value ->
      Json.obj [ ("kind", Json.string "invalid_state"); ("state", Json.string value); ]
  | Missing_field missing ->
      Json.obj [ ("kind", Json.string "missing_field"); ("error", missing_field_to_json missing); ]
  | Not_started -> Json.obj [ ("kind", Json.string "not_started"); ]
  | Config error ->
      Json.obj [ ("kind", Json.string "config"); ("error", config_error_to_json error); ]
  | Sqlx error ->
      Json.obj [ ("kind", Json.string "sqlx"); ("message", Json.string (Sqlx.show_error error)); ]
  | Migration error ->
      Json.obj
        [
          ("kind", Json.string "migration");
          ("message", Json.string (Sqlx.Migrate.error_to_string error));
        ]

let config_error_to_string = fun error ->
  match error with
  | Missing_env env -> "missing environment variable " ^ env
  | Invalid_postgres_url { env; message } -> "invalid PostgreSQL URL in " ^ env ^ ": " ^ message
  | Invalid_mysql_url { env; message } -> "invalid MySQL URL in " ^ env ^ ": " ^ message
  | Unsupported_backend backend -> "unsupported suri-jobs backend: " ^ backend

let missing_field_to_string = fun missing ->
  match missing with
  | FieldMissing field -> "missing field: " ^ field
  | FieldTypeMismatch { field; expected; actual } ->
      "field " ^ field ^ " expected " ^ expected_field_to_string expected ^ ", got " ^ actual
  | JobRowMissing job_id -> "missing job row for suri_jobs.job_id=" ^ Job_id.to_string job_id
  | ActiveUniqueKeyRowMissing unique_key ->
      "missing active job row for suri_jobs.unique_key=" ^ Unique_key.to_string unique_key

let to_string = fun error ->
  match error with
  | Encode_payload { queue; error } ->
      "failed to encode payload for queue "
      ^ Queue_id.to_string queue
      ^ ": "
      ^ Serde.Error.to_string error
  | Decode_payload { queue; job_id; error } ->
      "failed to decode payload for queue "
      ^ Queue_id.to_string queue
      ^ " job "
      ^ Job_id.to_string job_id
      ^ ": "
      ^ Serde.Error.to_string error
  | Invalid_state value -> "invalid job state: " ^ value
  | Missing_field missing -> missing_field_to_string missing
  | Not_started -> "suri-jobs has not been started with a Sqlx pool"
  | Config error -> config_error_to_string error
  | Sqlx error -> Sqlx.show_error error
  | Migration error -> Sqlx.Migrate.error_to_string error
