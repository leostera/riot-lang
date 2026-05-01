open Std

type source_path_error =
  | Source_missing
  | Source_not_file
  | Source_not_directory

type io_detail =
  | Fs of Fs.error
  | File of Fs.File.error

type t =
  | Missing of {
      path: Path.t;
    }
  | Invalid_source_path of {
      path: Path.t;
      reason: source_path_error;
    }
  | Io of {
      op: string;
      path: Path.t;
      related_path: Path.t option;
      detail: io_detail;
    }

let source_path_error_message = fun error ->
  match error with
  | Source_missing -> "source path does not exist"
  | Source_not_file -> "source path is not a file"
  | Source_not_directory -> "source path is not a directory"

let io_detail_message = fun detail ->
  match detail with
  | Fs detail -> IO.error_message detail
  | File detail -> Fs.File.error_to_string detail

let error_message = fun error ->
  match error with
  | Missing { path } -> "missing: " ^ Path.to_string path
  | Invalid_source_path { path; reason } ->
      source_path_error_message reason ^ ": " ^ Path.to_string path
  | Io {
      op;
      path;
      related_path = None;
      detail;
    } ->
      op ^ " failed for " ^ Path.to_string path ^ ": " ^ io_detail_message detail
  | Io {
      op;
      path;
      related_path = Some related_path;
      detail;
    } ->
      op
      ^ " failed for "
      ^ Path.to_string path
      ^ " (related: "
      ^ Path.to_string related_path
      ^ "): "
      ^ io_detail_message detail
