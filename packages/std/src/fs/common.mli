(** # Common - Filesystem error types

    Common error types and conversion utilities for filesystem operations.

    ## Error Handling

    All filesystem operations return [Result.t] with this error type:

    ```ocaml
    open Std

    match Fs.read (Path.v "config.json") with
    | Ok content -> process content
    | Error (Fs.Common.SystemError msg) ->
        Log.error "Filesystem error: %s" msg
    ```

    ## When to Use

    This module is primarily internal. Users typically work with the
    [Result.t] values returned by [Fs] functions and use [Result.expect]
    or pattern matching for error handling.
*)

type error = SystemError of string
(** Filesystem error type. *)

val kernel_error_to_string :
  [> `Closed
  | `Connection_closed
  | `Eof
  | `Exn of exn
  | `No_info
  | `Noop
  | `Process_down
  | `Timeout
  | `IO_error of Kernel.IO.error
  | `Would_block ] ->
  string

val convert_kernel_result :
  ( 'a,
    [> `Closed
    | `Connection_closed
    | `Eof
    | `Exn of exn
    | `No_info
    | `Noop
    | `Process_down
    | `Timeout
    | `IO_error of Kernel.IO.error
    | `Would_block ] )
  result ->
  ('a, error) result
