open Global

(** # Common - Filesystem error types

    Common error types and conversion utilities for filesystem operations.

    ## Error Handling

    All filesystem operations return [Result.t] with this error type:

    ```ocaml open Std

    match Fs.read (Path.v "config.json") with 
    | Ok content -> process content 
    | Error err -> Log.error "Filesystem error: %s" (Fs.Common.error_message err)
    ```

    ## When to Use

    This module is primarily internal. Users typically work with the [Result.t]
    values returned by [Fs] functions and use [Result.expect] or pattern
    matching for error handling. *)

type error = Kernel.IO.error

(** Filesystem error type - preserves structured error info. *)
val error_message: error -> string

(** Convert a filesystem error to a human-readable message. *)
val convert_kernel_result: ('a, Kernel.IO.error) result -> ('a, error) result

(** Convert a kernel result to a filesystem result (currently a no-op since types match). *)
