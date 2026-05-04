open Global

(**
   Filesystem error types.

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
   matching for error handling.
*)

(** Filesystem error type - preserves structured error info. *)
type error = IO.error

(** Convert a filesystem error to a human-readable message. *)
val error_message: error -> string

val from_file_error: Kernel.Fs.File.error -> error

val from_read_dir_error: Kernel.Fs.ReadDir.error -> error

val convert_kernel_result: ('a, Kernel.Fs.File.error) Kernel.Result.t -> ('a, error) result

val convert_read_dir_result: ('a, Kernel.Fs.ReadDir.error) Kernel.Result.t -> ('a, error) result
