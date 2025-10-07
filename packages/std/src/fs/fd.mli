(** # Fd - File descriptor

    Raw file descriptor type. This is a low-level type used internally by the
    filesystem operations.

    Most users should use [Fs.File] instead of working with file descriptors
    directly.

    ## Examples

    ```ocaml open Std

    (* Use Fs.File instead of Fd directly *) let file = Fs.File.open_ (Path.v
    "data.txt") ~flags:Fs.File.Read |> Result.expect ~msg:"Failed to open file"
    in

    let content = Fs.File.read_to_string file |> Result.expect ~msg:"Failed to
    read" in

    Fs.File.close file ```

    See [Fs.File] for the high-level file API. *)

type t = Kernel.Fd.t
