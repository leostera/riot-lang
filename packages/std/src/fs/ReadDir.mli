(** # ReadDir - Directory iteration

    Iterator for reading directory entries. Automatically skips `.` and `..`
    entries.

    ## Examples

    ```ocaml
    open Std

    (* Iterate through directory entries *)
    let dir = Fs.ReadDir.create (Path.v "src")
      |> Result.expect ~msg:"Failed to open directory" in

    let rec process_entries () =
      match Fs.ReadDir.next dir with
      | Some path ->
          Log.info "Found: %s" (Path.to_string path);
          process_entries ()
      | None ->
          Fs.ReadDir.close dir |> ignore
    in
    process_entries ()
    ```

    ## Notes

    - Automatically filters out `.` and `..` entries
    - Returns absolute paths (relative to the opened directory)
    - Must call [close] to release resources

    See [Fs.read_dir] for a simpler API that returns all entries at once.
*)

open Common

type t
(** Directory reading iterator. *)

type state = t
type item = Path.t

val create : Path.t -> (t, error) result
(** Open a directory for reading. *)

val next : t -> Path.t option
(** Get next entry from directory, skipping . and .. *)

val close : t -> (unit, error) Result.t
(** Close the directory handle. *)

val size : t -> int
(** MutIterator interface. *)

val clone : t -> t
