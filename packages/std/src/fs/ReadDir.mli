(** # ReadDir - Directory iteration

    Iterator for reading directory entries. Automatically skips `.` and `..`
    entries.

    ## Examples

    ```ocaml open Std

    (* Iterate through directory entries *) let dir = Fs.ReadDir.create (Path.v
    "src") |> Result.expect ~msg:"Failed to open directory" in

    let rec process_entries () = match Fs.ReadDir.next dir with | Some path ->
    Log.info "Found: %s" (Path.to_string path); process_entries () | None ->
    Fs.ReadDir.close dir |> ignore in process_entries () ```

    ## Notes

    - Automatically filters out `.` and `..` entries
    - Returns relative entry paths
    - Must call [close] to release resources

    See [Fs.read_dir] for a simpler API that returns all entries at once. *)

open Global
open Common

(** Directory reading iterator. *)
type t
type state = t
(** Open a directory for reading. *)
type item = Path.t

(** Lightweight kind hint derived from the directory entry itself.

    This avoids a metadata syscall on the common path. `Unknown` means the
    platform could not classify the entry cheaply. *)
type entry_kind =
  | Unknown
  | Regular
  | Directory
  | Symlink
  | Other

(** One relative directory entry returned by [next_entry]. *)
type entry = {
  path: Path.t;
  kind: entry_kind;
}

val create: Path.t -> (t, error) result

(** Get next entry from directory, skipping . and .., along with its cheap
    kind hint. *)
val next_entry: t -> entry option

(** Get next entry from directory, skipping . and .. *)
val next: t -> Path.t option

(** Close the directory handle. *)
val close: t -> (unit, error) Result.t

(** MutIterator interface. *)
val size: t -> int

val clone: t -> t
