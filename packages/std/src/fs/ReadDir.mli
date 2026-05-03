(**
   # ReadDir - Directory iteration

   Iterator for reading directory entries. Automatically skips `.` and `..`
   entries.

   ## Examples

   ```ocaml open Std

   (* Iterate through directory entries *) let dir = Fs.ReadDir.open_dir (Path.v
   "src") |> Result.expect ~msg:"Failed to open directory" in

   let rec process_entries () = match Fs.ReadDir.next dir with | Some entry ->
   Log.info "Found: %s" (Path.to_string entry.path); process_entries () | None ->
   Fs.ReadDir.close dir |> ignore in process_entries () ```

   ## Notes

   - Automatically filters out `.` and `..` entries
   - Returns relative entry paths
   - Must call [close] to release resources

   See [Fs.read_dir] for a simpler API that returns all entries at once.
*)

open Global
open Common

(** Directory reading iterator. *)
type t
type state = t
(**
   Lightweight kind hint derived from the directory entry itself.

   This avoids a metadata syscall on the common path. `Unknown` means the
   platform could not classify the entry cheaply.
*)
type entry_kind = Kernel.Fs.ReadDir.kind =
  | RegularFile
  | Directory
  | SymbolicLink
  | CharacterDevice
  | BlockDevice
  | NamedPipe
  | Socket
  | Unknown
(** One validated relative directory entry returned by [next]. *)
type entry = {
  path: Path.t;
  kind: entry_kind;
}
(** Directory entry item returned by the iterator surface. *)
type item = entry

(** Open a directory for reading. *)
val open_dir: Path.t -> (t, error) result

(**
   Get next entry from directory, skipping `.` and `..`, along with its cheap
   kind hint.
*)
val next: t -> entry option

(** Close the directory handle. *)
val close: t -> (unit, error) Result.t

(** MutIterator interface. *)
val size: t -> int

val clone: t -> t
