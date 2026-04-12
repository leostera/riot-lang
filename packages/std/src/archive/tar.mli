open Global

(** Kind of tar entry encountered while reading an archive. *)
type entry_kind =
  | File
  (** Regular file entry. *)
  | Directory
  (** Directory entry. *)
  | Symlink
  (** Symbolic link entry. *)
  | Hardlink
  (** Hard link entry. *)
  | Other of string
(** Any other tar typeflag not modeled explicitly. *)
(** Metadata for a single tar archive entry. *)
type entry = {
  (** Archive-relative path for the entry. *)
  path: Path.t;
  (** Entry kind. *)
  kind: entry_kind;
  (** Declared payload size in bytes. *)
  size: int64;
  (** POSIX mode bits if present in the archive header. *)
  mode: Fs.Permissions.t option;
  (** Link target for link entries, when present. *)
  link_target: Path.t option;
}
(** Tar-level failures surfaced by the high-level API. *)
type error =
  | Engine_error of Tar_engine.error
  (** The underlying incremental tar reader rejected the archive. *)
  | Invalid_path of string
  (** An archive entry path could not be converted into a valid [`Path.t`]. *)
  | Unsafe_path of string
  (** Extraction rejected an absolute or traversal path such as ["../escape"]. *)
  | Unsupported_entry_kind of entry_kind
  (** Extraction rejected a non-file, non-directory entry kind. *)
  | Duplicate_entry of Path.t
(** Extraction saw the same normalized target path more than once. *)
(** Errors raised while reading archive metadata from an [`IO.Reader`]. *)
type 'read_err read_error =
  | Entries_source_error of 'read_err
  (** The upstream reader failed while feeding tar data. *)
  | Entries_error of error
(** The tar archive itself was invalid or unsafe. *)
(** Errors raised while extracting archive contents. *)
type 'read_err extract_error =
  | Extract_source_error of 'read_err
  (** The upstream reader failed while feeding tar data. *)
  | Extract_fs_error of Fs.error
  (** Filesystem I/O failed while creating directories or writing files. *)
  | Extract_error of error

(** The archive itself was invalid or contained unsafe entries. *)

(** List entries from a tar archive.

    This consumes the entire archive from the provided reader and returns the
    entry metadata in archive order.

    ## Example

    ```ocaml
    open Std

    let list_archive path =
      match Fs.File.open_read path with
      | Error _ -> Error "failed to open archive"
      | Ok file ->
              Global.protect
            ~finally:(fun () -> ignore (Fs.File.close file))
            (fun () ->
              match Archive.Tar.entries (Fs.File.to_reader file) with
              | Ok entries ->
                  List.iter
                    (fun (entry : Archive.Tar.entry) ->
                      Log.info "entry: %s" (Path.to_string entry.path))
                    entries;
                  Ok ()
              | Error _ -> Error "failed to decode tar archive")
    ```
*)
val entries: ('src, 'read_err) IO.Reader.t -> (entry list, 'read_err read_error) result

(** Extract a tar archive into a directory.

    Extraction is conservative by default:

    - absolute paths are rejected
    - path traversal through [`..`] is rejected
    - duplicate normalized target paths are rejected
    - symlinks and hardlinks are rejected in v1

    Only regular files and directories are materialized on disk.

    ## Example

    ```ocaml
    open Std

    let extract_archive src into =
      match Archive.Tar.extract src ~into with
      | Ok () -> Log.info "archive extracted into %s" (Path.to_string into)
      | Error _ -> Log.error "failed to extract archive"
    ```
*)
val extract: ('src, 'read_err) IO.Reader.t -> into:Path.t -> (unit, 'read_err extract_error) result

(** Open a tar archive from disk and list its entries.

    This is a convenience wrapper around [`entries`] for filesystem paths. *)
val entries_file: Path.t -> (entry list, Fs.error read_error) result

(** Open a tar archive from disk and extract it into a directory.

    This is a convenience wrapper around [`extract`] for filesystem paths.

    ## Example

    ```ocaml
    open Std

    let () =
      match Archive.Tar.extract_file ~archive:(Path.v "package.tar") ~into:(Path.v "./out") with
      | Ok () -> Log.info "archive extracted"
      | Error _ -> Log.error "archive extraction failed"
    ```
*)
val extract_file: archive:Path.t -> into:Path.t -> (unit, Fs.error extract_error) result
