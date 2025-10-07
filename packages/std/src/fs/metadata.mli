(** # Fs.Metadata - File metadata and attributes

    File metadata including type, size, permissions, and timestamps. Obtained
    from filesystem stat operations.

    ## Examples

    Getting file metadata:

    ```ocaml open Std

    let path = Path.v "data.txt" in match Fs.metadata path with | Ok meta -> if
    Metadata.is_file meta then Log.info "File size: %d bytes" (Metadata.len
    meta);

    let perms = Metadata.permissions meta in if Permissions.user_write perms
    then Log.info "File is writable" | Error err -> Log.error "Cannot stat file"
    ```

    Checking file types:

    ```ocaml match Fs.metadata path with | Ok meta -> (match Metadata.file_type
    meta with | `Regular -> Log.info "Regular file" | `Directory -> Log.info
    "Directory" | `Symlink -> Log.info "Symbolic link" | _ -> Log.info "Special
    file") | Error err -> () ```

    Working with timestamps:

    ```ocaml let meta = Fs.metadata path |> Result.unwrap in let mtime =
    Metadata.modified meta in let atime = Metadata.accessed meta in

    Log.info "Modified: %f, Accessed: %f" mtime atime;

    match Metadata.created meta with | Some ctime -> Log.info "Created: %f"
    ctime | None -> Log.info "Creation time not available" ``` *)

type t = Kernel.Fs.File.Metadata.t
(** File metadata from filesystem stat operations. *)

(** ## File Properties *)

val file_type :
  t ->
  [ `Regular | `Directory | `Symlink | `Block | `Character | `Fifo | `Socket ]
(** Returns the file type.

    ## Examples

    ```ocaml match Metadata.file_type meta with | `Regular -> "regular file" |
    `Directory -> "directory" | `Symlink -> "symbolic link" | `Block -> "block
    device" | `Character -> "character device" | `Fifo -> "named pipe" | `Socket
    -> "Unix socket" ``` *)

val is_file : t -> bool
(** Returns [true] if this is a regular file.

    ## Examples

    ```ocaml if Metadata.is_file meta then process_file path ``` *)

val is_dir : t -> bool
(** Returns [true] if this is a directory.

    ## Examples

    ```ocaml if Metadata.is_dir meta then list_directory path ``` *)

val is_symlink : t -> bool
(** Returns [true] if this is a symbolic link.

    ## Examples

    ```ocaml if Metadata.is_symlink meta then Log.warn "Following symlink" ```
*)

val len : t -> int
(** Returns file size in bytes.

    ## Examples

    ```ocaml let size = Metadata.len meta in if size > 1_000_000 then Log.warn
    "File is larger than 1MB" ```

    ## Note

    For directories, this is the size of the directory structure itself, not the
    total size of contained files. *)

val permissions : t -> Permissions.t
(** Returns file permissions.

    ## Examples

    ```ocaml let perms = Metadata.permissions meta in if Permissions.user_write
    perms then modify_file path ``` *)

(** ## Timestamps *)

val accessed : t -> float
(** Returns last access time (atime) as seconds since Unix epoch.

    ## Examples

    ```ocaml let atime = Metadata.accessed meta in let days_ago = (Unix.time ()
    -. atime) /. 86400.0 in Log.info "Accessed %.1f days ago" days_ago ```

    ## Note

    Some filesystems or mount options (noatime) may not update access times. *)

val modified : t -> float
(** Returns last modification time (mtime) as seconds since Unix epoch.

    ## Examples

    ```ocaml let mtime = Metadata.modified meta in if mtime > last_build_time
    then rebuild_needed () ``` *)

val created : t -> float option
(** Returns creation time (birth time) if available.

    ## Examples

    ```ocaml match Metadata.created meta with | Some ctime -> Log.info "Created:
    %f" ctime | None -> Log.info "Creation time not available" ```

    ## Platform Support

    - **macOS**: Returns creation time (birth time)
    - **Linux**: Returns None (most filesystems don't track creation time)
    - **Windows**: Returns creation time *)

(** ## Unix-specific *)

val mode : t -> int
(** Returns Unix mode bits (permissions + file type).

    ## Examples

    ```ocaml let mode = Metadata.mode meta in Printf.printf "Mode: 0o%o\n" mode
    ``` *)

val uid : t -> int
(** Returns user ID of the file owner.

    ## Examples

    ```ocaml let uid = Metadata.uid meta in if uid = Unix.getuid () then
    Log.info "You own this file" ``` *)

val gid : t -> int
(** Returns group ID of the file.

    ## Examples

    ```ocaml let gid = Metadata.gid meta ``` *)

val nlink : t -> int
(** Returns number of hard links to the file.

    ## Examples

    ```ocaml let links = Metadata.nlink meta in if links > 1 then Log.info "File
    has %d hard links" links ``` *)

val ino : t -> int
(** Returns inode number.

    ## Examples

    ```ocaml let inode = Metadata.ino meta ``` *)

val dev : t -> int
(** Returns device ID containing the file.

    ## Examples

    ```ocaml let device = Metadata.dev meta ``` *)

val rdev : t -> int
(** Returns device ID for special files (block/character devices).

    ## Examples

    ```ocaml if Metadata.file_type meta = `Block then let dev_id = Metadata.rdev
    meta ``` *)
