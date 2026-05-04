(**
   Filesystem operations with path-typed Result APIs.

   Use `Fs` for safe, ergonomic filesystem operations. All path operations use
   `Path.t` instead of strings.

   ## Examples

   Basic file operations:

   ```ocaml open Std

   (* Read a file *) let content = Fs.read (Path.v "config.toml") |>
   Result.expect ~msg:"Config file required"

   (* Write a file *) Fs.write "Hello, world!" (Path.v "output.txt") |>
   Result.expect ~msg:"Failed to write"

   (* Check if path exists *) if Fs.exists (Path.v "data.json") |>
   Result.unwrap_or ~default:false then println "Data file found" ```

   Directory operations:

   ```ocaml (* Create directory tree *) Fs.create_dir_all (Path.v
   "output/results/2024") |> Result.expect ~msg:"Failed to create directories"

   (* Read directory contents *) match Fs.read_dir (Path.v "src") with | Ok
   iter -> MutIterator.iter (fun path -> println "Found: %s" (Path.to_string
   path) ) iter | Error e -> println "Error: %s" (show_error e) ```

   ## Time-of-Check to Time-of-Use (TOCTOU)

   Many filesystem operations are subject to TOCTOU race conditions. This
   occurs when checking a condition (like file existence) and then using that
   information, but the condition may change between check and use.

   For example: ```ocaml (* TOCTOU vulnerable - file might be created between
   check and write *) if not (Fs.exists path |> Result.unwrap_or
   ~default:false) then Fs.write content path (* Another process might create
   it first *) ```

   To avoid TOCTOU issues:
   - Use atomic operations when possible (e.g., [`File.create_new`])
   - Keep files open for the duration of operations
   - Be aware that metadata operations may be affected by concurrent changes

   ## Error Handling

   All operations return `Result.t` for explicit error handling. Never throws
   exceptions for I/O errors. Common error conditions include permission denied,
   file not found, disk full, etc. Filesystem operations use the shared
   {!Std.IO.error} surface today.

   ## Platform-specific behavior

   This module uses OCaml's Unix module internally, which provides
   cross-platform filesystem operations. Behavior may vary slightly between
   Unix-like systems and Windows, particularly for permissions and symbolic
   links.
*)
open Iter

type error = IO.error

module Permissions: sig
  (** Unix file permissions. *)
  type t

  (** Create from Unix mode bits *)
  val from_mode: int -> t

  (** Convert to Unix mode bits *)
  val to_mode: t -> int

  (** Check if no write bits are set *)
  val readonly: t -> bool

  (** Set or clear all write permissions *)
  val set_readonly: t -> bool -> t

  (** Check owner read permission *)
  val user_read: t -> bool

  (** Check owner write permission *)
  val user_write: t -> bool

  (** Check owner execute permission *)
  val user_execute: t -> bool

  (** Check group read permission *)
  val group_read: t -> bool

  (** Check group write permission *)
  val group_write: t -> bool

  (** Check group execute permission *)
  val group_execute: t -> bool

  (** Check others read permission *)
  val other_read: t -> bool

  (** Check others write permission *)
  val other_write: t -> bool

  (** Check others execute permission *)
  val other_execute: t -> bool

  (** rw-r--r-- (0644) - Owner read/write, group/others read-only *)
  val read_write: t

  (** rwxr-xr-x (0755) - Owner read/write/execute, group/others read/execute *)
  val executable: t

  (** rw------- (0600) - Owner read/write only, no access for others *)
  val private_read_write: t

  (** rwx------ (0700) - Owner read/write/execute only, no access for others *)
  val private_executable: t
end

module Metadata: sig
  (** Filesystem metadata. *)
  type t
  (** File kind reported by the operating system. *)
  type file_type =
    | Regular
    | Directory
    | Symlink
    | Block
    | Character
    | Fifo
    | Socket
    | Unknown

  val file_type: t -> file_type

  (** Check if it's a regular file *)
  val is_file: t -> bool

  (** Check if it's a directory *)
  val is_dir: t -> bool

  (** Check if it's a symbolic link *)
  val is_symlink: t -> bool

  (** Get file size in bytes *)
  val len: t -> int

  (** Get file permissions *)
  val permissions: t -> Permissions.t

  (** Last access time *)
  val accessed: t -> float

  (** Last modification time *)
  val modified: t -> float

  (** Creation time (platform-specific, may be None) *)
  val created: t -> float option

  (** Unix mode bits *)
  val mode: t -> int

  (** User ID of owner *)
  val uid: t -> int

  (** Group ID of owner *)
  val gid: t -> int

  (** Number of hard links *)
  val nlink: t -> int

  (** Inode number *)
  val ino: t -> int

  (** Device number *)
  val dev: t -> int

  (** Device type (if special file) *)
  val rdev: t -> int
end

module ReadDir: sig
  (** Opaque directory handle. *)
  type t
  (** Directory entry kind. *)
  type entry_kind = Kernel.Fs.ReadDir.kind =
    | RegularFile
    | Directory
    | SymbolicLink
    | CharacterDevice
    | BlockDevice
    | NamedPipe
    | Socket
    | Unknown
  type entry = {
    path: Path.t;
    kind: entry_kind;
  }

  (** Open a directory. *)
  val open_dir: Path.t -> (t, error) Result.t

  (** Get next entry, or None when done. Skips . and .. *)
  val next: t -> entry option

  (** Close the directory handle *)
  val close: t -> (unit, error) Result.t
end

module File = File

module Walker = Walker

(**
   Returns the canonical, absolute form of a path.

   All intermediate components are normalized and symbolic links resolved. The
   path must exist.

   ## Examples

   ```ocaml (* Resolve relative paths and symlinks *) let abs_path =
   Fs.canonicalize (Path.v "../data/input.txt") |> Result.expect ~msg:"Path
   must exist" (* abs_path might be "/home/user/project/data/input.txt" *)

   (* Follow symlinks to real file *) let real_file = Fs.canonicalize (Path.v
   "latest.log") (* symlink *) |> Result.unwrap (* real_file might be
   "logs/2024-01-15.log" *) ```
*)
val canonicalize: Path.t -> (Path.t, error) Result.t

(**
   Copies file contents and permissions from source to destination.

   Overwrites destination if it exists. Creates parent directories if needed.

   ## Examples

   ```ocaml (* Simple file copy *) Fs.copy ~src:(Path.v "template.html")
   ~dst:(Path.v "index.html") |> Result.expect ~msg:"Copy failed"

   (* Backup before modifying *) let backup_file path = let backup =
   Path.add_extension path ~ext:"bak" in Fs.copy ~src:path ~dst:backup ```

   ## Errors

   - Source file doesn't exist
   - Insufficient permissions
   - Disk full
*)
val copy: src:Path.t -> dst:Path.t -> (unit, error) Result.t

(**
   Creates a single directory.

   Fails if parent doesn't exist or directory already exists.

   ## Examples

   ```ocaml (* Create directory in existing parent *) Fs.create_dir (Path.v
   "output") |> Result.expect ~msg:"Failed to create output dir"

   (* Handle existing directory *) match Fs.create_dir (Path.v "cache") with |
   Ok () -> println "Created cache directory" | Error _ -> println "Cache
   directory already exists" ```
*)
val create_dir: Path.t -> (unit, error) Result.t

(**
   Recursively creates directory and all parent directories.

   Like `mkdir -p`. Succeeds if directory already exists.

   ## Examples

   ```ocaml (* Create deep directory structure *) Fs.create_dir_all (Path.v
   "output/2024/01/15") |> Result.expect ~msg:"Failed to create directories"

   (* Ensure directory exists before writing *) let write_to_dir dir filename
   content = Fs.create_dir_all dir |> Result.and_then (fun () -> Fs.write
   content (dir / Path.v filename) ) ```
*)
val create_dir_all: Path.t -> (unit, error) Result.t

(**
   Returns `Ok true` if the path points at an existing entity.

   This function will traverse symbolic links to query the destination. For
   broken symbolic links, returns `Ok false`.

   As opposed to a simple boolean check, this returns `Ok true` or `Ok false`
   only if existence was *verified*. If existence can neither be confirmed nor
   denied (e.g., permission denied on parent directory), returns `Error`.

   ## Examples

   ```ocaml (* Simple existence check *) if Fs.exists (Path.v "config.json") |>
   Result.unwrap_or ~default:false then println "Config found"

   (* Distinguish between not found and error *) match Fs.exists (Path.v
   "/root/secret.txt") with | Ok false -> println "File doesn't exist" | Ok
   true -> println "File exists" | Error _ -> println "Cannot determine
   (permission denied?)" ```

   ## TOCTOU Warning

   Note that while this avoids some pitfalls, it still cannot prevent TOCTOU
   bugs. The file's existence may change between this check and subsequent use.
   Only use where TOCTOU is not a concern.

   ## See Also

   - [`is_file`] - Check if path is specifically a file
   - [`is_dir`] - Check if path is specifically a directory
   - [`File.create_new`] - Atomically create if doesn't exist
*)
val exists: Path.t -> (bool, error) Result.t

(**
   Creates a hard link to an existing file.

   Both paths will refer to the same file data. Changes through either path
   affect the same file.

   ## Examples

   ```ocaml (* Create hard link for important file *) Fs.hard_link ~src:(Path.v
   "data.db") ~dst:(Path.v "data.db.link") |> Result.expect ~msg:"Failed to
   create hard link" ```
*)
val hard_link: src:Path.t -> dst:Path.t -> (unit, error) Result.t

(**
   Reads entire file contents as a UTF-8 string.

   Best for reasonably-sized text files. For large files, consider streaming
   with `File` module.

   ## Examples

   ```ocaml (* Read configuration file *) let config = Fs.read (Path.v
   "config.toml") |> Result.expect ~msg:"Config required"

   (* Read and parse JSON *) let data = Fs.read (Path.v "data.json") |>
   Result.and_then Data.Json.parse |> Result.expect ~msg:"Invalid JSON"

   (* Read with default value *) let readme = Fs.read (Path.v "README.md") |>
   Result.unwrap_or ~default:"No documentation available" ```

   ## Errors

   - File doesn't exist
   - Permission denied
   - Not a regular file (e.g., directory)
   - Invalid UTF-8 content

   ## See Also

   - [`read_to_string`] - Alias for this function
   - [`File.open_`] and [`File.read_all`] - For more control
   - [`File.read_lines`] - To read line by line
*)
val read: Path.t -> (string, error) Result.t

(**
   Returns an iterator over directory entries.

   Automatically skips `.` and `..` entries. The iterator yields full paths
   (directory path joined with entry name).

   ## Examples

   ```ocaml (* List all files in directory *) match Fs.read_dir (Path.v "src")
   with | Ok iter -> MutIterator.iter (fun path -> println "- %s"
   (Path.to_string path) ) iter | Error e -> println "Cannot read directory:
   %s" (show_error e)

   (* Find specific files *) let find_ml_files dir = Fs.read_dir dir |>
   Result.map (fun iter -> MutIterator.filter (fun path -> Path.extension path
   = Some "ml" ) iter |> MutIterator.to_list )

   (* Process directory recursively *) let rec process_tree dir = match
   Fs.read_dir dir with | Ok iter -> MutIterator.iter (fun path -> if Fs.is_dir
   path |> Result.unwrap_or ~default:false then process_tree path else
   process_file path ) iter | Error _ -> () ```
*)
val read_dir: Path.t -> (Path.t MutIterator.t, error) Result.t

(**
   Reads a symbolic link target.

   Returns the path the symlink points to (may be relative). Does not resolve
   the target.

   ## Examples

   ```ocaml (* Read symlink target *) match Fs.read_link (Path.v "latest") with
   | Ok target -> println "Latest points to: %s" (Path.to_string target) |
   Error _ -> println "Not a symbolic link" ```
*)
val read_link: Path.t -> (Path.t, error) Result.t

(**
   Reads entire file as string (alias for `read`).

   ## Examples

   ```ocaml let content = Fs.read_to_string (Path.v "data.txt") |>
   Result.expect ~msg:"Cannot read file" ```
*)
val read_to_string: Path.t -> (string, error) Result.t

(**
   Removes an empty directory.

   Fails if directory is not empty or doesn't exist.

   ## Examples

   ```ocaml (* Remove temporary directory after cleanup *) Fs.remove_dir
   (Path.v "tmp") |> Result.expect ~msg:"Directory not empty" ```
*)
val remove_dir: Path.t -> (unit, error) Result.t

(**
   Recursively removes directory and all contents.

   Like `rm -rf`. **Use with extreme caution!**

   This function does **not** follow symbolic links - it will remove the
   symlink itself, not the target.

   ## Examples

   ```ocaml (* Clean build artifacts *) Fs.remove_dir_all (Path.v "_build") |>
   Result.unwrap_or ~default:() (* Ignore if doesn't exist *)

   (* Safe cleanup with confirmation *) let cleanup_if_safe dir = if not
   (Path.is_absolute dir) then Fs.remove_dir_all dir else Error (IO.Unknown_error
   "Won't delete absolute paths") ```

   ## Errors

   - Path doesn't exist (returns error, not idempotent)
   - Path is not a directory
   - Permission denied
   - Directory being modified concurrently (partial deletion)

   ## TOCTOU Considerations

   On most platforms, this function protects against symlink TOCTOU races by
   using directory file descriptors. However, concurrent modifications to the
   directory tree may cause partial deletion.
*)
val remove_dir_all: Path.t -> (unit, error) Result.t

(**
   Removes a file.

   Fails if path is a directory or doesn't exist.

   ## Examples

   ```ocaml (* Delete temporary file *) Fs.remove_file (Path.v "output.tmp") |>
   Result.expect ~msg:"Failed to remove temp file"

   (* Clean up multiple files *) ["a.tmp"; "b.tmp"; "c.tmp"] |> List.iter (fun
   name -> Fs.remove_file (Path.v name) |> ignore ) ```
*)
val remove_file: Path.t -> (unit, error) Result.t

(**
   Renames file or directory, replacing destination if it exists.

   Can move across directories but not filesystems.

   ## Examples

   ```ocaml (* Simple rename *) Fs.rename ~src:(Path.v "draft.txt")
   ~dst:(Path.v "final.txt") |> Result.expect ~msg:"Rename failed"

   (* Move to different directory *) Fs.rename ~src:(Path.v
   "downloads/file.pdf") ~dst:(Path.v "documents/file.pdf") |> Result.expect
   ~msg:"Move failed"

   (* Atomic file update pattern *) let atomic_write path content = let tmp =
   Path.add_extension path ~ext:"tmp" in Fs.write content tmp |>
   Result.and_then (fun () -> Fs.rename ~src:tmp ~dst:path ) ```
*)
val rename: src:Path.t -> dst:Path.t -> (unit, error) Result.t

(**
   Changes file or directory permissions.

   ## Examples

   ```ocaml (* Make file executable *) Fs.set_permissions (Path.v "script.sh")
   Permissions.executable |> Result.expect ~msg:"Cannot change permissions"

   (* Make file read-only *) Fs.set_permissions (Path.v "config.locked")
   Permissions.read_write |> Result.expect ~msg:"Cannot change permissions"

   (* Restrict access to owner only *) Fs.set_permissions (Path.v
   "secrets.txt") Permissions.private_read_write |> Result.expect ~msg:"Cannot
   secure file" ```
*)
val set_permissions: Path.t -> Permissions.t -> (unit, error) Result.t

(**
   Creates a symbolic link.

   `dst` is the name of the symlink, `src` is what it points to.

   ## Examples

   ```ocaml (* Create symlink to latest log *) Fs.symlink ~src:(Path.v
   "logs/2024-01-15.log") ~dst:(Path.v "latest.log") |> Result.expect
   ~msg:"Cannot create symlink"

   (* Link to directory *) Fs.symlink ~src:(Path.v "/usr/local/bin")
   ~dst:(Path.v "bin") |> Result.expect ~msg:"Cannot create symlink" ```
*)
val symlink: src:Path.t -> dst:Path.t -> (unit, error) Result.t

(**
   Writes string to file, creating or overwriting it.

   This function will **overwrite** existing file contents. Parent directories
   are NOT automatically created.

   ## Examples

   ```ocaml (* Write simple file *) Fs.write "Hello, World!" (Path.v
   "greeting.txt") |> Result.expect ~msg:"Write failed"

   (* Write JSON data *) let save_json data path = Data.Json.to_string data |>
   fun json -> Fs.write json path

   (* Atomic write pattern *) let atomic_write path content = let tmp =
   Path.add_extension path ~ext:"tmp" in Fs.write content tmp |>
   Result.and_then (fun () -> Fs.rename ~src:tmp ~dst:path ) ```

   ## Errors

   - Parent directory doesn't exist
   - Permission denied
   - Disk full
   - Path is a directory

   ## Platform-specific behavior

   On Unix, creates files with mode 0o666 (modified by umask). On Windows, uses
   default file permissions.

   ## See Also

   - [`File.create`] - For writing with specific options
   - [`File.append`] - To append instead of overwrite
   - [`create_dir_all`] - To ensure parent directories exist
*)
val write: string -> Path.t -> (unit, error) Result.t

(**
   Gets file metadata, following symbolic links.

   Use this for getting size, permissions, timestamps, etc.

   ## Examples

   ```ocaml (* Get file size *) let size = Fs.metadata (Path.v "data.bin") |>
   Result.map Metadata.len |> Result.unwrap_or ~default:0

   (* Check file age *) let is_stale path max_age = match Fs.metadata path with
   | Ok meta -> let age = Unix.time () -. Metadata.modified meta in age >
   max_age | Error _ -> true

   (* Get all file info *) match Fs.metadata (Path.v "important.doc") with | Ok
   meta -> println "Size: %d bytes" (Metadata.len meta); println "Modified:
   %.0f" (Metadata.modified meta); println "Permissions: %o" (Metadata.mode
   meta) | Error e -> println "Cannot stat file: %s" (show_error e) ```
*)
val metadata: Path.t -> (Metadata.t, error) Result.t

(**
   Gets metadata without following symbolic links.

   Use this to get information about the symlink itself.

   ## Examples

   ```ocaml (* Check if path is a symlink *) let is_symlink path = match
   Fs.symlink_metadata path with | Ok meta -> Metadata.is_symlink meta | Error
   _ -> false ```
*)
val symlink_metadata: Path.t -> (Metadata.t, error) Result.t

(**
   Checks if path is a regular file (not directory or symlink).

   ## Examples

   ```ocaml (* Filter files from paths *) let files_only paths = List.filter
   (fun p -> Fs.is_file p |> Result.unwrap_or ~default:false ) paths

   (* Validate input *) let process_file path = match Fs.is_file path with | Ok
   true -> do_processing path | Ok false -> Error "Not a file" | Error e ->
   Error (show_error e) ```
*)
val is_file: Path.t -> (bool, error) Result.t

(**
   Checks if path is a directory.

   ## Examples

   ```ocaml (* Ensure directory exists *) let ensure_dir path = match Fs.is_dir
   path with | Ok true -> Ok () | Ok false -> Error "Path exists but is not a
   directory" | Error _ -> Fs.create_dir path

   (* Recursive traversal *) let rec count_files dir = if not (Fs.is_dir dir |>
   Result.unwrap_or ~default:false) then 0 else match Fs.read_dir dir with | Ok
   iter -> MutIterator.fold (fun acc path -> if Fs.is_dir path |>
   Result.unwrap_or ~default:false then acc + count_files path else acc + 1 ) 0
   iter | Error _ -> 0 ```
*)
val is_dir: Path.t -> (bool, error) Result.t

(**
   Creates a temporary directory, runs a function, then cleans up.

   The directory is automatically removed even if the function raises an
   exception. Perfect for tests and temporary workspaces.

   ## Examples

   ```ocaml (* Run tests in isolated environment *) let test_result =
   Fs.with_tempdir (fun tmpdir -> (* tmpdir is created and empty *) let
   test_file = tmpdir / Path.v "test.txt" in Fs.write "test data" test_file |>
   Result.unwrap;

   (* Run actual tests *) run_tests tmpdir ) (* tmpdir is automatically deleted
   here *) |> Result.expect ~msg:"Test failed"

   (* Process files in temporary workspace *) let process_archive archive =
   Fs.with_tempdir ~prefix:"extract_" (fun workspace -> extract_to workspace
   archive; let files = Fs.read_dir workspace |> Result.unwrap in
   MutIterator.map process_file files |> MutIterator.to_list )

   (* Cleanup happens even on failure *) let safe_operation () =
   Fs.with_tempdir (fun tmp -> Fs.write "data" (tmp / Path.v "file.txt") |>
   Result.unwrap; panic "Something went wrong" (* tmp still gets cleaned up
   *) ) ```

   - `prefix`: Optional prefix for directory name (default: "tmp")
   - Returns: Result of the function or filesystem error
*)
val with_tempdir: ?prefix:string -> (Path.t -> 'a) -> ('a, error) Result.t

module Event = Event

module FileWatcher = File_watcher
