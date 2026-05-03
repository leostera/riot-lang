(**
   Type-safe filesystem paths.

   This module provides a type-safe wrapper for filesystem paths, ensuring all
   paths are valid UTF-8 strings. Similar to Rust's `std::path::Path`, but
   owned (like `PathBuf`) since OCaml has garbage collection.

   ## Examples

   Basic path manipulation:

   ```ocaml open Std

   (* Create paths *) let home = Path.v "/home/user" in let config = home /
   Path.v ".config" / Path.v "app.toml" in (* config =
   "/home/user/.config/app.toml" *)

   (* Extract components *) let file = Path.v "/dir/file.txt" in let parent =
   Path.parent file in (* Some "/dir" *) let name = Path.basename file in (*
   "file.txt" *) let ext = Path.extension file in (* Some "txt" *)

   (* Check properties *) if Path.exists config then println "Config found at
   %s" (Path.to_string config) ```

   ## Path Safety

   Unlike string-based paths, this module ensures:
   - All paths are valid UTF-8
   - Path operations preserve validity
   - Type safety prevents mixing paths with regular strings

   ## Platform Differences

   Path separators are platform-specific:
   - Unix-like systems: `/`
   - Windows: `\` (though `/` is also accepted)
*)

(** The type of filesystem paths. Always contains valid UTF-8. *)
type t = string
type error =
  (** Path contains invalid UTF-8 bytes *)
  | InvalidUtf8 of { path: string }
  (** System call returned invalid UTF-8 *)
  | SystemInvalidUtf8 of { syscall: string; path: string }
  (** Other system-level error *)
  | SystemError of string

(**
   Creates a path from a string, validating UTF-8 encoding.

   ## Examples

   ```ocaml
   (* Safe construction with error handling *)
   match Path.of_string "/home/user" with
   | Ok path -> println "Valid path: %s" (Path.to_string path)
   | Error (InvalidUtf8 {path}) -> println "Invalid UTF-8 in: %s" path

   (* Handle user input *)
   let parse_path input =
     Path.of_string input
     |> Result.map_err (fun _ -> "Invalid path provided")
   ```
*)
val from_string: string -> (t, error) Result.t

val from_string_unchecked: string -> t

(**
   Creates a path from a string literal.

   ## Panics

   Panics if the string is not valid UTF-8. Use this only with string literals
   or when you're certain the string is valid.

   ## Examples

   ```ocaml (* Safe with literals *) let home = Path.v "/home/user" in let
   config = Path.v "config.toml" in

   (* Building paths *) let full_path = home / config (* full_path =
   "/home/user/config.toml" *) ```
*)
val v: string -> t

(**
   Converts a path to a UTF-8 string.

   The returned string is always valid UTF-8 since paths are validated at
   construction time.

   ## Examples

   ```ocaml let path = Path.v "/usr/local/bin" in Printf.printf "Path: %s\n"
   (Path.to_string path);

   (* Use with string functions *) let contains_local path = String.contains
   (Path.to_string path) "local" ```
*)
val to_string: t -> string

(**
   Joins two paths together with a path separator.

   ## Examples

   ```ocaml let dir = Path.v "/usr" in let subdir = Path.v "local/bin" in let
   full = Path.join dir subdir in (* full = "/usr/local/bin" *)

   (* Joining absolute paths replaces the first path *) let path1 = Path.v
   "/home/user" in let path2 = Path.v "/etc" in let result = Path.join path1
   path2 in (* result = "/etc" *) ```
*)
val join: t -> t -> t

(**
   Infix operator for joining paths.

   Allows natural path construction with chaining.

   ## Examples

   ```ocaml let path = Path.v "/home" / Path.v "user" / Path.v "documents" in
   (* path = "/home/user/documents" *)

   (* Build paths incrementally *) let home = Path.v (Sys.getenv "HOME") in let
   config = home / Path.v ".config" / Path.v "myapp" in let settings = config /
   Path.v "settings.json" ```
*)
val ( / ): t -> t -> t

(**
   Returns the parent directory, if any.

   Returns [`None`] for root paths or single components.

   ## Examples

   ```ocaml let path = Path.v "/home/user/file.txt" in assert (Path.parent path
   = Some (Path.v "/home/user"));

   let root = Path.v "/" in assert (Path.parent root = None);

   let relative = Path.v "file.txt" in assert (Path.parent relative = None) ```
*)
val parent: t -> t option

(**
   Returns the final component of a path as a string.

   Returns empty string for root paths.

   ## Examples

   ```ocaml assert (Path.basename (Path.v "/home/user/file.txt") = "file.txt");
   assert (Path.basename (Path.v "/home/user/") = "user"); assert
   (Path.basename (Path.v "/") = ""); assert (Path.basename (Path.v "file.txt")
   = "file.txt") ```
*)
val basename: t -> string

(**
   Returns the directory portion of a path.

   Similar to [`parent`] but returns the path itself if no parent.

   ## Examples

   ```ocaml let file = Path.v "/home/user/file.txt" in assert (Path.dirname
   file = Path.v "/home/user");

   let dir = Path.v "/home/user/" in assert (Path.dirname dir = Path.v
   "/home");

   let root = Path.v "/" in assert (Path.dirname root = Path.v "/") ```
*)
val dirname: t -> t

(**
   Returns the file extension, if any.

   The extension is the part after the final `.` in the basename.

   ## Examples

   ```ocaml assert (Path.extension (Path.v "file.txt") = Some "txt"); assert
   (Path.extension (Path.v "archive.tar.gz") = Some "gz"); assert
   (Path.extension (Path.v "README") = None); assert (Path.extension (Path.v
   ".gitignore") = None); assert (Path.extension (Path.v "file.") = Some "")
   ```
*)
val extension: t -> string option

(**
   Removes the file extension, if present.

   ## Examples

   ```ocaml let path = Path.v "/dir/file.txt" in assert (Path.remove_extension
   path = Path.v "/dir/file");

   let no_ext = Path.v "/dir/README" in assert (Path.remove_extension no_ext =
   no_ext);

   (* Only removes last extension *) let archive = Path.v "file.tar.gz" in
   assert (Path.remove_extension archive = Path.v "file.tar") ```
*)
val remove_extension: t -> t

(**
   Adds an extension to the file.

   ## Examples

   ```ocaml
   let path = Path.v "file" in
   assert (Path.add_extension path ~ext:"txt" = Path.v "file.txt");

   (* Adds existing extension *)
   let doc = Path.v "document.doc"
   in assert (Path.add_extension doc ~ext:"pdf" = Path.v "document.doc.pdf");
   ```
*)
val add_extension: t -> ext:string -> t

(**
   Replaces a file's extension.

   ## Example

   ```ocaml
   (* Replace existing extension *)
   let doc = Path.v "document.doc"
   in assert (Path.add_extension doc ~ext:"pdf" = Path.v "document.pdf");
   ```
*)
val replace_extension: t -> ext:string -> t

(**
   Returns `true` if the path is absolute.

   Absolute paths start from the root of the filesystem.

   ## Examples

   ```ocaml assert (Path.is_absolute (Path.v "/home/user")); assert
   (Path.is_absolute (Path.v "/")); assert (not (Path.is_absolute (Path.v
   "relative/path"))); assert (not (Path.is_absolute (Path.v "./file")));

   (* Platform-specific on Windows *) (* Path.is_absolute (Path.v
   "C:\\Windows") = true on Windows *) ```
*)
val is_absolute: t -> bool

(**
   Returns `true` if the path is relative.

   Relative paths are interpreted from the current directory.

   ## Examples

   ```ocaml assert (Path.is_relative (Path.v "file.txt")); assert
   (Path.is_relative (Path.v "./subdir")); assert (Path.is_relative (Path.v
   "../parent")); assert (not (Path.is_relative (Path.v "/absolute"))) ```
*)
val is_relative: t -> bool

(**
   Splits a path into its components.

   Returns a list of path segments. Does not normalize the path.

   ## Examples

   ```ocaml let parts = Path.components (Path.v "a/b/c") in (* parts =
   [Path.v "a"; Path.v "b"; Path.v "c"] *)

   let abs = Path.components (Path.v "/usr/local/bin") in (* abs =
   [Path.v "/"; Path.v "usr"; Path.v "local"; Path.v "bin"] *)

   (* Special components are preserved *) let with_dots = Path.components
   (Path.v "a/./b/../c") in (* with_dots =
   [Path.v "a"; Path.v "."; Path.v "b"; Path.v ".."; Path.v "c"] *) ```
*)
val components: t -> t list

(**
   Normalizes a path by resolving `.` and `..` components.

   Does not access the filesystem or resolve symbolic links.

   ## Examples

   ```ocaml let path = Path.v "/home/user/../admin/./config" in assert
   (Path.normalize path = Path.v "/home/admin/config");

   let relative = Path.v "./a/b/../c/." in assert (Path.normalize relative =
   Path.v "a/c");

   (* Can't go above root *) let above_root = Path.v "/../.." in assert
   (Path.normalize above_root = Path.v "/") ```
*)
val normalize: t -> t

(**
   Checks if the path exists on the filesystem.

   ## Examples

   ```ocaml if Path.exists (Path.v "config.json") then println "Config file
   found" else println "No config file, using defaults"

   (* Check before operations *) let safe_remove path = if Path.exists path
   then Fs.remove_file path else Ok () ```

   ## Note

   Subject to TOCTOU race conditions. The file may be created or deleted
   between this check and subsequent operations.
*)
val exists: t -> bool

(**
   Returns `true` if the path exists and is a directory.

   ## Examples

   ```ocaml let path = Path.v "/home" in assert (Path.is_directory path);

   let file = Path.v "/etc/passwd" in assert (not (Path.is_directory file));

   (* Returns false if path doesn't exist *) let missing = Path.v
   "/does/not/exist" in assert (not (Path.is_directory missing)) ```
*)
val is_directory: t -> bool

(**
   Returns `true` if the path exists and is a regular file.

   ## Examples

   ```ocaml let file = Path.v "/etc/passwd" in assert (Path.is_file file);

   let dir = Path.v "/home" in assert (not (Path.is_file dir));

   (* Process only files *) let process_if_file path = if Path.is_file path
   then process_file path else println "Not a file: %s" (Path.to_string path)
   ```
*)
val is_file: t -> bool

(**
   Compares two paths for equality.

   Comparison is byte-for-byte. Paths are not normalized before comparison.

   ## Examples

   ```ocaml let p1 = Path.v "/home/user" in let p2 = Path.v "/home/user" in
   assert (Path.equal p1 p2);

   (* Different representations are not equal *) let p3 = Path.v "/home/./user"
   in assert (not (Path.equal p1 p3));

   (* Normalize first for semantic equality *) assert (Path.equal
   (Path.normalize p1) (Path.normalize p3)) ```
*)
val equal: t -> t -> bool

(**
   Orders two paths using their normalized representation.

   This keeps ordering consistent with semantic path equality.
*)
val compare: t -> t -> Order.t

(**
   Removes a prefix from a path if it matches.

   Returns the remaining path after removing the prefix.

   ## Examples

   ```ocaml let path = Path.v "/home/user/documents/file.txt" in let prefix =
   Path.v "/home/user" in

   match Path.strip_prefix path ~prefix with | Ok rel -> (* rel = Path.v
   "documents/file.txt" *) println "Relative: %s" (Path.to_string rel) | Error
   _ -> println "Not a prefix"

   (* Make paths relative to a base directory *) let make_relative ~base path =
   Path.strip_prefix path ~prefix:base |> Result.unwrap_or ~default:path ```
*)
val strip_prefix: t -> prefix:t -> (t, error) Result.t
