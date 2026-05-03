(**
   File permission bits.

   Unix-style file permissions with owner, group, and other read/write/execute
   bits.

   ## Examples

   Creating permissions:

   ```ocaml open Std.Fs

   (* Common permission modes *) let perms = Permissions.read_write in (*
   rw-r--r-- / 0644 *) let perms = Permissions.executable in (* rwxr-xr-x /
   0755 *) let perms = Permissions.private_read_write in (* rw------- / 0600 *)

   (* From Unix mode *) let perms = Permissions.of_mode 0o755 in
   Permissions.to_mode perms (* 0o755 *) ```

   Checking permissions:

   ```ocaml if Permissions.user_write perms then Log.info "Owner can write"

   if Permissions.readonly perms then Log.info "No write permissions anywhere"
   ```

   Modifying permissions:

   ```ocaml (* Make readonly *) let readonly = Permissions.set_readonly perms
   true in

   (* Warning: set_readonly false makes world-writable on Unix! *) let writable
   = Permissions.set_readonly perms false ```

   ## Permission Bits

   Unix permissions use 9 bits organized as: `rwxrwxrwx`
   - First 3 bits: owner (user) permissions
   - Middle 3 bits: group permissions
   - Last 3 bits: other (world) permissions

   Each triple is: read (4), write (2), execute (1)
*)

(** Unix permission bits for owner, group, and others. *)
type t

(**
   Creates permissions from Unix mode bits (octal).

   ## Examples

   ```ocaml Permissions.of_mode 0o644 (* rw-r--r-- *) Permissions.of_mode 0o755
   (* rwxr-xr-x *) Permissions.of_mode 0o600 (* rw------- *) ```
*)
val of_mode: int -> t

(**
   Converts to Unix mode bits.

   ## Examples

   ```ocaml let perms = Permissions.read_write in Permissions.to_mode perms (*
   0o644 *) ```
*)
val to_mode: t -> int

(**
   Returns [true] if no write bits are set (owner, group, or others).

   ## Examples

   ```ocaml let perms = Permissions.of_mode 0o444 in (* r--r--r-- *)
   Permissions.readonly perms (* true *)

   let perms = Permissions.of_mode 0o644 in (* rw-r--r-- *)
   Permissions.readonly perms (* false - owner can write *) ```

   ## Note

   This only checks permission bits. It doesn't consider:
   - ACLs (Access Control Lists)
   - Actual user/group ownership
   - SELinux/AppArmor policies
*)
val readonly: t -> bool

(**
   Sets or clears write permissions for owner, group, and others.

   ## Examples

   ```ocaml let perms = Permissions.of_mode 0o755 in let readonly =
   Permissions.set_readonly perms true in Permissions.to_mode readonly (* 0o555
   \- r-xr-xr-x *) ```

   ## Warning

   `set_readonly false` makes the file world-writable on Unix! This is rarely
   what you want. Consider setting specific bits instead.
*)
val set_readonly: t -> bool -> t

(** Checks if owner has read permission. *)
val user_read: t -> bool

(** Checks if owner has write permission. *)
val user_write: t -> bool

(** Checks if owner has execute permission. *)
val user_execute: t -> bool

(** Checks if group has read permission. *)
val group_read: t -> bool

(** Checks if group has write permission. *)
val group_write: t -> bool

(** Checks if group has execute permission. *)
val group_execute: t -> bool

(** Checks if others have read permission. *)
val other_read: t -> bool

(** Checks if others have write permission. *)
val other_write: t -> bool

(** Checks if others have execute permission. *)
val other_execute: t -> bool

(**
   `rw-r--r--` (0644) - Owner read/write, group/others read-only.

   Common for data files that need to be shared but not modified by others.
*)
val read_write: t

(**
   `rwxr-xr-x` (0755) - Owner read/write/execute, group/others read/execute.

   Common for executable files and directories.
*)
val executable: t

(**
   `rw-------` (0600) - Owner read/write only, no access for others.

   Common for private data files like SSH keys or credentials.
*)
val private_read_write: t

(**
   `rwx------` (0700) - Owner read/write/execute only, no access for others.

   Common for private executables or personal directories.
*)
val private_executable: t
