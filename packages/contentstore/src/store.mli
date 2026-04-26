open Std

(** Store retention policy handle. *)
type policy = Policy.t

module Namespace = Namespace

(**
   Generic content-addressable store bound to one namespace under one
   filesystem root.
*)
type t
type source_path_error =
  | Source_missing
  | Source_not_file
  | Source_not_directory
type io_detail =
  | Fs of Fs.error
  | File of Fs.File.error
(** Store operation error. *)
type error =
  | Missing of {
      path: Path.t;
    }
  | Invalid_source_path of {
      path: Path.t;
      reason: source_path_error;
    }
  | Io of {
      op: string;
      path: Path.t;
      related_path: Path.t option;
      detail: io_detail;
    }
val error_message: error -> string

(** Create one logical store handle rooted at [root] scoped to [ns]. *)
val create: root:Path.t -> ns:Namespace.t -> policy:policy -> t

val root: t -> Path.t

val namespace: t -> Namespace.t

val policy: t -> policy

(** Return the stable hash-addressed tree directory for [hash]. *)
val hash_dir_of: t -> Crypto.hash -> Path.t

(** Check whether a hash-addressed directory currently exists. *)
val exists: t -> Crypto.hash -> bool

(**
   Atomically commit [source_dir] into the hash-addressed tree location for [hash].

   Successful commits consume [source_dir]. If another writer already committed
   the same [hash], this still succeeds and [source_dir] is treated as
   disposable.
*)
val commit_dir: t -> hash:Crypto.hash -> source_dir:Path.t -> (unit, error) result

(** Save one immutable object keyed by [hash]. *)
val save_object: t -> hash:Crypto.hash -> content:string -> (unit, error) result

(** Import one immutable object from an existing file keyed by [hash]. *)
val save_file: t -> hash:Crypto.hash -> source:Path.t -> (unit, error) result

(** Open one immutable object keyed by [hash] for reading. *)
val open_object: t -> hash:Crypto.hash -> (Fs.File.t, error) result

(** Save one mutable named object keyed by [key]. *)
val save_named_object: t -> key:string -> content:string -> (unit, error) result

(** Import one mutable named object from an existing file keyed by [key]. *)
val save_named_file: t -> key:string -> source:Path.t -> (unit, error) result

(** Open one mutable named object keyed by [key] for reading. *)
val open_named_object: t -> key:string -> (Fs.File.t, error) result
