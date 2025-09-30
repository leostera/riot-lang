type t = Unix.stats

let of_unix stats = stats
let to_unix t = t

let file_type t =
  match t.Unix.st_kind with
  | Unix.S_REG -> `Regular
  | Unix.S_DIR -> `Directory
  | Unix.S_LNK -> `Symlink
  | Unix.S_BLK -> `Block
  | Unix.S_CHR -> `Character
  | Unix.S_FIFO -> `Fifo
  | Unix.S_SOCK -> `Socket

let is_file t = t.Unix.st_kind = Unix.S_REG
let is_dir t = t.Unix.st_kind = Unix.S_DIR
let is_symlink t = t.Unix.st_kind = Unix.S_LNK

let len t = t.Unix.st_size

let permissions t = Permissions.of_mode t.Unix.st_perm

let accessed t = t.Unix.st_atime
let modified t = t.Unix.st_mtime

(* Birth time is not in Unix.stats, platform-specific *)
let created _t = None

let mode t = t.Unix.st_perm
let uid t = t.Unix.st_uid
let gid t = t.Unix.st_gid
let nlink t = t.Unix.st_nlink
let ino t = t.Unix.st_ino
let dev t = t.Unix.st_dev
let rdev t = t.Unix.st_rdev