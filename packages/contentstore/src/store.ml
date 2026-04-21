open Std

type policy = Policy.t

module Namespace = Namespace

let panic = Kernel.SystemError.panic

type source_path_error = Store_error.source_path_error =
  | Source_missing
  | Source_not_file
  | Source_not_directory

type io_detail = Store_error.io_detail =
  | Fs of Fs.error
  | File of Fs.File.error

type error = Store_error.t =
  | Missing of { path: Path.t }
  | Invalid_source_path of { path: Path.t; reason: source_path_error }
  | Io of { op: string; path: Path.t; related_path: Path.t option; detail: io_detail }

type t = {
  root: Path.t;
  ns: Namespace.t;
  policy: policy;
}

let ( let* ) value fn = Result.and_then value ~fn

let create = fun ~root ~ns ~policy -> { root; ns; policy }

let error_message = Store_error.error_message

let root = fun store -> store.root

let namespace = fun store -> store.ns

let policy = fun store -> store.policy

let empty_hash_hex = Crypto.Sha256.hash_string "" |> Crypto.Digest.hex

let checked_hash_hex = fun fn hash ->
  let hex = Crypto.Digest.hex hash in
  if String.equal hex empty_hash_hex then
    panic ("Contentstore.Store." ^ fn ^ " received the SHA-256 empty digest")
  else
    hex

let hash_dir_of = fun store hash ->
  let _ = checked_hash_hex "hash_dir_of" hash in
  Layout.tree_dir store.root hash

let exists = fun store hash -> Fs.exists (hash_dir_of store hash) |> Result.unwrap_or ~default:false

let open_path = fun path ->
  let* path_exists = Fs.exists path
  |> Result.map_err
    ~fn:(fun detail -> Io { op = "exists"; path; related_path = None; detail = Fs detail }) in
  if not path_exists then
    Error (Missing { path })
  else
    Fs.File.open_read path
    |> Result.map_err
      ~fn:(fun detail -> Io { op = "open_read"; path; related_path = None; detail = File detail })

let temp_path = fun store ~scope ~seed -> Layout.temp_path store.root ~scope ~seed

let seed_of_hash = fun hash -> checked_hash_hex "seed_of_hash" hash

let seed_of_key = fun key -> Crypto.hash_string key |> Crypto.Digest.hex

let save_object = fun store ~hash ~content ->
  let _ = checked_hash_hex "save_object" hash in
  let destination = Layout.object_path store.root ~ns:store.ns ~hash in
  let temp = temp_path store ~scope:Layout.Immutable ~seed:(seed_of_hash hash) in
  Atomic.write_object_if_absent ~temp ~dst:destination ~content

let save_file = fun store ~hash ~source ->
  let _ = checked_hash_hex "save_file" hash in
  let destination = Layout.object_path store.root ~ns:store.ns ~hash in
  let temp = temp_path store ~scope:Layout.Immutable ~seed:(seed_of_hash hash) in
  Atomic.copy_file_if_absent ~source ~temp ~dst:destination

let open_object = fun store ~hash ->
  let _ = checked_hash_hex "open_object" hash in
  open_path (Layout.object_path store.root ~ns:store.ns ~hash)

let save_named_object = fun store ~key ~content ->
  let destination = Layout.named_object_path store.root ~ns:store.ns ~key in
  let temp = temp_path store ~scope:Layout.Mutable ~seed:(seed_of_key key) in
  Atomic.replace_with_object ~temp ~dst:destination ~content

let save_named_file = fun store ~key ~source ->
  let destination = Layout.named_object_path store.root ~ns:store.ns ~key in
  let temp = temp_path store ~scope:Layout.Mutable ~seed:(seed_of_key key) in
  Atomic.replace_with_file ~source ~temp ~dst:destination

let open_named_object = fun store ~key ->
  open_path (Layout.named_object_path store.root ~ns:store.ns ~key)

let commit_dir = fun store ~hash ~source_dir ->
  let destination = hash_dir_of store hash in
  let temp = temp_path store ~scope:Layout.Trees ~seed:(seed_of_hash hash) in
  Atomic.commit_dir_if_absent ~source_dir ~staging:temp ~dst:destination
