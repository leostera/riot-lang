open Std

type temp_scope =
  | Trees
  | Immutable
  | Mutable

let temp_counter = Sync.Atomic.make 0

let path_of_segments = fun root segments ->
  let rec loop path segments =
    match segments with
    | [] -> path
    | segment :: rest -> loop Path.(path / Path.v segment) rest
  in
  loop root segments

let hash_hex = fun hash -> Crypto.Digest.hex hash

let shard_of_hex = fun hex -> String.sub hex ~offset:0 ~len:2

let tmp_root = fun root -> Path.(root / Path.v "tmp")

let scope_dir = fun scope ->
  match scope with
  | Trees -> Path.v "trees"
  | Immutable -> Path.v "immutable"
  | Mutable -> Path.v "mutable"

let scope_segment = fun scope ->
  match scope with
  | Trees -> "trees"
  | Immutable -> "immutable"
  | Mutable -> "mutable"

let tree_segments = fun hash ->
  let hex = hash_hex hash in [ "trees"; shard_of_hex hex; hex ]

let object_segments = fun ~ns ~hash ->
  let hex = hash_hex hash in ("objects" :: Namespace.parts ns) @ [ shard_of_hex hex; hex ]

let named_object_segments = fun ~ns ~key ->
  let key_hash = Crypto.hash_string key |> Crypto.Digest.hex in ("named" :: Namespace.parts ns) @ [ shard_of_hex key_hash; key_hash ]

let tree_dir = fun root hash -> path_of_segments root (tree_segments hash)

let object_path = fun root ~ns ~hash -> path_of_segments root (object_segments ~ns ~hash)

let named_object_path = fun root ~ns ~key -> path_of_segments root (named_object_segments ~ns ~key)

let temp_segments = fun ~scope ~seed ->
  let pid = Process.id () |> Int32.to_string in
  let nanos = Time.SystemTime.duration_since_epoch () |> Time.Duration.to_nanos |> Int64.to_string in
  let counter = Sync.Atomic.fetch_and_add temp_counter 1 |> Int.to_string in [ "tmp"; scope_segment scope; seed ^ "." ^ pid ^ "." ^ nanos ^ "." ^ counter ^ ".tmp" ]

let temp_path = fun root ~scope ~seed -> path_of_segments root (temp_segments ~scope ~seed)
