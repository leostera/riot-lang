open Std
open Tusk_model

type t = {
  workspace_root : Path.t;
  toolchain : Tusk_toolchain.t;
  workspace : Workspace.t;
  db_path : Path.t;
  watch : bool;
}

let create ~workspace_root ~toolchain ~workspace ~db_path ?(watch = true) () =
  { workspace_root; toolchain; workspace; db_path; watch }

let workspace_root t = t.workspace_root
let toolchain t = t.toolchain
let workspace t = t.workspace
let db_path t = t.db_path
let watch t = t.watch
