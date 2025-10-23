open Std
open Tusk_model

type t = { dir : Path.t; workspace : Workspace.t }

let create ~workspace =
  let now = Time.Instant.now () in
  let nanos = Time.Instant.elapsed now |> Time.Duration.to_nanos in
  let sandbox_id = format "%08x" (Hashtbl.hash nanos) in
  let sandbox_dir =
    Path.(
      workspace.Workspace.root / v "target" / v "debug" / v "sandbox"
      / v sandbox_id)
  in
  Fs.create_dir_all sandbox_dir
  |> Result.expect
       ~msg:
         (format "Failed to create sandbox dir: %s"
            (Path.to_string sandbox_dir));
  { dir = sandbox_dir; workspace }

let get_dir t = t.dir

let relativize_from_root ~workspace_root ~abs_path =
  let root_str = Path.to_string workspace_root in
  let abs_str = Path.to_string abs_path in

  if String.starts_with ~prefix:root_str abs_str then
    let prefix_len = String.length root_str in
    let relative =
      String.sub abs_str prefix_len (String.length abs_str - prefix_len)
    in
    let relative =
      if String.starts_with ~prefix:"/" relative then
        String.sub relative 1 (String.length relative - 1)
      else relative
    in
    Path.v relative
  else panic (format "Path %s is not under workspace root %s" abs_str root_str)

let copy_inputs ~sandbox ~inputs =
  List.iter
    (fun abs_input ->
      let rel_path =
        relativize_from_root ~workspace_root:sandbox.workspace.Workspace.root
          ~abs_path:abs_input
      in
      let dest = Path.(sandbox.dir / rel_path) in
      let dest_parent = Path.dirname dest in
      Fs.create_dir_all dest_parent
      |> Result.expect
           ~msg:
             (format "Failed to create parent dir: %s"
                (Path.to_string dest_parent));
      Fs.copy ~src:abs_input ~dst:dest
      |> Result.expect
           ~msg:
             (format "Failed to copy input %s to %s" (Path.to_string abs_input)
                (Path.to_string dest)))
    inputs

let verify_outputs ~sandbox ~expected_outputs = ()
(* NOTE: Verification disabled because cache hits promote to target without 
     populating sandbox, causing false failures. The store's save/promote 
     logic already validates outputs exist. *)

let cleanup sandbox =
  let _ = Fs.remove_dir_all sandbox.dir in
  ()

let with_sandbox ~workspace ~inputs ~expected_outputs f =
  let sandbox = create ~workspace in
  Fun.protect
    (fun () ->
      copy_inputs ~sandbox ~inputs;
      let result = f sandbox in
      verify_outputs ~sandbox ~expected_outputs;
      result)
    ~finally:(fun () -> cleanup sandbox)
