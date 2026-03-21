open Std
open Std.Collections
open Std.Iter
open Tusk_model
open Tusk_store

type t = {
  package : Package.t;
  artifact : Artifact.t;
  depset : t list;
  hash : Crypto.hash;
}

let library_cmxa (dep : t) : Path.t =
  List.find_opt
    (fun path ->
      match Path.extension path with Some ".cmxa" -> true | _ -> false)
    dep.artifact.files
  |> Option.expect
       ~msg:
         ("No .cmxa file found in artifact for package " ^
            dep.package.name)

let transitive_closure deps =
  let seen = HashSet.create () in
  let ordered = vec [] in
  let rec collect dep =
    if HashSet.contains seen dep.package.name then ()
    else (
      let _ = HashSet.insert seen dep.package.name in
      List.iter collect dep.depset;
      Vector.push ordered dep)
  in
  List.iter collect deps;
  Vector.into_iter ordered |> Iterator.to_list
