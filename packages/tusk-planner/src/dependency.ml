open Std
open Std.Collections
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
         (format "No .cmxa file found in artifact for package %s"
            dep.package.name)
