open Std

let find_lock_package = fun ~(package: Tusk_model.Package.t) ~(lockfile: Tusk_model.Lockfile.t) ->
  List.find_opt
    (fun (lock_package: Tusk_model.Lockfile.package) ->
      String.equal lock_package.id.name package.name)
    lockfile.packages

let rec resolve_packages_loop = fun acc packages lockfile ->
  match packages with
  | [] -> Ok (List.rev acc)
  | package :: rest -> (
      match find_lock_package ~package ~lockfile with
      | None ->
          Error ("lockfile is missing package '" ^ package.name ^ "'")
      | Some lock_package -> (
          match Tusk_model.Package.resolve ~package ~lock_package with
          | Ok resolved ->
              resolve_packages_loop (resolved :: acc) rest lockfile
          | Error _ as err -> err
        )
    )

let resolve_packages = fun ~packages ~lockfile ->
  resolve_packages_loop [] packages lockfile
