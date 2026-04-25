open Std
open Std.Sync
open Std.Sync.Cell

type package = string

type version = Version.t

type version_ranges = version Ranges.t

type dependency_list = (package * version_ranges) list

type dependencies =
  | Available of dependency_list
  | Unavailable of string

type 'error t = {
  choose_version: package -> version_ranges -> (version option, 'error) result;
  count_versions: package -> version_ranges -> (int, 'error) result;
  get_dependencies: package -> version -> (dependencies, 'error) result;
}

type offline_package = {
  versions_desc: version list;
  deps_by_version: (version, dependency_list) Collections.HashMap.t;
}

type offline = { packages: (package, offline_package) Collections.HashMap.t }

let create_offline = fun () -> { packages = Collections.HashMap.create () }

let version_compare = Version.compare

let rec insert_version_desc = fun versions ver ->
  match versions with
  | [] -> [ ver ]
  | current :: rest -> (
    match version_compare ver current with
    | Order.EQ -> versions
    | Order.GT -> ver :: versions
    | Order.LT -> current :: insert_version_desc rest ver
  )

let create_offline_package = fun ver deps ->
  let deps_by_version = Collections.HashMap.create () in
  let _ = Collections.HashMap.insert deps_by_version ~key:ver ~value:deps in { versions_desc = [ ver ]; deps_by_version }

let add_package = fun provider pkg ver deps ->
  match Collections.HashMap.get provider.packages ~key:pkg with
  | None ->
      let offline_pkg = create_offline_package ver deps in
      let _ = Collections.HashMap.insert provider.packages ~key:pkg ~value:offline_pkg in ()
  | Some offline_pkg ->
      let _ = Collections.HashMap.insert offline_pkg.deps_by_version ~key:ver ~value:deps in
      let updated = { offline_pkg with versions_desc = insert_version_desc offline_pkg.versions_desc ver } in
      let _ = Collections.HashMap.insert provider.packages ~key:pkg ~value:updated in ()

let to_provider: offline -> string t = fun offline ->
  let choose_version pkg ranges =
    match Collections.HashMap.get offline.packages ~key:pkg with
    | None -> Ok None
    | Some offline_pkg -> Ok (List.find offline_pkg.versions_desc ~fn:(
      fun version -> Ranges.contains ~compare_v:version_compare ranges version
    ))
  in
  let count_versions pkg ranges =
    match Collections.HashMap.get offline.packages ~key:pkg with
    | None -> Ok 0
    | Some offline_pkg -> Ok (List.fold_left offline_pkg.versions_desc ~init:0 ~fn:(
      fun count version ->
        if Ranges.contains ~compare_v:version_compare ranges version then
          count + 1
        else count
    ))
  in
  let get_dependencies pkg ver =
    match Collections.HashMap.get offline.packages ~key:pkg with
    | None -> Ok (Unavailable ("Package '" ^ pkg ^ "' not found"))
    | Some offline_pkg -> (
      match Collections.HashMap.get offline_pkg.deps_by_version ~key:ver with
      | None -> Ok (Unavailable ("Version " ^ Version.to_string ver ^ " not found for package '" ^ pkg ^ "'"))
      | Some deps -> Ok (Available deps)
    )
  in
  { choose_version; count_versions; get_dependencies }
