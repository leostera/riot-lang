open Std
open Std.Sync
open Std.Sync.Cell

type package = string

type version = Version.t

type version_ranges = version Ranges.t

type dependency_list = (package * version_ranges) list

type dependencies =
  Available of dependency_list
  | Unavailable of string

type 'error t = {
  choose_version: package -> version_ranges -> (version option, 'error) result;
  get_dependencies: package -> version -> (dependencies, 'error) result;
}

type offline_entry = {
  version: version;
  deps: dependency_list;
}

type offline = {
  packages: (package, offline_entry list) Collections.HashMap.t;
}

let create_offline = fun () -> {packages = Collections.HashMap.create ()}

let add_package = fun provider pkg ver deps ->
  let entry = {version = ver; deps} in
  match Collections.HashMap.get provider.packages pkg with
  | None -> ignore (Collections.HashMap.insert provider.packages pkg [ entry ])
  | Some entries -> ignore (Collections.HashMap.insert provider.packages pkg (entry :: entries))

let version_compare = fun a b ->
  match Version.compare a b with
  | Lt -> (-1)
  | Eq -> 0
  | Gt -> 1

let to_provider : offline -> string t = fun offline ->
  let choose_version = fun pkg ranges ->
    match Collections.HashMap.get offline.packages pkg with
    | None -> Ok None
    | Some entries ->
        let matching =
          List.filter
          (fun entry -> Ranges.contains ~compare_v:version_compare ranges entry.version)
          entries
        in
        let sorted =
          List.sort (fun a b -> version_compare b.version a.version) matching
        in
        Ok (
          match sorted with
          | [] -> None
          | entry :: _ -> Some entry.version
        )
  in
  let get_dependencies = fun pkg ver ->
    match Collections.HashMap.get offline.packages pkg with
    | None -> Ok (Unavailable ("Package '" ^ pkg ^ "' not found"))
    | Some entries -> (
        match List.find_opt (fun entry -> version_compare entry.version ver = 0) entries with
        | None -> Ok (Unavailable ("Version "
        ^ Version.to_string ver
        ^ " not found for package '"
        ^ pkg
        ^ "'"))
        | Some entry -> Ok (Available entry.deps)
      )
  in
  {choose_version; get_dependencies}
