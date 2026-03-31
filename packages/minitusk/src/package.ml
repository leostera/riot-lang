open Stdlib

type binary = {
  name: string;
  path: string;
}

type t = {
  name: string;
  path: string;
  deps: string list;
  binaries: binary list;
  uses_stdlib: bool;
  uses_unix: bool;
  uses_dynlink: bool;
  cc_flags: string list;
  ld_flags: string list;
}

let binaries = fun t -> t.binaries

let uses_stdlib = fun t -> t.uses_stdlib

let uses_unix = fun t -> t.uses_unix

let uses_dynlink = fun t -> t.uses_dynlink

let cc_flags = fun t -> t.cc_flags

let ld_flags = fun t -> t.ld_flags

(* Detect the current OS *)

let detect_os = fun () ->
    let ic = Unix.open_process_in "uname -s" in
    let uname = input_line ic in
    let _ = Unix.close_process_in ic in
    match uname with
    | "Darwin" -> "macos"
    | "Linux" -> "linux"
    | _ -> "unknown"

(* Parse a string list from TOML array *)

let parse_string_list = fun toml_array ->
    match toml_array with
    | Toml.Array items ->
        List.filter_map
          (
            function
            | Toml.String s -> Some s
            | _ -> None
          )
          items
    | _ -> []

let read = fun path ->
    let toml_path = Filename.concat path "tusk.toml" in
    if Sys.file_exists toml_path then
      (
        (* Read the file content *)
        let ic = open_in toml_path in
        let content = really_input_string ic (in_channel_length ic) in
        close_in ic;
        (* Parse TOML *)
        match Toml.parse content with
        | Error err ->
            Printf.printf "Error parsing %s: %s\n" toml_path (Toml.error_to_string err);
            {
              name = Filename.basename path;
              path;
              deps = [];
              binaries = [];
              uses_stdlib = false;
              uses_unix = false;
              uses_dynlink = false;
              cc_flags = [];
              ld_flags = []
            }
        | Ok (Toml.Table items) ->
            (* Get package name *)
            let name =
              match Toml.find "package" items with
              | Some (Toml.Table pkg_items) -> (
                  match Toml.find "name" pkg_items with
                  | Some (Toml.String n) -> n
                  | _ -> Filename.basename path
                )
              | _ -> Filename.basename path
            in
            (* Get binaries from [[bin]] array *)
            let binaries =
              match Toml.find "bin" items with
              | Some (Toml.Array bin_tables) ->
                  List.filter_map
                    (fun bin_value ->
                      match bin_value with
                      | Toml.Table bin_items -> (
                          let bin_name =
                            match Toml.find "name" bin_items with
                            | Some (Toml.String n) -> Some n
                            | _ -> None
                          in
                          let bin_path =
                            match Toml.find "path" bin_items with
                            | Some (Toml.String p) -> Some p
                            | _ -> None
                          in
                          match (bin_name, bin_path) with
                          | Some n, Some p -> Some {name = n; path = Filename.concat path p}
                          | _ -> None
                        )
                      | _ -> None)
                    bin_tables
              | _ -> []
            in
            (* Get dependencies from [dependencies] table *)
            let uses_stdlib, uses_unix, uses_dynlink =
              match Toml.find "dependencies" items with
              | Some (Toml.Table dep_items) ->
                  let has_stdlib = Toml.find "stdlib" dep_items != None in
                  let has_unix = Toml.find "unix" dep_items != None in
                  let has_dynlink = Toml.find "dynlink" dep_items != None in
                  (has_stdlib, has_unix, has_dynlink)
              | _ -> (false, false, false)
            in
            (* Get target-specific flags based on OS *)
            let os = detect_os () in
            Printf.printf "  DEBUG: Detected OS: %s\n" os;
            let cc_flags, ld_flags =
              (* Look for [target] table first, then check for OS-specific subtable *)
              match Toml.find "target" items with
              | Some (Toml.Table target_items) -> (
                  Printf.printf "  DEBUG: Found [target] table\n";
                  match Toml.find os target_items with
                  | Some (Toml.Table os_items) ->
                      Printf.printf "  DEBUG: Found [target.%s] table\n" os;
                      let cc =
                        match Toml.find "cc_flags" os_items with
                        | Some arr ->
                            let flags = parse_string_list arr in
                            Printf.printf
                              "  DEBUG: Found %d cc_flags: %s\n"
                              (List.length flags)
                              (String.concat " " flags);
                            flags
                        | None ->
                            Printf.printf "  DEBUG: No cc_flags found\n";
                            []
                      in
                      let ld =
                        match Toml.find "ld_flags" os_items with
                        | Some arr ->
                            let flags = parse_string_list arr in
                            Printf.printf
                              "  DEBUG: Found %d ld_flags: %s\n"
                              (List.length flags)
                              (String.concat " " flags);
                            flags
                        | None -> []
                      in
                      (cc, ld)
                  | _ ->
                      Printf.printf "  DEBUG: No [target.%s] subtable found\n" os;
                      ([], [])
                )
              | _ ->
                  Printf.printf "  DEBUG: No [target] table found\n";
                  ([], [])
            in
            {
              name;
              path;
              deps = [];
              binaries;
              uses_stdlib;
              uses_unix;
              uses_dynlink;
              cc_flags;
              ld_flags
            }
        | _ ->
            {
              name = Filename.basename path;
              path;
              deps = [];
              binaries = [];
              uses_stdlib = false;
              uses_unix = false;
              uses_dynlink = false;
              cc_flags = [];
              ld_flags = []
            }
      )
    else
      {
        name = Filename.basename path;
        path;
        deps = [];
        binaries = [];
        uses_stdlib = false;
        uses_unix = false;
        uses_dynlink = false;
        cc_flags = [];
        ld_flags = []
      }
