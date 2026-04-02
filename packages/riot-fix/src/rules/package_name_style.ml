open Std
module Toml = Data.Toml

let rule_id = "package-name-style"

let rule_description = "Package names should start with a letter, use kebab-case, and avoid trailing separators"

let rule_explain = {|
Package names show up everywhere: in workspace manifests, dependency declarations,
build output, and command-line messages.

When a package name mixes styles like `my_pkg`, starts with a digit, or leaves a trailing
`-` or `_`, the name stops looking like a stable identifier and starts looking like an
accident that leaked out of one local directory choice.

This rule keeps package names on one simple convention:

- start with a letter
- use lowercase letters, digits, and `-`
- do not end with `-` or `_`

That gives the workspace one predictable package naming story instead of several
near-misses.
|}

let split_path = fun path -> Path.to_string path |> String.split_on_char '/'

let rec find_package_root_components = fun prefix ->
  function
  | []
  | [ _ ] -> None
  | "packages" :: package_dir :: _ -> Some (List.rev_append prefix [ "packages"; package_dir ])
  | segment :: rest -> find_package_root_components (segment :: prefix) rest

let package_root_for_file = fun path ->
  split_path path
  |> find_package_root_components []
  |> Option.map (fun components -> Path.v (String.concat "/" components))

let package_toml_for_file = fun path ->
  package_root_for_file path
  |> Option.map (fun package_root -> Path.(package_root / Path.v "riot.toml"))

let get_table value =
  match value with
  | Toml.Table table -> Some table
  | _ -> None

let get_string value =
  match value with
  | Toml.String value -> Some value
  | _ -> None

let package_name = fun path ->
  match package_toml_for_file path with
  | None -> None
  | Some package_toml -> (
      match Fs.read package_toml with
      | Error _ -> None
      | Ok content -> (
          match Toml.parse content with
          | Error _ ->
              None
          | Ok (Toml.Table fields) -> (
              match List.assoc_opt "package" fields with
              | Some package_value -> (
                  match get_table package_value with
                  | Some package_fields -> (
                      match List.assoc_opt "name" package_fields with
                      | Some name_value -> get_string name_value
                      | None -> None
                    )
                  | None -> None
                )
              | None -> None
            )
          | Ok _ ->
              None
        )
    )

let starts_with_letter = fun name ->
  String.length name > 0 && match String.get name 0 with
  | 'a' .. 'z'
  | 'A' .. 'Z' -> true
  | _ -> false

let is_kebab_case_char ch =
  match ch with
  | 'a' .. 'z'
  | '0' .. '9'
  | '-' -> true
  | _ -> false

let is_kebab_case = fun name ->
  String.length name > 0 && String.for_all is_kebab_case_char name && String.exists
    (
      function
      | 'A' .. 'Z' -> true
      | _ -> false
    )
    name |> not

let has_trailing_separator = fun name ->
  String.ends_with ~suffix:"-" name || String.ends_with ~suffix:"_" name

let make_diagnostic = fun ~suggestion path ->
  Diagnostic.make
    ~severity:Warning
    ~kind:(Diagnostic.Known { rule_id; message = rule_description })
    ~span:(Syn.Ceibo.Span.make ~start:0 ~end_:0)
    ~suggestion:((suggestion ^ " In `" ^ Path.to_string path ^ "`."))
    ()

let diagnostics_for_name = fun path name ->
  let starts_with_letter_diagnostic =
    if starts_with_letter name then
      []
    else
      [
        make_diagnostic ~suggestion:(("Rename package `" ^ name ^ "` so it starts with a letter")) path;
      ]
  in
  let kebab_case_diagnostic =
    if is_kebab_case name then
      []
    else
      [
        make_diagnostic
          ~suggestion:(("Rename package `" ^ name ^ "` to use lowercase letters, digits, and `-` only"))
          path;
      ]
  in
  let trailing_separator_diagnostic =
    if has_trailing_separator name then
      [
        make_diagnostic
          ~suggestion:(("Rename package `" ^ name ^ "` so it does not end with `-` or `_`"))
          path;
      ]
    else
      []
  in
  starts_with_letter_diagnostic @ kebab_case_diagnostic @ trailing_separator_diagnostic

let check_tree = fun (ctx: Rule.context) _red_root ->
  let path = Path.v ctx.file_path in
  match package_name path with
  | Some name -> diagnostics_for_name path name
  | None -> []

let make = fun () ->
  Rule.make ~id:rule_id ~description:rule_description ~explain:rule_explain ~run:check_tree ()
