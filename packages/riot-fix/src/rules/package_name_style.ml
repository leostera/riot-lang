open Std
open Std.Collections

module Toml = Data.Toml
module H = Rule_helpers

let rule_id = Rule_id.from_string "package-name-style"

let rule_description =
  "Package names should start with a letter, use kebab-case, and avoid trailing separators"

let rule_explain =
  {|
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

let split_path = fun path ->
  Path.to_string path
  |> String.split ~by:"/"

let rec find_package_root_components = fun prefix components ->
  match components with
  | []
  | [ _ ] -> None
  | "packages" :: package_dir :: _ ->
      Some (List.append (List.reverse prefix) [ "packages"; package_dir ])
  | segment :: rest -> find_package_root_components (segment :: prefix) rest

let package_root_for_file = fun path ->
  split_path path
  |> find_package_root_components []
  |> Option.map ~fn:(fun components -> Path.v (String.concat "/" components))

let package_toml_for_file = fun path ->
  package_root_for_file path
  |> Option.map ~fn:(fun package_root -> Path.(package_root / Path.v "riot.toml"))

let get_table = fun value ->
  match value with
  | Toml.Table table -> Some table
  | _ -> None

let get_string = fun value ->
  match value with
  | Toml.String value -> Some value
  | _ -> None

let find_field = fun fields name ->
  List.find fields ~fn:(fun (field_name, _) -> String.equal field_name name)
  |> Option.map ~fn:(fun (_, value) -> value)

let package_name = fun path ->
  match package_toml_for_file path with
  | None -> None
  | Some package_toml -> (
      match Fs.read package_toml with
      | Error _ -> None
      | Ok content -> (
          match Toml.parse content with
          | Error _ -> None
          | Ok (Toml.Table fields) -> (
              match find_field fields "package"
              |> Option.and_then ~fn:get_table with
              | Some package_fields ->
                  find_field package_fields "name"
                  |> Option.and_then ~fn:get_string
              | None -> None
            )
          | Ok _ -> None
        )
    )

let starts_with_letter = fun name ->
  String.length name > 0 && match String.get_unchecked name ~at:0 with
  | 'a' .. 'z'
  | 'A' .. 'Z' -> true
  | _ -> false

let is_kebab_case_char = fun ch ->
  match ch with
  | 'a' .. 'z'
  | '0' .. '9'
  | '-' -> true
  | _ -> false

let is_kebab_case = fun name -> String.length name > 0 && String.for_all name ~fn:is_kebab_case_char

let has_trailing_separator = fun name ->
  String.ends_with ~suffix:"-" name || String.ends_with ~suffix:"_" name

let make_diagnostic = fun ~suggestion path ->
  H.diagnostic
    ~rule_id
    ~message:rule_description
    ~span:(Syn.Span.make ~start:0 ~end_:0)
    ~suggestion:(suggestion ^ " In `" ^ Path.to_string path ^ "`.")
    ()

let diagnostics_for_name = fun path name ->
  let diagnostics = Vector.with_capacity ~size:3 in
  if not (starts_with_letter name) then
    H.push_diagnostic
      diagnostics
      (make_diagnostic ~suggestion:("Rename package `" ^ name ^ "` so it starts with a letter") path);
  if not (is_kebab_case name) then
    H.push_diagnostic
      diagnostics
      (make_diagnostic
        ~suggestion:("Rename package `" ^ name ^ "` to use lowercase letters, digits, and `-` only")
        path);
  if has_trailing_separator name then
    H.push_diagnostic
      diagnostics
      (make_diagnostic
        ~suggestion:("Rename package `" ^ name ^ "` so it does not end with `-` or `_`")
        path);
  H.vector_to_list diagnostics

let check_tree = fun (ctx: Rule.context) _root ->
  let path = Path.v ctx.file_path in
  match package_name path with
  | Some name -> diagnostics_for_name path name
  | None -> []

let make = fun () ->
  Rule.make
    ~id:rule_id
    ~description:rule_description
    ~explain:rule_explain
    ~run:check_tree
    ()
