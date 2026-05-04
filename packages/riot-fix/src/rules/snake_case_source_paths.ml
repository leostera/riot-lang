open Std

let rule_id = Rule_id.from_string "snake-case-source-paths"

let rule_description = "Source filenames and subdirectories should use snake_case"

let rule_explain =
  {|
Prefer `snake_case` for source filenames and directories inside `src/`. Module
discovery and package browsing are easier when filesystem names follow one
boring convention instead of mixing `JsonHelpers`, `sessionStore`, and
`session_store`.

Use lowercase letters, digits, and underscores for source path segments.
|}

let is_snake_case = fun name ->
  let len = String.length name in
  let rec loop index =
    if index >= len then
      true
    else
      let ch = String.get_unchecked name ~at:index in
      ((ch >= 'a' && ch <= 'z') || (ch >= '0' && ch <= '9') || Char.equal ch '_') && loop
        (index + 1)
  in
  len > 0 && (
    let first = String.get_unchecked name ~at:0 in
    first >= 'a' && first <= 'z'
  ) && loop 1

let split_source_path = fun path ->
  let rec after_src = fun __tmp1 ->
    match __tmp1 with
    | [] -> []
    | "src" :: rest -> rest
    | _ :: rest -> after_src rest
  in
  Path.to_string path
  |> String.split ~by:"/"
  |> after_src

let path_segments = fun path ->
  match List.reverse (split_source_path path) with
  | [] -> []
  | file_name :: rev_dirs ->
      let stem =
        Path.(v file_name
        |> remove_extension
        |> basename)
      in
      List.reverse rev_dirs @ [ stem ]

let make_diagnostic = fun name ->
  Diagnostic.make
    ~severity:Warning
    ~kind:(Diagnostic.Known { rule_id; message = rule_description })
    ~span:(Syn.Span.make ~start:0 ~end_:0)
    ~suggestion:("Rename `" ^ name ^ "` to use lowercase letters, digits, and underscores only.")
    ()

let check_tree = fun (ctx: Rule.context) _root ->
  let path = Path.v ctx.file_path in
  let path_str = Path.to_string path in
  if
    not
      ((String.ends_with ~suffix:".ml" path_str || String.ends_with ~suffix:".mli" path_str)
      && String.contains path_str "/src/")
  then
    []
  else
    path_segments path
    |> List.filter_map
      ~fn:(fun name ->
        if is_snake_case name then
          None
        else
          Some (make_diagnostic name))

let make = fun () ->
  Rule.make
    ~id:rule_id
    ~description:rule_description
    ~explain:rule_explain
    ~run:check_tree
    ()
