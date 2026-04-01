open Std

let rule_id = "snake-case-source-paths"

let rule_description = "Source filenames and subdirectories should use snake_case"

let rule_explain = {|
Prefer `snake_case` for source filenames and directories inside `src/`.

The filesystem is part of the public shape of a package. Once source paths drift into
mixed styles like `JsonHelpers/sessionStore.ml`, module discovery becomes less
predictable and directory listings start looking inconsistent for no real gain.

Keeping paths in `snake_case` gives the workspace one boring, reliable convention.
That makes it easier to guess where a module should live, easier to rename things
consistently, and easier to scan packages without mentally translating styles.

The goal here is not aesthetics. It is predictability across the whole tree.
|}

let is_snake_case = fun name ->
  let len = String.length name in
  let is_lower = function
    | 'a' .. 'z' -> true
    | _ -> false
  in
  let is_digit = function
    | '0' .. '9' -> true
    | _ -> false
  in
  let rec loop idx =
    if idx >= len then
      true
    else
      let ch = String.get name idx in
      (is_lower ch || is_digit ch || Char.equal ch '_') && loop (idx + 1)
  in
  len > 0 && is_lower (String.get name 0) && loop 1

let split_source_path = fun path ->
  let path_str = Path.to_string path in
  let rec after_src = function
    | []
    | [] -> []
    | "src" :: rest -> rest
    | _ :: rest -> after_src rest
  in
  String.split_on_char '/' path_str |> after_src

let path_segments = fun path ->
  match List.rev (split_source_path path) with
  | [] -> []
  | file_name :: rev_dirs ->
      let stem = Path.(v file_name |> remove_extension |> basename) in
      List.rev rev_dirs @ [ stem ]

let make_diagnostic = fun name ->
  Diagnostic.make
    ~severity:Warning
    ~kind:(Diagnostic.Known { rule_id; message = rule_description })
    ~span:(Syn.Ceibo.Span.make ~start:0 ~end_:0)
    ~suggestion:(("Rename `" ^ name ^ "` to use lowercase letters, digits, and underscores only."))
    ()

let check_tree = fun (ctx: Rule.context) _red_root ->
  let path = Path.v ctx.file_path in
  let path_str = Path.to_string path in
  if
    not
      ((String.ends_with ~suffix:".ml" path_str || String.ends_with ~suffix:".mli" path_str)
      && String.contains path_str "/src/")
  then
    []
  else
    path_segments path |> List.filter_map
      (fun name ->
        if is_snake_case name then
          None
        else
          Some (make_diagnostic name))

let make = fun () ->
  Rule.make ~id:rule_id ~description:rule_description ~explain:rule_explain ~run:check_tree ()
