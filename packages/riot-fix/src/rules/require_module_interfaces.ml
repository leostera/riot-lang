open Std

let rule_id = Rule_id.from_string "require-module-interfaces"

let rule_description = "Library source modules should usually have a matching .mli file"

let rule_explain =
  {|
An `.mli` file turns the boundary of a module into a deliberate choice instead of an
accident of whatever happens to be defined in the implementation today.

That buys a few useful things immediately: internal helpers stay private by default,
reviewers can see API changes directly, and future refactors have room to rearrange the
implementation without exposing every intermediate helper.

This rule is biased toward explicit library boundaries. Even a small module often gets
easier to maintain once its public surface is written down separately.
|}

let is_source_module = fun path ->
  String.ends_with ~suffix:".ml" path
  && String.contains path "/src/"
  && not (String.ends_with ~suffix:".mli" path)
  && not (String.ends_with ~suffix:"/main.ml" path)

let interface_path_for = fun path -> Path.(add_extension (remove_extension path) ~ext:"mli")

let make_diagnostic = fun path ->
  Diagnostic.make
    ~severity:Warning
    ~kind:(Diagnostic.Known { rule_id; message = rule_description })
    ~span:(Syn.Span.make ~start:0 ~end_:0)
    ~suggestion:("Add a matching interface file at `"
    ^ Path.to_string (interface_path_for path)
    ^ "`.")
    ()

let check_tree = fun (ctx: Rule.context) _root ->
  let path = Path.v ctx.file_path in
  if not (is_source_module ctx.file_path) then
    []
  else
    let interface_path = interface_path_for path in
    match Fs.exists interface_path with
    | Ok true -> []
    | Ok false
    | Error _ -> [ make_diagnostic path ]

let make = fun () ->
  Rule.make
    ~id:rule_id
    ~description:rule_description
    ~explain:rule_explain
    ~run:check_tree
    ()
