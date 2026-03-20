open Std

type t = { rules : Rule.t list }
type result = {
  tree : Rule.green_tree;
  diagnostics : Diagnostic.t list;
  parse_diagnostics : Syn.Diagnostic.t list;
}

let make ~rules () = { rules }

let builtin_rule_factories () =
  [
    ("no-stdlib", Rules.No_stdlib.make);
    ("naming-convention", Rules.Naming_convention.make);
  ]

let run pipeline ?filename source =
  let parse_result =
    match filename with
    | Some filename -> Syn.parse ~filename source
    | None -> Syn.parse_implementation source
  in
  (* Skip linting if there are parse errors *)
  let lint_diagnostics =
    if List.length parse_result.diagnostics > 0 then
      []
    else
      let red_tree = Syn.Ceibo.Red.new_root parse_result.tree in
      let file_path = Option.unwrap_or ~default:"<stdin>" filename |> Path.v in
      let ctx = Rule.{ file_path } in
      pipeline.rules
      |> List.map (fun rule -> Rule.run rule ctx red_tree)
      |> List.concat
  in
  {
    tree = parse_result.tree;
    diagnostics = lint_diagnostics;
    parse_diagnostics = parse_result.diagnostics;
  }

let default_rules () = 
  [ 
    Rules.No_stdlib.make ();
    (* Rules.Naming_convention.make (); *)
  ]

let default_rule_ids () =
  default_rules () |> List.map Rule.id

let rules_by_id ids =
  let factories = builtin_rule_factories () in
  ids
  |> List.filter_map (fun id ->
         match List.find_opt (fun (rule_id, _) -> String.equal rule_id id) factories with
         | Some (_rule_id, factory) -> Some (factory ())
         | None ->
             Log.warn ("Unknown tusk-fix rule '" ^ id ^ "', ignoring");
             None)
let default () = make ~rules:(default_rules ()) ()
