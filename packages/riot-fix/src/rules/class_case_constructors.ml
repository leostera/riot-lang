open Std

let rule_id = Rule_id.of_string "class-case-constructors"

let rule_description = "Rule disabled while Syn Ast migration is in progress"

let rule_explain = {|
This rule is temporarily disabled while riot-fix migrates from the removed Syn CST
API to Syn Ast views. The rule id remains loadable so catalogs and provider wiring
continue to work during the parser cleanup.
|}

let check_tree = fun _ctx _root -> []

let make = fun () -> Rule.make ~id:rule_id ~description:rule_description ~explain:rule_explain ~run:check_tree ()
