open Std

let rule_id = "naming-convention"
let rule_name = "Naming Convention"

let rule_description =
  "Enforces naming conventions: prefer 'from_X' over 'of_X' for conversion \
   functions"

let check_tree _ctx red_root =
  (* For now, just return empty - we'll implement this after fixing traversal *)
  []

let make () =
  Rule.make ~id:rule_id ~name:rule_name ~description:rule_description
    ~run:check_tree ()
