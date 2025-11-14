(** Example 5: Retraction - Removing facts while maintaining history *)

open Std
open Poneglyph

let () =
  Log.info "=== Example 5: Retraction ===";

  let graph = create () in

  let file_uri = Uri.of_string "example:file:config.json" in
  let valid_attr = Uri.of_string "example:valid" in
  let error_attr = Uri.of_string "example:error_message" in

  (* Initially mark file as invalid *)
  Log.info "Stating initial facts...";
  let initial_facts =
    [
      Fact.make ~entity:file_uri ~attribute:valid_attr
        ~value:(Fact.Bool false) ~stated_at:(Datetime.now ()) ~tx_id:0;
      Fact.make ~entity:file_uri ~attribute:error_attr
        ~value:(Fact.String "Syntax error on line 42")
        ~stated_at:(Datetime.now ()) ~tx_id:0;
    ]
  in

  let _ = state graph initial_facts in

  Log.info "";
  Log.info "Initial state:";
  (match get graph ~entity:file_uri ~attr:valid_attr with
  | Some (Fact.Bool false) -> Log.info "  File is INVALID"
  | Some (Fact.Bool true) -> Log.info "  File is VALID"
  | _ -> ());

  (match get graph ~entity:file_uri ~attr:error_attr with
  | Some (Fact.String msg) -> Log.info ("  Error: " ^ msg)
  | _ -> ());

  (* Get all facts to find the error fact URI *)
  let all_facts = get_all_facts graph ~entity:file_uri in
  let error_fact =
    List.find
      (fun f -> Uri.equal f.Fact.attribute error_attr)
      all_facts
  in

  (* User fixes the file - retract the error *)
  Log.info "";
  Log.info "File was fixed! Retracting error message...";
  retract graph ~fact_uri:error_fact.fact_uri;

  (* State new valid status *)
  let update_facts =
    [
      Fact.make ~entity:file_uri ~attribute:valid_attr
        ~value:(Fact.Bool true) ~stated_at:(Datetime.now ()) ~tx_id:0;
    ]
  in
  let _ = state graph update_facts in

  Log.info "";
  Log.info "Updated state:";
  (match get graph ~entity:file_uri ~attr:valid_attr with
  | Some (Fact.Bool true) -> Log.info "  File is now VALID ✓"
  | Some (Fact.Bool false) -> Log.info "  File is still INVALID"
  | _ -> ());

  (match get graph ~entity:file_uri ~attr:error_attr with
  | Some (Fact.String msg) -> Log.info ("  Error: " ^ msg)
  | None -> Log.info "  No error message (retracted)");

  (* Show history - including retracted facts *)
  Log.info "";
  Log.info "Complete history (including retracted):";
  let all_facts_history = get_all_facts graph ~entity:file_uri in
  List.iter
    (fun fact ->
      let status = if fact.Fact.retracted then "[RETRACTED]" else "[CURRENT]" in
      Log.info ("  " ^ status ^ " " ^ Uri.to_string fact.Fact.attribute ^ " = " ^
        Fact.value_to_string fact.Fact.value))
    all_facts_history;

  (* Show only current facts *)
  Log.info "";
  Log.info "Current facts only:";
  let current = get_current_facts graph ~entity:file_uri in
  List.iter
    (fun fact ->
      Log.info ("  " ^ Uri.to_string fact.Fact.attribute ^ " = " ^
        Fact.value_to_string fact.Fact.value))
    current;

  Log.info "=== Example 5 Complete ==="
