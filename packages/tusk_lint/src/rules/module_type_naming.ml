open Std

let name = "module-type-naming"

let is_all_caps str =
  String.length str > 0
  && String.for_all
       (fun c -> (c >= 'A' && c <= 'Z') || c = '_')
       str

let suggest_name str =
  let parts = String.split_on_char '_' str in
  let capitalize_first s =
    if String.length s = 0 then s
    else
      String.mapi (fun i c -> if i = 0 then c else Char.lowercase_ascii c) s
  in
  let pascal_case = String.concat "" (List.map capitalize_first parts) in
  if String.ends_with ~suffix:"_INTF" str || String.ends_with ~suffix:"_INTERFACE" str then
    pascal_case ^ "Intf"
  else
    pascal_case

let rec skip_whitespace = function
  | Syn.TokenTree.Token Syn.Token.Whitespace :: rest -> skip_whitespace rest
  | other -> other

let rec check_tree tree =
  match tree with
  | Syn.TokenTree.Token tok ->
      Printf.eprintf "DEBUG: Token in tree\n";
      []
  | Syn.TokenTree.Tree (_, trees) ->
      Printf.eprintf "DEBUG: Tree with %d children\n" (List.length trees);
      check_trees trees

and check_trees trees =
  Printf.eprintf "DEBUG: check_trees called with %d trees\n" (List.length trees);
  let rec aux acc = function
    | [] -> List.rev acc
    | tree :: rest -> (
        let issues = check_tree tree in
        match is_module_type_all_caps (tree :: rest) with
        | Some issue ->
            Printf.eprintf "DEBUG: Found issue!\n";
            aux (issue :: (issues @ acc)) rest
        | None -> aux (issues @ acc) rest)
  in
  aux [] trees

and is_module_type_all_caps trees =
  match skip_whitespace trees with
  | Syn.TokenTree.Token (Syn.Token.Keyword Syn.Token.Module) :: rest1 ->
      Printf.eprintf "DEBUG: Found Module keyword\n";
      (match skip_whitespace rest1 with
      | Syn.TokenTree.Token (Syn.Token.Keyword Syn.Token.Type) :: rest2 ->
          Printf.eprintf "DEBUG: Found Type keyword\n";
          (match skip_whitespace rest2 with
          | Syn.TokenTree.Token (Syn.Token.Ident type_name) :: _ ->
              Printf.eprintf "DEBUG: Found ident: %s, is_all_caps=%b\n" type_name (is_all_caps type_name);
              if is_all_caps type_name then
                let suggested = suggest_name type_name in
                Some
                  {
                    Lint_rule.rule_name = name;
                    severity = Lint_rule.Warning;
                    message =
                      format "Module type name '%s' is all caps, prefer PascalCase"
                        type_name;
                    suggestion =
                      Some (format "Use '%s' or '%sIntf' instead" suggested suggested);
                    fix = None;
                  }
              else None
          | _ ->
              Printf.eprintf "DEBUG: No ident after Type\n";
              None)
      | _ ->
          Printf.eprintf "DEBUG: No Type keyword after Module\n";
          None)
  | _ :: rest -> is_module_type_all_caps rest
  | [] -> None

let check trees = check_trees trees
