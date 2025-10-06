open Std

let name = "redundant-path-conversion"

let rec check_tree tree =
  match tree with
  | Syn.TokenTree.Token _ -> []
  | Syn.TokenTree.Tree (_, trees) -> check_trees trees

and check_trees trees =
  let rec aux acc = function
    | [] -> List.rev acc
    | tree :: rest -> (
        let issues = check_tree tree in
        match is_redundant_path_conversion (tree :: rest) with
        | Some issue -> aux (issue :: (issues @ acc)) rest
        | None -> aux (issues @ acc) rest)
  in
  aux [] trees

and is_redundant_path_conversion trees =
  match trees with
  | Syn.TokenTree.Token (Syn.Token.Ident "Path")
    :: Syn.TokenTree.Token Syn.Token.Dot
    :: Syn.TokenTree.Token (Syn.Token.Ident "of_string")
    :: Syn.TokenTree.Tree (Syn.Token.Paren, inner) :: rest -> (
      match find_path_to_string inner with
      | Some var_name ->
          Some
            {
              Lint_rule.rule_name = name;
              severity = Lint_rule.Warning;
              message =
                format "Redundant Path.of_string (Path.to_string %s)" var_name;
              suggestion = Some (format "Use %s directly" var_name);
              fix = None;
            }
      | None -> None)
  | _ :: rest -> is_redundant_path_conversion rest
  | [] -> None

and find_path_to_string trees =
  match trees with
  | Syn.TokenTree.Token (Syn.Token.Ident "Path")
    :: Syn.TokenTree.Token Syn.Token.Dot
    :: Syn.TokenTree.Token (Syn.Token.Ident "to_string")
    :: Syn.TokenTree.Tree (Syn.Token.Paren, inner) :: _ -> (
      match inner with
      | [ Syn.TokenTree.Token (Syn.Token.Ident var) ] -> Some var
      | _ -> None)
  | _ :: rest -> find_path_to_string rest
  | [] -> None

let check trees = check_trees trees
