open Std

let name = "eta-reduction"

let rec check_tree tree =
  match tree with
  | Syn.TokenTree.Token _ -> []
  | Syn.TokenTree.Tree (_, trees) -> check_trees trees

and check_trees trees =
  let rec aux acc = function
    | [] -> List.rev acc
    | tree :: rest -> (
        let issues = check_tree tree in
        match is_eta_reducible tree with
        | Some issue -> aux (issue :: (issues @ acc)) rest
        | None -> aux (issues @ acc) rest)
  in
  aux [] trees

and is_eta_reducible tree =
  match tree with
  | Syn.TokenTree.Tree (Syn.Token.Paren, inner) -> (
      match inner with
      | Syn.TokenTree.Token (Syn.Token.Keyword Syn.Token.Fun)
        :: Syn.TokenTree.Token (Syn.Token.Ident param)
        :: Syn.TokenTree.Token Syn.Token.Arrow :: rest -> (
          match extract_simple_application rest param with
          | Some func_name ->
              Some
                {
                  Lint_rule.rule_name = name;
                  severity = Lint_rule.Info;
                  message =
                    format "Eta-reduction possible: (fun %s -> %s %s)" param
                      func_name param;
                  suggestion = Some (format "Use %s directly" func_name);
                  fix = None;
                }
          | None -> None)
      | _ -> None)
  | _ -> None

and extract_simple_application trees param =
  match trees with
  | [ Syn.TokenTree.Token (Syn.Token.Ident func);
      Syn.TokenTree.Token (Syn.Token.Ident arg) ]
    when arg = param ->
      Some func
  | [ Syn.TokenTree.Token (Syn.Token.Ident func);
      Syn.TokenTree.Tree (Syn.Token.Paren, inner) ] -> (
      match inner with
      | [ Syn.TokenTree.Token (Syn.Token.Ident arg) ] when arg = param ->
          Some func
      | _ -> None)
  | _ -> None

let check trees = check_trees trees
