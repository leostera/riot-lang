open Std
open Std.Collections

(* Helper to skip whitespace and comments *)
let skip_trivia elements =
  Array.to_list elements
  |> List.filter (function
    | Ceibo.Green.Token tok ->
        tok.kind != Parser.Syntax_kind.WHITESPACE
        && tok.kind != Parser.Syntax_kind.COMMENT
    | _ -> true)

(* Get text from a token *)
let get_token_text = function
  | Ceibo.Green.Token tok -> tok.text
  | Ceibo.Green.Node _ -> ""

(* Convert a term node to Term.t *)
let rec convert_term = function
  | Ceibo.Green.Node node when node.kind = Parser.Syntax_kind.STRING_LITERAL ->
      let text =
        node.children |> Array.to_list |> List.map get_token_text
        |> String.concat ""
      in
      (* Remove quotes (both double and single) *)
      let unquoted =
        if String.length text >= 2 then
          let first = String.get text 0 in
          let last = String.get text (String.length text - 1) in
          if (first = '"' && last = '"') || (first = '\'' && last = '\'')
          then String.sub text 1 (String.length text - 2)
          else text
        else text
      in
      Ok (Term.Const (Value.String unquoted))
      
  | Ceibo.Green.Node node when node.kind = Parser.Syntax_kind.INT_LITERAL ->
      let text =
        node.children |> Array.to_list |> List.map get_token_text
        |> String.concat ""
      in
      (match int_of_string_opt text with
      | Some i -> Ok (Term.Const (Value.Int i))
      | None -> Error ("Invalid integer: " ^ text))
      
  | Ceibo.Green.Node node when node.kind = Parser.Syntax_kind.VARIABLE ->
      let name =
        node.children |> Array.to_list |> List.map get_token_text
        |> String.concat ""
      in
      Ok (Term.Var name)
      
  | Ceibo.Green.Node node when node.kind = Parser.Syntax_kind.WILDCARD ->
      Ok Term.Wildcard
      
  | other ->
      Error ("Unknown term: " ^ get_token_text other)

(* Convert an ATOM node to atom *)
let rec convert_atom node =
  let children = skip_trivia node.Ceibo.Green.children in
  match children with
  (* Nested ATOM node - unwrap it *)
  | [Ceibo.Green.Node inner] when inner.kind = Parser.Syntax_kind.ATOM ->
      convert_atom inner
      
  | Ceibo.Green.Token pred_tok :: Ceibo.Green.Token _ :: args_and_paren ->
      let predicate = pred_tok.Ceibo.Green.text in
      
      (* Extract argument nodes *)
      let arg_nodes =
        List.filter_map
          (function
            | Ceibo.Green.Node n
              when n.Ceibo.Green.kind = Parser.Syntax_kind.STRING_LITERAL
                   || n.kind = Parser.Syntax_kind.INT_LITERAL
                   || n.kind = Parser.Syntax_kind.VARIABLE
                   || n.kind = Parser.Syntax_kind.WILDCARD ->
                Some (Ceibo.Green.Node n)
            | _ -> None)
          args_and_paren
      in
      
      (* Convert all args *)
      let rec convert_args acc = function
        | [] -> Ok (List.rev acc)
        | node :: rest ->
            (match convert_term node with
            | Ok term -> convert_args (term :: acc) rest
            | Error e -> Error e)
      in
      
      (match convert_args [] arg_nodes with
      | Ok args -> Ok (Ast.atom ~predicate ~args)
      | Error e -> Error e)
      
  | children ->
      let debug_info = 
        children |> List.map (function
          | Ceibo.Green.Token tok -> "Token(" ^ tok.text ^ ")"
          | Ceibo.Green.Node n -> "Node(" ^ Parser.Syntax_kind.to_string n.kind ^ ")")
        |> String.concat ", "
      in
      Error ("Invalid atom structure: [" ^ debug_info ^ "]")

(* Convert a clause (body element) *)
let convert_clause = function
  | Ceibo.Green.Node ({ kind = Parser.Syntax_kind.ATOM; _ } as node) ->
      (match convert_atom node with
      | Ok atom -> Ok (Ast.Atom atom)
      | Error e -> Error e)
      
  | Ceibo.Green.Node ({ kind = Parser.Syntax_kind.NEGATED_ATOM; _ } as node) ->
      let inner_children = skip_trivia node.children in
      let atom_opt =
        List.find_map
          (function
            | Ceibo.Green.Node ({ kind = Parser.Syntax_kind.ATOM; _ } as n) ->
                Some n
            | _ -> None)
          inner_children
      in
      (match atom_opt with
      | Some atom_node ->
          (match convert_atom atom_node with
          | Ok atom -> Ok (Ast.Negated atom)
          | Error e -> Error e)
      | None -> Error "Negated atom missing inner atom")
      
  | Ceibo.Green.Node ({ kind = Parser.Syntax_kind.BUILTIN; _ } as node) ->
      (* Extract operator and arguments *)
      let inner_children = skip_trivia node.children in
      
      (* Check for nested builtin *)
      let rec find_innermost_builtin = function
        | Ceibo.Green.Node ({ kind = Parser.Syntax_kind.BUILTIN; _ } as n) ->
            let nested = skip_trivia n.children in
            (match List.find_map
              (function
                | Ceibo.Green.Node ({ kind = Parser.Syntax_kind.BUILTIN; _ } as inner) ->
                    Some inner
                | _ -> None)
              nested
            with
            | Some inner -> find_innermost_builtin (Ceibo.Green.Node inner)
            | None -> n)
        | _ -> node
      in
      
      let builtin_node = find_innermost_builtin (Ceibo.Green.Node node) in
      let children = skip_trivia builtin_node.children in
      
      let op_opt =
        List.find_map
          (function
            | Ceibo.Green.Token { kind = Parser.Syntax_kind.GT; _ } -> Some ">"
            | Ceibo.Green.Token { kind = Parser.Syntax_kind.LT; _ } -> Some "<"
            | Ceibo.Green.Token { kind = Parser.Syntax_kind.GTEQ; _ } -> Some ">="
            | Ceibo.Green.Token { kind = Parser.Syntax_kind.LTEQ; _ } -> Some "<="
            | Ceibo.Green.Token { kind = Parser.Syntax_kind.EQ; _ } -> Some "="
            | Ceibo.Green.Token { kind = Parser.Syntax_kind.NOTEQ; _ } -> Some "!="
            | _ -> None)
          children
      in
      
      let arg_nodes =
        List.filter_map
          (function
            | Ceibo.Green.Node n
              when n.Ceibo.Green.kind = Parser.Syntax_kind.STRING_LITERAL
                   || n.kind = Parser.Syntax_kind.INT_LITERAL
                   || n.kind = Parser.Syntax_kind.VARIABLE
                   || n.kind = Parser.Syntax_kind.WILDCARD ->
                Some (Ceibo.Green.Node n)
            | _ -> None)
          children
      in
      
      (* Convert all args *)
      let rec convert_args acc = function
        | [] -> Ok (List.rev acc)
        | node :: rest ->
            (match convert_term node with
            | Ok term -> convert_args (term :: acc) rest
            | Error e -> Error e)
      in
      
      (match op_opt, convert_args [] arg_nodes with
      | Some op, Ok args -> Ok (Ast.Builtin (op, args))
      | None, _ -> Error "Builtin missing operator"
      | _, Error e -> Error e)
      
  | _ -> Error "Unknown clause type"

(* Convert a RULE node *)
let convert_rule node =
  let children = skip_trivia node.Ceibo.Green.children in
  
  (* Find head (first ATOM) *)
  let head_opt =
    List.find_map
      (function
        | Ceibo.Green.Node ({ kind = Parser.Syntax_kind.ATOM; _ } as n) ->
            Some n
        | _ -> None)
      children
  in
  
  (* Find all body clauses (after COLON_DASH) *)
  let rec find_body_clauses found_dash acc = function
    | [] -> List.rev acc
    | Ceibo.Green.Token { kind = Parser.Syntax_kind.COLON_DASH; _ } :: rest ->
        find_body_clauses true acc rest
    | elem :: rest when found_dash ->
        (match elem with
        | Ceibo.Green.Node ({ kind = Parser.Syntax_kind.ATOM; _ } as n)
        | Ceibo.Green.Node ({ kind = Parser.Syntax_kind.NEGATED_ATOM; _ } as n)
        | Ceibo.Green.Node ({ kind = Parser.Syntax_kind.BUILTIN; _ } as n) ->
            find_body_clauses true (Ceibo.Green.Node n :: acc) rest
        | _ -> find_body_clauses true acc rest)
    | _ :: rest -> find_body_clauses found_dash acc rest
  in
  
  let body_nodes = find_body_clauses false [] children in
  
  match head_opt with
  | None -> Error "Rule missing head"
  | Some head_node ->
      (match convert_atom head_node with
      | Error e -> Error e
      | Ok head ->
          (* Convert all body clauses *)
          let rec convert_body_clauses acc = function
            | [] -> Ok (List.rev acc)
            | node :: rest ->
                (match convert_clause node with
                | Ok clause -> convert_body_clauses (clause :: acc) rest
                | Error e -> Error e)
          in
          (match convert_body_clauses [] body_nodes with
          | Ok body -> Ok (Ast.rule ~head ~body)
          | Error e -> Error e))

(* Convert a FACT node *)
let convert_fact node =
  let children = skip_trivia node.Ceibo.Green.children in
  match children with
  | Ceibo.Green.Node ({ kind = Parser.Syntax_kind.ATOM; _ } as atom) :: _ ->
      convert_atom atom
  | _ -> Error "Fact missing atom"

(* Convert a PROGRAM node *)
let program_of_cst node =
  if node.Ceibo.Green.kind != Parser.Syntax_kind.PROGRAM then
    Error "Expected PROGRAM node"
  else
    let children = skip_trivia node.children in
    
    let rec process_items facts rules = function
      | [] -> Ok (Ast.program ~facts:(List.rev facts) ~rules:(List.rev rules))
      | Ceibo.Green.Node ({ kind = Parser.Syntax_kind.FACT; _ } as n) :: rest ->
          (match convert_fact n with
          | Ok atom -> process_items (atom :: facts) rules rest
          | Error e -> Error e)
      | Ceibo.Green.Node ({ kind = Parser.Syntax_kind.RULE; _ } as n) :: rest ->
          (match convert_rule n with
          | Ok rule -> process_items facts (rule :: rules) rest
          | Error e -> Error e)
      | _ :: rest -> process_items facts rules rest
    in
    
    process_items [] [] children

(* Convert a query - single or multi-clause *)
let query_of_cst node =
  match node.Ceibo.Green.kind with
  | Parser.Syntax_kind.ATOM -> 
      convert_atom node |> Result.map (fun a -> Ast.Single a)
  | Parser.Syntax_kind.PROGRAM ->
      (* Could be single or multi-clause query *)
      let children = skip_trivia node.children in
      
      (* Extract all clauses from the query *)
      let rec extract_clauses acc = function
        | [] -> Ok (List.rev acc)
        | (Ceibo.Green.Node ({ kind = Parser.Syntax_kind.ATOM; _ }) as clause) :: rest
        | (Ceibo.Green.Node ({ kind = Parser.Syntax_kind.NEGATED_ATOM; _ }) as clause) :: rest
        | (Ceibo.Green.Node ({ kind = Parser.Syntax_kind.BUILTIN; _ }) as clause) :: rest ->
            (match convert_clause clause with
            | Ok c -> extract_clauses (c :: acc) rest
            | Error e -> Error e)
        | _ :: rest -> extract_clauses acc rest
      in
      
      (match extract_clauses [] children with
      | Ok [] -> Error "Query must contain at least one clause"
      | Ok [Ast.Atom a] -> Ok (Ast.Single a)
      | Ok clauses -> Ok (Ast.Multi clauses)
      | Error e -> Error e)
  | _ -> Error "Expected ATOM or PROGRAM for query"

