open Std

(** 
   Printer Design: gofmt/elixir-style Direct AST Formatting
   =========================================================
   
   This printer follows the approach used by gofmt and elixir format:
   - Walk the AST directly
   - Apply fixed formatting rules per node type
   - No intermediate document algebra (unlike Wadler's prettier printer)
   - Single-pass rendering
   
   Why this approach?
   ------------------
   1. Simple to implement and understand
   2. Predictable output (same input = same output)
   3. Fast (single pass, no optimization phase)
   4. Proven in production (gofmt has formatted billions of lines)
   
   Compared to alternatives:
   - Wadler's pretty printer: More flexible, but adds complexity we don't need
   - Knuth-Plass: Designed for prose, not code structure
   - Oppen/Strictly Pretty: Too complex for our needs
   
   Architecture:
   -------------
   1. Token-based printer (current): Handles spacing around operators, keywords, etc.
   2. Node-based printer (new): Handles multi-line structures (if/match/let)
   3. Context tracking: Mutable refs for state (labeled args, records, etc.)
   
   When to add multi-line formatting:
   - IF expressions: Always multi-line if it has else branch
   - MATCH expressions: Always multi-line
   - TYPE declarations: Multi-line for records/variants with 2+ fields/constructors
   - LET...IN: Multi-line when body is complex
*)

let indent_string config level =
  if config.Config.use_tabs then String.make level '\t'
  else String.make (level * config.Config.indent_size) ' '

let needs_space_before kind =
  match kind with
  | Syn.SyntaxKind.WHITESPACE | Syn.SyntaxKind.COMMENT
  | Syn.SyntaxKind.DOCSTRING ->
      false
  | _ -> true

let needs_space_after kind =
  match kind with
  | Syn.SyntaxKind.WHITESPACE | Syn.SyntaxKind.COMMENT
  | Syn.SyntaxKind.DOCSTRING ->
      false
  | _ -> true

let is_keyword text =
  match text with
  | "let" | "rec" | "in" | "if" | "then" | "else" | "match" | "with" | "fun"
  | "function" | "type" | "of" | "and" | "module" | "struct" | "sig" | "end"
  | "open" | "include" | "val" | "external" | "exception" | "when" | "as"
  | "while" | "for" | "to" | "downto" | "do" | "done" | "try" | "raise"
  | "begin" | "assert" | "lazy" | "mutable" | "private" | "constraint" ->
      true
  | _ -> false

let is_opening_delimiter text =
  match text with "(" | "[" | "{" | "[|" -> true | _ -> false

let is_closing_delimiter text =
  match text with ")" | "]" | "}" | "|]" -> true | _ -> false

let is_operator text =
  match text with
  | "=" | "+" | "-" | "*" | "/" | "+." | "-." | "*." | "/." | "&&" | "||" | "<"
  | ">" | "<=" | ">=" | "==" | "!=" | "<>" | "@" | "^" | "::" | "|>" | "@@"
  | ">>" | "<<" | ">>>" | "<|" | "->" | "=>" | ":=" ->
      true
  | _ -> false

let is_prefix_operator text =
  match text with "-" | "!" | "~" -> true | _ -> false

let is_punctuation text = match text with ";" | "," | "|" -> true | _ -> false
let needs_space_before_colon = true

(**
   Helper: Extract parts of IF expression
   
   Given IF_EXPR children array, split into:
   - condition (expression between 'if' and 'then')
   - then_branch (expression between 'then' and 'else')  
   - else_branch (expression after 'else')
   
   Returns: (cond_children, then_children, else_children_opt)
*)
let partition_if_expr children =
  let rec go i state cond_acc then_acc else_acc =
    if i >= Array.length children then
      (List.rev cond_acc, List.rev then_acc, 
       if else_acc = [] then None else Some (List.rev else_acc))
    else
      match children.(i) with
      | Syn.Ceibo.Red.Token tok ->
          let text = Syn.Ceibo.Red.SyntaxToken.text tok in
          let kind = Syn.Ceibo.Red.SyntaxToken.kind tok in
          (* Skip whitespace tokens *)
          if kind = Syn.SyntaxKind.WHITESPACE && 
             String.for_all (fun c -> c = ' ' || c = '\t' || c = '\n' || c = '\r') text
          then go (i+1) state cond_acc then_acc else_acc
          else if text = "if" then 
            go (i+1) `AfterIf cond_acc then_acc else_acc
          else if text = "then" then 
            go (i+1) `AfterThen cond_acc then_acc else_acc
          else if text = "else" then 
            go (i+1) `AfterElse cond_acc then_acc else_acc
          else
            (match state with
             | `Start | `AfterIf -> go (i+1) state (children.(i) :: cond_acc) then_acc else_acc
             | `AfterThen -> go (i+1) state cond_acc (children.(i) :: then_acc) else_acc
             | `AfterElse -> go (i+1) state cond_acc then_acc (children.(i) :: else_acc))
      | node ->
          (match state with
           | `Start | `AfterIf -> go (i+1) state (children.(i) :: cond_acc) then_acc else_acc
           | `AfterThen -> go (i+1) state cond_acc (children.(i) :: then_acc) else_acc
           | `AfterElse -> go (i+1) state cond_acc then_acc (children.(i) :: else_acc))
  in
  go 0 `Start [] [] []

(**
   Helper: Extract parts of MATCH expression
   
   Given MATCH_EXPR children, split into:
   - scrutinee (expression between 'match' and 'with')
   - cases (list of MATCH_CASE nodes)
*)
let partition_match_expr children =
  let rec go i state scrut_acc cases_acc =
    if i >= Array.length children then
      (List.rev scrut_acc, List.rev cases_acc)
    else
      match children.(i) with
      | Syn.Ceibo.Red.Token tok ->
          let text = Syn.Ceibo.Red.SyntaxToken.text tok in
          let kind = Syn.Ceibo.Red.SyntaxToken.kind tok in
          if kind = Syn.SyntaxKind.WHITESPACE &&
             String.for_all (fun c -> c = ' ' || c = '\t' || c = '\n' || c = '\r') text
          then go (i+1) state scrut_acc cases_acc
          else if text = "match" then
            go (i+1) `AfterMatch scrut_acc cases_acc
          else if text = "with" then
            go (i+1) `AfterWith scrut_acc cases_acc
          else
            (match state with
             | `Start | `AfterMatch -> go (i+1) state (children.(i) :: scrut_acc) cases_acc
             | `AfterWith -> go (i+1) state scrut_acc cases_acc)
      | Syn.Ceibo.Red.Node node ->
          let node_kind = Syn.Ceibo.Red.SyntaxNode.kind node in
          if node_kind = Syn.SyntaxKind.MATCH_CASE then
            go (i+1) state scrut_acc (children.(i) :: cases_acc)
          else
            (match state with
             | `Start | `AfterMatch -> go (i+1) state (children.(i) :: scrut_acc) cases_acc
             | `AfterWith -> go (i+1) state scrut_acc cases_acc)
  in
  go 0 `Start [] []

(**
   Helper: Extract parts of MATCH_CASE
   
   Returns: (pattern_children, body_children)
*)
let partition_match_case children =
  let rec go i state pattern_acc body_acc =
    if i >= Array.length children then
      (List.rev pattern_acc, List.rev body_acc)
    else
      match children.(i) with
      | Syn.Ceibo.Red.Token tok ->
          let text = Syn.Ceibo.Red.SyntaxToken.text tok in
          let kind = Syn.Ceibo.Red.SyntaxToken.kind tok in
          if kind = Syn.SyntaxKind.WHITESPACE &&
             String.for_all (fun c -> c = ' ' || c = '\t' || c = '\n' || c = '\r') text
          then go (i+1) state pattern_acc body_acc
          else if text = "|" then
            go (i+1) `AfterPipe pattern_acc body_acc
          else if text = "->" then
            go (i+1) `AfterArrow pattern_acc body_acc
          else
            (match state with
             | `Start | `AfterPipe -> go (i+1) state (children.(i) :: pattern_acc) body_acc
             | `AfterArrow -> go (i+1) state pattern_acc (children.(i) :: body_acc))
      | node ->
          (match state with
           | `Start | `AfterPipe -> go (i+1) state (children.(i) :: pattern_acc) body_acc
           | `AfterArrow -> go (i+1) state pattern_acc (children.(i) :: body_acc))
  in
  go 0 `Start [] []

let print_root ~config root =
  let buf = Buffer.create 1024 in
  let indent_level = ref 0 in
  let last_token_kind = ref None in
  let last_token_text = ref None in
  let prev_token_text = ref None in
  let in_labeled_arg = ref false in
  let in_indexing = ref false in
  let in_record = ref false in

  let add_indent () =
    Buffer.add_string buf (indent_string config !indent_level)
  in
  let add_newline () = Buffer.add_char buf '\n' in
  let add_space () = Buffer.add_char buf ' ' in

  let should_add_space_before current_text =
    match !last_token_text with
    | None -> false
    | Some last_text ->
        (* Track labeled/optional arg context *)
        if last_text = "~" || last_text = "?" then in_labeled_arg := true;
        
        (* Track record context *)
        if last_text = "{" then in_record := true
        else if current_text = "}" then in_record := false;
        
        (* No space around . for module paths and indexing *)
        if current_text = "." || last_text = "." then false
        (* No space after ` for poly variants *)
        else if last_text = "`" then false
        (* No space after ~ or ? for labeled/optional args *)
        else if last_text = "~" || last_text = "?" then false
        (* No space before/after : in labeled args *)
        else if !in_labeled_arg && (current_text = ":" || last_text = ":") then
          (if current_text <> ":" then in_labeled_arg := false; false)
        (* No space around = in record fields *)
        else if !in_record && (current_text = "=" || last_text = "=") then false
        (* No space after prefix operators when they come after = or other operators *)
        else if is_prefix_operator last_text then
          (match !prev_token_text with
           | Some prev when prev = "=" || is_operator prev || prev = "(" || prev = "[" || prev = "," -> false
           | _ -> true)
        (* Space after regular operators *)
        else if is_operator last_text then true
          (* No space in empty brackets [] or [||] *)
        else if last_text = "[" && current_text = "]" then false
        else if last_text = "[|" && current_text = "|]" then false
          (* Track indexing context when we see .[ *)
          (* No space after [ when it's for indexing (prev is .) *)
        else if last_text = "[" && !prev_token_text = Some "." then 
          (in_indexing := true; false)
          (* No space before ] when in indexing context *)
        else if current_text = "]" && !in_indexing then 
          (in_indexing := false; false)
          (* Space after opening bracket/brace *)
        else if last_text = "[" || last_text = "{" || last_text = "[|" then true
          (* Space before closing bracket/brace *)
        else if current_text = "]" || current_text = "}" || current_text = "|]"
        then true (* No space after opening paren *)
        else if last_text = "(" then false (* No space before closing paren *)
        else if current_text = ")" then false
        else if is_opening_delimiter current_text then
          if is_keyword last_text || is_operator last_text then true else true
        else if current_text = ":" then true
        else if last_text = ":" then true
        else if is_punctuation current_text then false
        else if is_keyword last_text then true
        else if is_keyword current_text then true
        else if is_operator last_text then true
        else if is_operator current_text then true
        else true
  in

  let is_actual_whitespace text =
    (* Check if string contains only whitespace characters *)
    String.for_all (fun c -> c = ' ' || c = '\t' || c = '\n' || c = '\r') text
  in

  (**
     Format IF expression with multi-line layout:
     
     if <condition> then
       <then_branch>
     else
       <else_branch>
  *)
  let rec format_if_expr ~needs_indent children =
    let (cond_children, then_children, else_children_opt) = partition_if_expr children in
    
    (* Multi-line IF: start on new line, indented from current level *)
    add_newline ();
    incr indent_level;  (* Indent the whole if expression *)
    add_indent ();
    
    (* Print: if <condition> then *)
    Buffer.add_string buf "if ";
    List.iter (print_element ~needs_indent:false) cond_children;
    Buffer.add_string buf " then";
    add_newline ();
    
    (* Print then-branch (indented one more level from the if) *)
    incr indent_level;
    add_indent ();
    List.iter (print_element ~needs_indent:false) then_children;
    decr indent_level;
    
    (* Print else-branch if present *)
    (match else_children_opt with
     | Some else_children ->
         add_newline ();
         add_indent ();  (* Same level as 'if' *)
         Buffer.add_string buf "else";
         add_newline ();
         incr indent_level;
         add_indent ();
         List.iter (print_element ~needs_indent:false) else_children;
         decr indent_level
     | None -> ());
    
    decr indent_level;  (* Restore indent level *)
    add_newline ();
    (* Reset token tracking *)
    last_token_kind := None;
    last_token_text := None
  
  (**
     Format MATCH expression with multi-line layout:
     
     match <scrutinee> with
     | <pattern> -> <body>
     | <pattern> -> <body>
  *)
  and format_match_expr ~needs_indent children =
    let (scrut_children, case_children) = partition_match_expr children in
    
    (* Multi-line MATCH: put on new line when part of larger expression *)
    add_newline ();
    incr indent_level;
    add_indent ();
    
    (* Print: match <scrutinee> with *)
    Buffer.add_string buf "match ";
    List.iter (print_element ~needs_indent:false) scrut_children;
    Buffer.add_string buf " with";
    add_newline ();
    
    (* Print each case *)
    List.iter (fun case_elem ->
      match case_elem with
      | Syn.Ceibo.Red.Node node ->
          let case_children = Syn.Ceibo.Red.SyntaxNode.children node in
          let (pattern, body) = partition_match_case case_children in
          
          add_indent ();
          Buffer.add_string buf "| ";
          List.iter (print_element ~needs_indent:false) pattern;
          Buffer.add_string buf " -> ";
          List.iter (print_element ~needs_indent:false) body;
          add_newline ()
      | _ -> ()
    ) case_children;
    
    decr indent_level;
    add_newline ();
    (* Reset token tracking *)
    last_token_kind := None;
    last_token_text := None
  
  and print_element ~needs_indent elem =
    match elem with
    | Syn.Ceibo.Red.Token tok -> (
        let text = Syn.Ceibo.Red.SyntaxToken.text tok in
        let kind = Syn.Ceibo.Red.SyntaxToken.kind tok in
        match kind with
        | Syn.SyntaxKind.WHITESPACE ->
            if
              (* WORKAROUND: syn parser marks delimiters as WHITESPACE *)
              (* Only skip actual whitespace characters, not delimiters *)
              is_actual_whitespace text
            then ()
            else (
              (* This is a delimiter misclassified as whitespace *)
              if should_add_space_before text then add_space ();
              Buffer.add_string buf text;
              (* Don't save WHITESPACE as last kind - use a synthetic kind *)
              prev_token_text := !last_token_text;
              last_token_kind := None;
              last_token_text := Some text)
        | Syn.SyntaxKind.COMMENT | Syn.SyntaxKind.DOCSTRING ->
            if should_add_space_before text then add_space ();
            Buffer.add_string buf text;
            prev_token_text := !last_token_text;
            last_token_kind := Some kind;
            last_token_text := Some text
        | _ ->
            if should_add_space_before text then add_space ();
            Buffer.add_string buf text;
            prev_token_text := !last_token_text;
            last_token_kind := Some kind;
            last_token_text := Some text)
    | Syn.Ceibo.Red.Node node -> print_node ~needs_indent node
  and print_node ~needs_indent node =
    let kind = Syn.Ceibo.Red.SyntaxNode.kind node in
    let children = Syn.Ceibo.Red.SyntaxNode.children node in
    
    (**
       Multi-line formatting rules (gofmt-style):
       - IF_EXPR: Always multi-line (for now, later: only if has else)
       - MATCH_EXPR: Always multi-line
       - Other constructs: Use token-based printer
       
       Why fixed rules?
       - Predictable: Same input always produces same output
       - Simple: No need to measure line width or try alternatives
       - Fast: Single pass, no backtracking
       - Proven: gofmt's approach has formatted billions of lines
    *)
    match kind with
    (* Multi-line constructs - use custom formatters *)
    | Syn.SyntaxKind.IF_EXPR ->
        format_if_expr ~needs_indent children
        
    | Syn.SyntaxKind.MATCH_EXPR ->
        format_match_expr ~needs_indent children
    
    (* Top-level structure *)
    | Syn.SyntaxKind.SOURCE_FILE | Syn.SyntaxKind.STRUCTURE ->
        let prev_kind = ref None in
        Array.iteri
          (fun i child ->
            let current_kind = match child with
              | Syn.Ceibo.Red.Node n -> Some (Syn.Ceibo.Red.SyntaxNode.kind n)
              | _ -> None
            in
            (* Add blank line between declarations, except between consecutive opens/includes *)
            if i > 0 then (
              let skip_blank = match (!prev_kind, current_kind) with
                | (Some Syn.SyntaxKind.OPEN_STMT, Some Syn.SyntaxKind.OPEN_STMT)
                | (Some Syn.SyntaxKind.INCLUDE_STMT, Some Syn.SyntaxKind.INCLUDE_STMT)
                | (Some Syn.SyntaxKind.OPEN_STMT, Some Syn.SyntaxKind.INCLUDE_STMT)
                | (Some Syn.SyntaxKind.INCLUDE_STMT, Some Syn.SyntaxKind.OPEN_STMT) -> true
                | _ -> false
              in
              if not skip_blank then add_newline ()
            );
            prev_kind := current_kind;
            print_element ~needs_indent:true child)
          children
          
    (* Simple declarations - single line with token printer *)
    | Syn.SyntaxKind.LET_BINDING | Syn.SyntaxKind.LET_REC_BINDING ->
        if needs_indent then add_indent ();
        Array.iter
          (fun child -> print_element ~needs_indent:false child)
          children;
        add_newline ();
        last_token_kind := None;
        last_token_text := None
        
    | Syn.SyntaxKind.TYPE_DECL ->
        if needs_indent then add_indent ();
        Array.iter
          (fun child -> print_element ~needs_indent:false child)
          children;
        add_newline ();
        last_token_kind := None;
        last_token_text := None
        
    | Syn.SyntaxKind.MODULE_DECL | Syn.SyntaxKind.MODULE_TYPE_DECL ->
        if needs_indent then add_indent ();
        Array.iter
          (fun child -> print_element ~needs_indent:false child)
          children;
        add_newline ();
        last_token_kind := None;
        last_token_text := None
        
    | Syn.SyntaxKind.OPEN_STMT | Syn.SyntaxKind.INCLUDE_STMT ->
        if needs_indent then add_indent ();
        Array.iter
          (fun child -> print_element ~needs_indent:false child)
          children;
        add_newline ();
        last_token_kind := None;
        last_token_text := None
        
    (* Compound expressions - use token printer *)
    | Syn.SyntaxKind.PAREN_EXPR | Syn.SyntaxKind.TUPLE_EXPR
    | Syn.SyntaxKind.LIST_EXPR | Syn.SyntaxKind.ARRAY_EXPR ->
        (* Explicitly print all children including delimiters *)
        Array.iter
          (fun child -> print_element ~needs_indent:false child)
          children
          
    (* Default: use token-based printer *)
    | _ ->
        Array.iter
          (fun child -> print_element ~needs_indent:false child)
          children
  in

  print_element ~needs_indent:false (Syn.Ceibo.Red.Node root);
  Buffer.contents buf
