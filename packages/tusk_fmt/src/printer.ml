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

(**
   Helper: Check if TYPE_DECL contains variant constructors
   
   Look for TYPE_VARIANT_CONSTR nodes OR the presence of | tokens
   which indicate a variant type
*)
let has_variant_constructors children =
  Array.exists (fun child ->
    match child with
    | Syn.Ceibo.Red.Node node ->
        Syn.Ceibo.Red.SyntaxNode.kind node = Syn.SyntaxKind.TYPE_VARIANT_CONSTR
    | Syn.Ceibo.Red.Token tok ->
        (* Also check for | tokens which indicate variants *)
        Syn.Ceibo.Red.SyntaxToken.text tok = "|"
    | _ -> false
  ) children

(**
   Helper: Extract parts of TYPE_VARIANT declaration
   
   Returns: (header_children, variant_constructors)
   where header is everything before the first constructor
*)
let partition_type_variant children =
  let rec go i header_acc constr_acc =
    if i >= Array.length children then
      (List.rev header_acc, List.rev constr_acc)
    else
      match children.(i) with
      | Syn.Ceibo.Red.Node node when 
          Syn.Ceibo.Red.SyntaxNode.kind node = Syn.SyntaxKind.TYPE_VARIANT_CONSTR ->
          go (i+1) header_acc (children.(i) :: constr_acc)
      | Syn.Ceibo.Red.Token tok ->
          let kind = Syn.Ceibo.Red.SyntaxToken.kind tok in
          if kind = Syn.SyntaxKind.WHITESPACE &&
             String.for_all (fun c -> c = ' ' || c = '\t' || c = '\n' || c = '\r') 
               (Syn.Ceibo.Red.SyntaxToken.text tok)
          then go (i+1) header_acc constr_acc
          else if List.length constr_acc = 0 then
            (* Before first constructor - part of header *)
            go (i+1) (children.(i) :: header_acc) constr_acc
          else
            (* After constructors started - skip pipes between them *)
            go (i+1) header_acc constr_acc
      | elem ->
          if List.length constr_acc = 0 then
            go (i+1) (elem :: header_acc) constr_acc
          else
            go (i+1) header_acc constr_acc
  in
  go 0 [] []

(**
   Helper: Extract parts of TRY expression
   
   Given TRY_EXPR children, split into:
   - body (expression between 'try' and 'with')
   - cases (list of match cases after 'with')
*)
let partition_try_expr children =
  let rec go i state body_acc cases_acc =
    if i >= Array.length children then
      (List.rev body_acc, List.rev cases_acc)
    else
      match children.(i) with
      | Syn.Ceibo.Red.Token tok ->
          let text = Syn.Ceibo.Red.SyntaxToken.text tok in
          let kind = Syn.Ceibo.Red.SyntaxToken.kind tok in
          if kind = Syn.SyntaxKind.WHITESPACE &&
             String.for_all (fun c -> c = ' ' || c = '\t' || c = '\n' || c = '\r') text
          then go (i+1) state body_acc cases_acc
          else if text = "try" then
            go (i+1) `AfterTry body_acc cases_acc
          else if text = "with" then
            go (i+1) `AfterWith body_acc cases_acc
          else
            (match state with
             | `Start | `AfterTry -> go (i+1) state (children.(i) :: body_acc) cases_acc
             | `AfterWith -> go (i+1) state body_acc cases_acc)
      | Syn.Ceibo.Red.Node node ->
          let node_kind = Syn.Ceibo.Red.SyntaxNode.kind node in
          if node_kind = Syn.SyntaxKind.MATCH_CASE then
            go (i+1) state body_acc (children.(i) :: cases_acc)
          else
            (match state with
             | `Start | `AfterTry -> go (i+1) state (children.(i) :: body_acc) cases_acc
             | `AfterWith -> go (i+1) state body_acc cases_acc)
  in
  go 0 `Start [] []

(**
   Helper: Extract parts of LET_IN expression
   
   Given LET_IN children, split into:
   - pattern (binding pattern between 'let' and '=')
   - value (expression between '=' and 'in')
   - body (expression after 'in')
   
   Returns: (pattern_children, value_children, body_children)
*)
let partition_let_in children =
  let rec go i state pattern_acc value_acc body_acc =
    if i >= Array.length children then
      (List.rev pattern_acc, List.rev value_acc, List.rev body_acc)
    else
      match children.(i) with
      | Syn.Ceibo.Red.Token tok ->
          let text = Syn.Ceibo.Red.SyntaxToken.text tok in
          let kind = Syn.Ceibo.Red.SyntaxToken.kind tok in
          if kind = Syn.SyntaxKind.WHITESPACE &&
             String.for_all (fun c -> c = ' ' || c = '\t' || c = '\n' || c = '\r') text
          then go (i+1) state pattern_acc value_acc body_acc
          else if text = "let" then
            go (i+1) `AfterLet pattern_acc value_acc body_acc
          else if text = "=" then
            go (i+1) `AfterEq pattern_acc value_acc body_acc
          else if text = "in" then
            go (i+1) `AfterIn pattern_acc value_acc body_acc
          else
            (match state with
             | `Start | `AfterLet -> go (i+1) state (children.(i) :: pattern_acc) value_acc body_acc
             | `AfterEq -> go (i+1) state pattern_acc (children.(i) :: value_acc) body_acc
             | `AfterIn -> go (i+1) state pattern_acc value_acc (children.(i) :: body_acc))
      | node ->
          (match state with
           | `Start | `AfterLet -> go (i+1) state (children.(i) :: pattern_acc) value_acc body_acc
           | `AfterEq -> go (i+1) state pattern_acc (children.(i) :: value_acc) body_acc
           | `AfterIn -> go (i+1) state pattern_acc value_acc (children.(i) :: body_acc))
  in
  go 0 `Start [] [] []

(** 
   Printer context - mutable state passed through formatting functions
   
   This avoids global mutable state and makes the printer:
   - Thread-safe (each call gets its own context)
   - Testable (can inspect context state)
   - Composable (can create multiple printers)
*)
type printer_ctx = {
  config: Config.t;
  buf: Buffer.t;
  mutable indent_level: int;
  mutable last_token_kind: Syn.SyntaxKind.t option;
  mutable last_token_text: string option;
  mutable prev_token_text: string option;
  mutable in_labeled_arg: bool;
  mutable in_indexing: bool;
  mutable in_record: bool;
  mutable just_added_newline: bool;
}

let create_context config = {
  config;
  buf = Buffer.create 1024;
  indent_level = 0;
  last_token_kind = None;
  last_token_text = None;
  prev_token_text = None;
  in_labeled_arg = false;
  in_indexing = false;
  in_record = false;
  just_added_newline = false;
}

let print_root ~config root =
  let ctx = create_context config in

  let add_indent ctx =
    Buffer.add_string ctx.buf (indent_string ctx.config ctx.indent_level);
    ctx.just_added_newline <- true
  in
  let add_newline ctx = 
    Buffer.add_char ctx.buf '\n';
    ctx.just_added_newline <- true
  in
  let add_space ctx = Buffer.add_char ctx.buf ' ' in

  let should_add_space_before ctx current_text =
    (* Don't add space right after newline/indent *)
    if ctx.just_added_newline then (
      ctx.just_added_newline <- false;
      false
    ) else
    match ctx.last_token_text with
    | None -> false
    | Some last_text ->
        (* Track labeled/optional arg context *)
        if last_text = "~" || last_text = "?" then ctx.in_labeled_arg <- true;
        
        (* Track record context *)
        if last_text = "{" then ctx.in_record <- true
        else if current_text = "}" then ctx.in_record <- false;
        
        (* No space around . for module paths and indexing *)
        if current_text = "." || last_text = "." then false
        (* No space after ` for poly variants *)
        else if last_text = "`" then false
        (* No space after ~ or ? for labeled/optional args *)
        else if last_text = "~" || last_text = "?" then false
        (* No space before/after : in labeled args *)
        else if ctx.in_labeled_arg && (current_text = ":" || last_text = ":") then
          (if current_text <> ":" then ctx.in_labeled_arg <- false; false)
        (* No space around = in record fields *)
        else if ctx.in_record && (current_text = "=" || last_text = "=") then false
        (* No space after prefix operators when they come after = or other operators *)
        else if is_prefix_operator last_text then
          (match ctx.prev_token_text with
           | Some prev when prev = "=" || is_operator prev || prev = "(" || prev = "[" || prev = "," -> false
           | _ -> true)
        (* Space after regular operators *)
        else if is_operator last_text then true
          (* No space in empty brackets [] or [||] *)
        else if last_text = "[" && current_text = "]" then false
        else if last_text = "[|" && current_text = "|]" then false
          (* Track indexing context when we see .[ *)
          (* No space after [ when it's for indexing (prev is .) *)
        else if last_text = "[" && ctx.prev_token_text = Some "." then 
          (ctx.in_indexing <- true; false)
          (* No space before ] when in indexing context *)
        else if current_text = "]" && ctx.in_indexing then 
          (ctx.in_indexing <- false; false)
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
  let rec format_if_expr ctx ~needs_indent children =
    let (cond_children, then_children, else_children_opt) = partition_if_expr children in
    
    (* Multi-line IF: start on new line, indented from current level *)
    add_newline ctx;
    ctx.indent_level <- ctx.indent_level + 1;  (* Indent the whole if expression *)
    add_indent ctx;
    
    (* Print: if <condition> then *)
    Buffer.add_string ctx.buf "if ";
    List.iter (print_element ctx ~needs_indent:false) cond_children;
    Buffer.add_string ctx.buf " then";  (* NO trailing space *)
    add_newline ctx;
    
    (* Print then-branch (indented one more level from the if) *)
    ctx.indent_level <- ctx.indent_level + 1;
    add_indent ctx;
    List.iter (print_element ctx ~needs_indent:false) then_children;
    ctx.indent_level <- ctx.indent_level - 1;
    
    (* Print else-branch if present *)
    (match else_children_opt with
     | Some else_children ->
         add_newline ctx;
         add_indent ctx;  (* Same level as 'if' *)
         Buffer.add_string ctx.buf "else ";  (* Trailing space after else *)
         add_newline ctx;
         ctx.indent_level <- ctx.indent_level + 1;
         add_indent ctx;
         List.iter (print_element ctx ~needs_indent:false) else_children;
         ctx.indent_level <- ctx.indent_level - 1
     | None -> ());
    
    ctx.indent_level <- ctx.indent_level - 1;  (* Restore indent level *)
    add_newline ctx;
    (* Reset token tracking *)
    ctx.last_token_kind <- None;
    ctx.last_token_text <- None
  
  (**
     Format TYPE_VARIANT declaration with multi-line layout:
     
     type name =
       | Constructor1
       | Constructor2
  *)
  and format_type_variant ctx ~needs_indent children =
    let (header_children, variant_constrs) = partition_type_variant children in
    
    (* Print header: type name = *)
    if needs_indent then add_indent ctx;
    List.iter (print_element ctx ~needs_indent:false) header_children;
    add_newline ctx;
    
    (* Print each constructor *)
    ctx.indent_level <- ctx.indent_level + 1;
    List.iter (fun constr_elem ->
      match constr_elem with
      | Syn.Ceibo.Red.Node node ->
          let constr_children = Syn.Ceibo.Red.SyntaxNode.children node in
          add_indent ctx;
          Buffer.add_string ctx.buf "| ";
          Array.iter (print_element ctx ~needs_indent:false) constr_children;
          add_newline ctx
      | _ -> ()
    ) variant_constrs;
    ctx.indent_level <- ctx.indent_level - 1;
    
    (* Reset token tracking *)
    ctx.last_token_kind <- None;
    ctx.last_token_text <- None
  
  (**
     Format LET_IN expression with multi-line layout:
     
     let <pattern> = <value> in
     <body>
  *)
  and format_let_in_expr ctx ~needs_indent children =
    let (pattern_children, value_children, body_children) = partition_let_in children in
    
    (* Multi-line LET_IN: start on new line *)
    add_newline ctx;
    ctx.indent_level <- ctx.indent_level + 1;
    add_indent ctx;
    
    (* Print: let <pattern> = <value> in *)
    Buffer.add_string ctx.buf "let ";
    List.iter (print_element ctx ~needs_indent:false) pattern_children;
    Buffer.add_string ctx.buf " = ";
    ctx.just_added_newline <- true;  (* Suppress spacing before value *)
    List.iter (print_element ctx ~needs_indent:false) value_children;
    Buffer.add_string ctx.buf " in";  (* NO trailing space *)
    add_newline ctx;
    
    (* Print body (not indented further - same level as let) *)
    add_indent ctx;
    List.iter (print_element ctx ~needs_indent:false) body_children;
    
    ctx.indent_level <- ctx.indent_level - 1;
    add_newline ctx;
    (* Reset token tracking *)
    ctx.last_token_kind <- None;
    ctx.last_token_text <- None
  
  (**
     Format TRY expression with multi-line layout:
     
     try
       <body>
     with
     | <pattern> -> <handler>
  *)
  and format_try_expr ctx ~needs_indent children =
    let (body_children, case_children) = partition_try_expr children in
    
    (* Multi-line TRY: start on new line *)
    add_newline ctx;
    ctx.indent_level <- ctx.indent_level + 1;
    add_indent ctx;
    
    (* Print: try *)
    Buffer.add_string ctx.buf "try ";  (* Trailing space *)
    add_newline ctx;
    
    (* Print body (indented) *)
    ctx.indent_level <- ctx.indent_level + 1;
    add_indent ctx;
    List.iter (print_element ctx ~needs_indent:false) body_children;
    ctx.indent_level <- ctx.indent_level - 1;
    add_newline ctx;
    
    (* Print: with *)
    add_indent ctx;
    Buffer.add_string ctx.buf "with ";  (* Trailing space for TRY *)
    add_newline ctx;
    
    (* Print each case *)
    List.iter (fun case_elem ->
      match case_elem with
      | Syn.Ceibo.Red.Node node ->
          let case_children = Syn.Ceibo.Red.SyntaxNode.children node in
          let (pattern, body) = partition_match_case case_children in
          
          add_indent ctx;
          Buffer.add_string ctx.buf "| ";
          List.iter (print_element ctx ~needs_indent:false) pattern;
          Buffer.add_string ctx.buf " -> ";
          ctx.just_added_newline <- true;  (* Suppress spacing before body *)
          List.iter (print_element ctx ~needs_indent:false) body;
          add_newline ctx
      | _ -> ()
    ) case_children;
    
    ctx.indent_level <- ctx.indent_level - 1;
    add_newline ctx;
    (* Reset token tracking *)
    ctx.last_token_kind <- None;
    ctx.last_token_text <- None
  
  (**
     Format MATCH expression with multi-line layout:
     
     match <scrutinee> with
     | <pattern> -> <body>
     | <pattern> -> <body>
  *)
  and format_match_expr ctx ~needs_indent children =
    let (scrut_children, case_children) = partition_match_expr children in
    
    (* Multi-line MATCH: put on new line when part of larger expression *)
    add_newline ctx;
    ctx.indent_level <- ctx.indent_level + 1;
    add_indent ctx;
    
    (* Print: match <scrutinee> with *)
    Buffer.add_string ctx.buf "match ";
    List.iter (print_element ctx ~needs_indent:false) scrut_children;
    Buffer.add_string ctx.buf " with";  (* NO trailing space *)
    add_newline ctx;
    
    (* Print each case *)
    List.iter (fun case_elem ->
      match case_elem with
      | Syn.Ceibo.Red.Node node ->
          let case_children = Syn.Ceibo.Red.SyntaxNode.children node in
          let (pattern, body) = partition_match_case case_children in
          
          add_indent ctx;
          Buffer.add_string ctx.buf "| ";
          List.iter (print_element ctx ~needs_indent:false) pattern;
          Buffer.add_string ctx.buf " -> ";
          ctx.just_added_newline <- true;  (* Suppress spacing before body *)
          List.iter (print_element ctx ~needs_indent:false) body;
          add_newline ctx
      | _ -> ()
    ) case_children;
    
    ctx.indent_level <- ctx.indent_level - 1;
    add_newline ctx;
    (* Reset token tracking *)
    ctx.last_token_kind <- None;
    ctx.last_token_text <- None
  
  and print_element ctx ~needs_indent elem =
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
              if should_add_space_before ctx text then add_space ctx;
              Buffer.add_string ctx.buf text;
              (* Don't save WHITESPACE as last kind - use a synthetic kind *)
              ctx.prev_token_text <- ctx.last_token_text;
              ctx.last_token_kind <- None;
              ctx.last_token_text <- Some text)
        | Syn.SyntaxKind.COMMENT | Syn.SyntaxKind.DOCSTRING ->
            if should_add_space_before ctx text then add_space ctx;
            Buffer.add_string ctx.buf text;
            ctx.prev_token_text <- ctx.last_token_text;
            ctx.last_token_kind <- Some kind;
            ctx.last_token_text <- Some text
        | _ ->
            if should_add_space_before ctx text then add_space ctx;
            Buffer.add_string ctx.buf text;
            ctx.prev_token_text <- ctx.last_token_text;
            ctx.last_token_kind <- Some kind;
            ctx.last_token_text <- Some text)
    | Syn.Ceibo.Red.Node node -> print_node ctx ~needs_indent node
  and print_node ctx ~needs_indent node =
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
        format_if_expr ctx ~needs_indent children
        
    | Syn.SyntaxKind.MATCH_EXPR ->
        format_match_expr ctx ~needs_indent children
    
    | Syn.SyntaxKind.TRY_EXPR ->
        format_try_expr ctx ~needs_indent children
    
    | Syn.SyntaxKind.LET_EXPR | Syn.SyntaxKind.LET_REC_EXPR ->
        format_let_in_expr ctx ~needs_indent children
    
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
              if not skip_blank then add_newline ctx
            );
            prev_kind := current_kind;
            print_element ctx ~needs_indent:true child)
          children
          
    (* Simple declarations - single line with token printer *)
    | Syn.SyntaxKind.LET_BINDING | Syn.SyntaxKind.LET_REC_BINDING ->
        if needs_indent then add_indent ctx;
        (* Check if RHS contains multi-line expression that needs trailing space after = *)
        let has_multiline_rhs = Array.exists (fun child ->
          match child with
          | Syn.Ceibo.Red.Node node ->
              let node_kind = Syn.Ceibo.Red.SyntaxNode.kind node in
              node_kind = Syn.SyntaxKind.LET_EXPR || 
              node_kind = Syn.SyntaxKind.LET_REC_EXPR ||
              node_kind = Syn.SyntaxKind.TRY_EXPR
          | _ -> false
        ) children in
        
        Array.iteri (fun i child ->
          print_element ctx ~needs_indent:false child;
          (* Add trailing space after = if multi-line expression follows *)
          if has_multiline_rhs && i < Array.length children - 1 then (
            match child with
            | Syn.Ceibo.Red.Token tok ->
                if Syn.Ceibo.Red.SyntaxToken.text tok = "=" then
                  Buffer.add_char ctx.buf ' '
            | _ -> ()
          )
        ) children;
        add_newline ctx;
        ctx.last_token_kind <- None;
        ctx.last_token_text <- None
        
    | Syn.SyntaxKind.TYPE_DECL ->
        (* Check if this is a variant type - if so, use multi-line formatter *)
        if has_variant_constructors children then
          format_type_variant ctx ~needs_indent children
        else (
          (* Simple type - single line *)
          if needs_indent then add_indent ctx;
          Array.iter
            (fun child -> print_element ctx ~needs_indent:false child)
            children;
          add_newline ctx;
          ctx.last_token_kind <- None;
          ctx.last_token_text <- None
        )
        
    | Syn.SyntaxKind.MODULE_DECL | Syn.SyntaxKind.MODULE_TYPE_DECL ->
        if needs_indent then add_indent ctx;
        Array.iter
          (fun child -> print_element ctx ~needs_indent:false child)
          children;
        add_newline ctx;
        ctx.last_token_kind <- None;
        ctx.last_token_text <- None
        
    | Syn.SyntaxKind.OPEN_STMT | Syn.SyntaxKind.INCLUDE_STMT ->
        if needs_indent then add_indent ctx;
        Array.iter
          (fun child -> print_element ctx ~needs_indent:false child)
          children;
        add_newline ctx;
        ctx.last_token_kind <- None;
        ctx.last_token_text <- None
        
    (* Compound expressions - use token printer *)
    | Syn.SyntaxKind.PAREN_EXPR | Syn.SyntaxKind.TUPLE_EXPR
    | Syn.SyntaxKind.LIST_EXPR | Syn.SyntaxKind.ARRAY_EXPR ->
        (* Explicitly print all children including delimiters *)
        Array.iter
          (fun child -> print_element ctx ~needs_indent:false child)
          children
          
    (* Default: use token-based printer *)
    | _ ->
        Array.iter
          (fun child -> print_element ctx ~needs_indent:false child)
          children
  in

  print_element ctx ~needs_indent:false (Syn.Ceibo.Red.Node root);
  Buffer.contents ctx.buf
