open Std

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

  let rec print_element ~needs_indent elem =
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
    match kind with
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
    | Syn.SyntaxKind.PAREN_EXPR | Syn.SyntaxKind.TUPLE_EXPR
    | Syn.SyntaxKind.LIST_EXPR | Syn.SyntaxKind.ARRAY_EXPR ->
        (* Explicitly print all children including delimiters *)
        Array.iter
          (fun child -> print_element ~needs_indent:false child)
          children
    | _ ->
        Array.iter
          (fun child -> print_element ~needs_indent:false child)
          children
  in

  print_element ~needs_indent:false (Syn.Ceibo.Red.Node root);
  Buffer.contents buf
