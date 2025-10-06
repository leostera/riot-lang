open Std

type context = {
  mutable indent : int;
  mutable newline_before : bool;
  mutable last_was_newline : bool;
  mutable buffer : Buffer.t;
}

let create_context () = {
  indent = 0;
  newline_before = false;
  last_was_newline = true;
  buffer = Buffer.create 1024;
}

let emit ctx s = 
  if ctx.newline_before && not ctx.last_was_newline then begin
    Buffer.add_char ctx.buffer '\n';
    ctx.last_was_newline <- true;
    ctx.newline_before <- false
  end;
  
  if ctx.last_was_newline && String.length s > 0 && s <> "\n" then begin
    for _ = 1 to ctx.indent do
      Buffer.add_string ctx.buffer "  "
    done;
    ctx.last_was_newline <- false
  end;
  
  Buffer.add_string ctx.buffer s;
  if s = "\n" then ctx.last_was_newline <- true

let emit_newline ctx =
  if not ctx.last_was_newline then begin
    Buffer.add_char ctx.buffer '\n';
    ctx.last_was_newline <- true
  end

let emit_space ctx =
  if not ctx.last_was_newline then
    Buffer.add_char ctx.buffer ' '

let keyword_to_string : Syn.Token.keyword -> string = function
  | And -> "and"
  | As -> "as"
  | Asr -> "asr"
  | Assert -> "assert"
  | Begin -> "begin"
  | Class -> "class"
  | Constraint -> "constraint"
  | Do -> "do"
  | Done -> "done"
  | Downto -> "downto"
  | Else -> "else"
  | End -> "end"
  | Exception -> "exception"
  | External -> "external"
  | False -> "false"
  | For -> "for"
  | Fun -> "fun"
  | Function -> "function"
  | Functor -> "functor"
  | If -> "if"
  | In -> "in"
  | Include -> "include"
  | Inherit -> "inherit"
  | Initializer -> "initializer"
  | Land -> "land"
  | Lazy -> "lazy"
  | Let -> "let"
  | Lor -> "lor"
  | Lsl -> "lsl"
  | Lsr -> "lsr"
  | Lxor -> "lxor"
  | Match -> "match"
  | Method -> "method"
  | Mod -> "mod"
  | Module -> "module"
  | Mutable -> "mutable"
  | New -> "new"
  | Nonrec -> "nonrec"
  | Object -> "object"
  | Of -> "of"
  | Open -> "open"
  | Or -> "or"
  | Private -> "private"
  | Rec -> "rec"
  | Sig -> "sig"
  | Struct -> "struct"
  | Then -> "then"
  | To -> "to"
  | True -> "true"
  | Try -> "try"
  | Type -> "type"
  | Val -> "val"
  | Virtual -> "virtual"
  | When -> "when"
  | While -> "while"
  | With -> "with"

let literal_to_string : Syn.Token.literal -> string = function
  | String { value; _ } -> Printf.sprintf "\"%s\"" value
  | Int i -> string_of_int i
  | Float f -> string_of_float f
  | Char c -> Printf.sprintf "'%c'" c

let rec format_token_tree ctx prev_token = function
  | Syn.TokenTree.Token tok -> 
      format_token ctx prev_token tok;
      (* Don't update prev_token for whitespace, comments, etc. *)
      (match tok with
       | Syn.Token.Whitespace 
       | Syn.Token.Comment _ 
       | Syn.Token.Docstring _ -> prev_token
       | _ -> Some tok)
  | Syn.TokenTree.Tree (Syn.Token.BeginEnd, contents) ->
      (* This is a top-level grouping - just format the contents *)
      let last_tok = List.fold_left (fun prev tree ->
        format_token_tree ctx prev tree
      ) prev_token contents in
      last_tok
  | Syn.TokenTree.Tree ((Syn.Token.StructEnd | Syn.Token.SigEnd | Syn.Token.ObjectEnd) as delim, contents) ->
      (* Special handling for struct/sig/object blocks *)
      let open_str, close_str = delimiter_strings delim in
      emit ctx open_str;
      
      (* Add newline and increase indent after struct/sig/object *)
      emit_newline ctx;
      ctx.indent <- ctx.indent + 1;
      
      (* Format contents - need to handle let/type/module keywords specially *)
      let rec format_struct_contents prev = function
        | [] -> prev
        | Syn.TokenTree.Token (Syn.Token.Keyword (Syn.Token.Let | Syn.Token.Type | Syn.Token.Module | Syn.Token.Open | Syn.Token.Include | Syn.Token.Val | Syn.Token.Exception) as kw) :: rest ->
            (* Start of a new definition inside struct/sig *)
            if prev <> None then emit_newline ctx;
            format_token ctx prev kw;
            format_struct_contents (Some kw) rest
        | tree :: rest ->
            let new_prev = format_token_tree ctx prev tree in
            format_struct_contents new_prev rest
      in
      
      let _ = format_struct_contents None contents in
      
      (* Decrease indent and emit end *)
      ctx.indent <- ctx.indent - 1;
      emit_newline ctx;
      emit ctx close_str;
      
      (* Return end as last token *)
      Some (Syn.Token.Keyword Syn.Token.End)
      
  | Syn.TokenTree.Tree (delim, contents) ->
      let open_str, close_str = delimiter_strings delim in
      (* Check if we need space before opening delimiter *)
      let needs_space = match prev_token, delim with
        | Some (Syn.Token.Keyword _), Syn.Token.Paren -> true
        | Some Syn.Token.Eq, Syn.Token.Paren -> true
        | Some (Syn.Token.Ident _), Syn.Token.Paren -> false (* function call *)
        | _ -> false
      in
      if needs_space && not ctx.last_was_newline then emit ctx " ";
      emit ctx open_str;
      let last_tok = List.fold_left (fun prev tree ->
        format_token_tree ctx prev tree
      ) None contents in  (* Start with None inside delimiters *)
      emit ctx close_str;
      last_tok

and delimiter_strings = function
  | Syn.Token.Paren -> ("(", ")")
  | Syn.Token.Brace -> ("{", "}")
  | Syn.Token.Bracket -> ("[", "]")
  | Syn.Token.BeginEnd -> ("", "")  (* Don't output anything for top-level groupings *)
  | Syn.Token.StructEnd -> ("struct", "end")
  | Syn.Token.SigEnd -> ("sig", "end")
  | Syn.Token.ObjectEnd -> ("object", "end")

and format_token ctx prev_token tok =
  let format = Printf.sprintf in
  
  let needs_space_before = match (prev_token, tok) with
    (* No space after opening delimiters or before closing *)
    | None, _ -> false
    (* Space around binary operators *)
    | Some t, op when is_binop op && t <> Syn.Token.OpenDelim Syn.Token.Paren -> true
    | Some op, _ when is_binop op -> true
    (* Space after keywords *)
    | Some (Syn.Token.Keyword _), _ -> true
    (* Space before keywords *)
    | Some _, Syn.Token.Keyword _ -> true
    (* Space after 'in' *)
    | Some (Syn.Token.Keyword Syn.Token.In), _ -> true
    (* Space around = *)
    | Some _, Syn.Token.Eq -> true
    | Some Syn.Token.Eq, _ -> true
    (* Space before and after arrow *)
    | Some _, Syn.Token.Arrow -> true
    | Some Syn.Token.Arrow, _ -> true
    (* No space before comma, semi, pipe *)
    | Some _, (Syn.Token.Comma | Syn.Token.Semi | Syn.Token.Pipe) -> false
    (* Space after comma and colon *)
    | Some (Syn.Token.Comma | Syn.Token.Colon), _ -> true
    (* Space after semi *)
    | Some Syn.Token.Semi, _ -> true
    (* Space between identifiers *)
    | Some (Syn.Token.Ident _), Syn.Token.Ident _ -> true
    (* Space between identifier and number *)
    | Some (Syn.Token.Ident _), Syn.Token.Literal (Syn.Token.Int _) -> true
    (* No space after ( or [ or { *)
    | Some (Syn.Token.OpenDelim _), _ -> false
    (* No space before ) or ] or } *)
    | Some _, Syn.Token.CloseDelim _ -> false
    (* No space around . *)
    | Some _, Syn.Token.Dot -> false
    | Some Syn.Token.Dot, _ -> false
    (* No space after :: *)
    | Some Syn.Token.ColonColon, _ -> false
    (* Default *)
    | _ -> false
  in
  
  let needs_newline_before = match (prev_token, tok) with
    (* Newline after semi at top level *)
    | Some Syn.Token.Semi, _ when ctx.indent = 0 -> true
    (* Pipe on new line in match cases *)
    | Some _, Syn.Token.Pipe -> ctx.indent > 0
    | _ -> false
  in
  
  let needs_newline_after = match tok with
    | _ -> false
  in
  
  let needs_indent_after = match tok with
    | _ -> false
  in
  
  (* Skip whitespace tokens *)
  if tok = Syn.Token.Whitespace then ()
  else begin
    if needs_newline_before then emit_newline ctx
    else if needs_space_before && not ctx.last_was_newline then emit ctx " ";
    
    (* Emit the actual token *)
    (match tok with
     | Syn.Token.Comment { value; _ } -> 
         emit ctx (format "(* %s *)" value);
         emit_newline ctx
     | Syn.Token.Docstring { value; _ } -> 
         emit ctx (format "(** %s *)" value);
         emit_newline ctx
     | Syn.Token.Keyword kw -> emit ctx (keyword_to_string kw)
     | Syn.Token.Ident s -> emit ctx s
     | Syn.Token.Literal lit -> emit ctx (literal_to_string lit)
     | Syn.Token.Plus -> emit ctx "+"
     | Syn.Token.Minus -> emit ctx "-"
     | Syn.Token.Star -> emit ctx "*"
     | Syn.Token.Slash -> emit ctx "/"
     | Syn.Token.Percent -> emit ctx "%"
     | Syn.Token.Caret -> emit ctx "^"
     | Syn.Token.Eq -> emit ctx "="
     | Syn.Token.Lt -> emit ctx "<"
     | Syn.Token.Gt -> emit ctx ">"
     | Syn.Token.LtEq -> emit ctx "<="
     | Syn.Token.GtEq -> emit ctx ">="
     | Syn.Token.Ne -> emit ctx "<>"
     | Syn.Token.Bang -> emit ctx "!"
     | Syn.Token.And -> emit ctx "&&"
     | Syn.Token.Or -> emit ctx "||"
     | Syn.Token.Colon -> emit ctx ":"
     | Syn.Token.Semi -> emit ctx ";"
     | Syn.Token.Comma -> emit ctx ","
     | Syn.Token.Dot -> emit ctx "."
     | Syn.Token.Arrow -> emit ctx "->"
     | Syn.Token.FatArrow -> emit ctx "=>"
     | Syn.Token.ColonColon -> emit ctx "::"
     | Syn.Token.ColonEq -> emit ctx ":="
     | Syn.Token.Question -> emit ctx "?"
     | Syn.Token.At -> emit ctx "@"
     | Syn.Token.Hash -> emit ctx "#"
     | Syn.Token.Tilde -> emit ctx "~"
     | Syn.Token.Dollar -> emit ctx "$"
     | Syn.Token.Pipe -> emit ctx "|"
     | Syn.Token.Ampersand -> emit ctx "&"
     | Syn.Token.Underscore -> emit ctx "_"
     | Syn.Token.Whitespace -> ()
     | Syn.Token.EOF -> ()
     | Syn.Token.Unknown c -> emit ctx (format "%c" c)
     | Syn.Token.OpenDelim Syn.Token.Paren -> emit ctx "("
     | Syn.Token.OpenDelim Syn.Token.Bracket -> emit ctx "["
     | Syn.Token.OpenDelim Syn.Token.Brace -> emit ctx "{"
     | Syn.Token.CloseDelim Syn.Token.Paren -> emit ctx ")"
     | Syn.Token.CloseDelim Syn.Token.Bracket -> emit ctx "]"
     | Syn.Token.CloseDelim Syn.Token.Brace -> emit ctx "}"
     | Syn.Token.OpenDelim _ | Syn.Token.CloseDelim _ -> ());
   
    if needs_newline_after then emit_newline ctx;
    if needs_indent_after then ctx.indent <- ctx.indent + 1
  end

and is_binop = function
  | Syn.Token.Plus | Syn.Token.Minus | Syn.Token.Star | Syn.Token.Slash
  | Syn.Token.Percent | Syn.Token.Caret | Syn.Token.Lt | Syn.Token.Gt
  | Syn.Token.LtEq | Syn.Token.GtEq | Syn.Token.Ne
  | Syn.Token.And | Syn.Token.Or | Syn.Token.ColonColon -> true
  | _ -> false

let format trees =
  let ctx = create_context () in
  let is_first = ref true in
  
  List.iter (fun tree ->
    (* Add blank line between top-level definitions (except for the first) *)
    (match tree with
     | Syn.TokenTree.Tree (Syn.Token.BeginEnd, _) ->
         if not !is_first then (
           (* Add blank line before non-first top-level definitions *)
           emit_newline ctx;
           Buffer.add_char ctx.buffer '\n';
           ctx.last_was_newline <- true
         );
         is_first := false
     | _ -> ());
    
    let _ = format_token_tree ctx None tree in
    ()
  ) trees;
  
  Buffer.contents ctx.buffer