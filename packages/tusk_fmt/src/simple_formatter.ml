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
  | String { value; _ } -> format "\"%s\"" value
  | Int i -> string_of_int i
  | Float f -> string_of_float f
  | Char c -> format "'%c'" c

let rec format_token_tree ctx prev_token = function
  | Syn.TokenTree.Token tok -> 
      format_token ctx prev_token tok;
      (* Don't update prev_token for whitespace, comments, etc. *)
      (match tok with
       | Syn.Token.Whitespace 
       | Syn.Token.Comment _ 
       | Syn.Token.Docstring _ -> prev_token
       | _ -> Some tok)
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
  | Syn.Token.BeginEnd -> ("begin", "end")
  | Syn.Token.StructEnd -> ("struct", "end")
  | Syn.Token.SigEnd -> ("sig", "end")
  | Syn.Token.ObjectEnd -> ("object", "end")

and format_token ctx prev_token tok =
  (* Decide on spacing/newlines based on token patterns *)
  let needs_space_before = 
    match prev_token, tok with
    (* Never add space before whitespace tokens themselves *)
    | _, Syn.Token.Whitespace -> false
    (* No space before/after comments on same line *)
    | _, Syn.Token.Comment _ | _, Syn.Token.Docstring _ -> false
    | Some (Syn.Token.Comment _), _ | Some (Syn.Token.Docstring _), _ -> true
    
    (* Keywords generally need space after *)
    | Some (Syn.Token.Keyword Syn.Token.Let), Syn.Token.OpenDelim Syn.Token.Paren -> true
    | Some (Syn.Token.Keyword _), _ -> true
    
    (* Identifiers and literals *)
    | Some (Syn.Token.Ident _), Syn.Token.Ident _ -> true  (* Space between identifiers *)
    | Some (Syn.Token.Ident _), Syn.Token.Literal _ -> true  
    | Some (Syn.Token.Ident _), Syn.Token.OpenDelim Syn.Token.Paren -> false  (* No space before ( in function calls *)
    | Some (Syn.Token.Literal (Syn.Token.Int _)), Syn.Token.Ident s when String.length s > 0 && s.[0] = 'x' -> false  (* hex literals like 0x80 *)
    | Some (Syn.Token.Literal _), Syn.Token.Ident _ -> true
    | Some (Syn.Token.Ident _), Syn.Token.Underscore -> true
    | Some Syn.Token.Underscore, Syn.Token.Ident _ -> true
    | Some Syn.Token.Underscore, Syn.Token.OpenDelim Syn.Token.Paren -> false
    
    (* Operators *)
    | Some Syn.Token.Eq, _ -> true
    | _, Syn.Token.Semi -> false
    | Some Syn.Token.Arrow, _ -> true
    | Some Syn.Token.Colon, Syn.Token.Colon -> false  (* :: *)
    | Some Syn.Token.Colon, Syn.Token.Eq -> false  (* := *)
    | Some Syn.Token.Colon, _ -> true
    | Some Syn.Token.Lt, Syn.Token.Eq -> false  (* <= *)
    | Some Syn.Token.Gt, Syn.Token.Eq -> false  (* >= *)
    | Some Syn.Token.Lt, Syn.Token.Gt -> false  (* <> *)
    | Some Syn.Token.Eq, Syn.Token.Gt -> false  (* => *)
    | Some Syn.Token.Minus, Syn.Token.Gt -> false  (* -> *)
    | Some Syn.Token.Star, Syn.Token.Star -> false  (* ** *)
    | Some Syn.Token.And, Syn.Token.And -> false  (* && - but this is wrong, token is already && *)
    | Some Syn.Token.Pipe, Syn.Token.Pipe -> false  (* || - but this is wrong too *)
    | Some Syn.Token.Pipe, Syn.Token.Gt -> false  (* |> *)
    | _, Syn.Token.Eq -> true
    | _, Syn.Token.Arrow -> true
    | Some op, _ when is_binop op -> true
    | _, op when is_binop op -> true
    
    (* Delimiters - no space after opening paren *)
    | Some (Syn.Token.OpenDelim Syn.Token.Paren), _ -> false
    | Some (Syn.Token.OpenDelim Syn.Token.Bracket), _ -> false
    | Some (Syn.Token.OpenDelim Syn.Token.Brace), _ -> false
    | Some (Syn.Token.OpenDelim _), Syn.Token.CloseDelim _ -> false
    | Some (Syn.Token.CloseDelim _), Syn.Token.OpenDelim _ -> false
    | Some (Syn.Token.CloseDelim _), Syn.Token.Ident _ -> true
    | Some (Syn.Token.CloseDelim _), Syn.Token.Literal _ -> true
    | Some (Syn.Token.CloseDelim Syn.Token.Paren), Syn.Token.CloseDelim Syn.Token.Paren -> false
    | Some Syn.Token.Comma, _ -> true
    | _, Syn.Token.Comma -> false
    | Some Syn.Token.Semi, _ -> true
    | _, Syn.Token.CloseDelim Syn.Token.Paren -> false
    
    (* Special cases for keywords that need space before *)
    | _, Syn.Token.Keyword Syn.Token.In -> true
    | _, Syn.Token.Keyword Syn.Token.Then -> true  
    | _, Syn.Token.Keyword Syn.Token.Else -> true
    | _, Syn.Token.Keyword Syn.Token.With -> true
    | _, Syn.Token.Keyword Syn.Token.When -> true
    | _, Syn.Token.Keyword Syn.Token.And -> true
    
    | _ -> false
  in
  
  let needs_newline_after =
    match tok with
    | Syn.Token.Comment _ | Syn.Token.Docstring _ -> true
    | _ -> false
  in
  
  let needs_newline_before =
    match prev_token, tok with
    (* Comments typically go on their own line *)
    | Some _, (Syn.Token.Comment _ | Syn.Token.Docstring _) -> 
        not ctx.last_was_newline
    | Some (Syn.Token.Comment _ | Syn.Token.Docstring _), _ when tok <> Syn.Token.Whitespace -> true
    | _, Syn.Token.Keyword Syn.Token.Let when prev_token <> None -> true
    | _, Syn.Token.Keyword Syn.Token.Type when prev_token <> None -> true
    | _, Syn.Token.Keyword Syn.Token.Module when prev_token <> None && prev_token <> Some (Syn.Token.Keyword Syn.Token.Open) -> true
    | _, Syn.Token.Keyword Syn.Token.Open when prev_token <> None -> true
    | _, Syn.Token.Keyword Syn.Token.Val when prev_token <> None -> true
    | _, Syn.Token.Pipe -> true
    | _ -> false
  in
  
  let needs_indent_after =
    match tok with
    | Syn.Token.Keyword Syn.Token.Struct
    | Syn.Token.Keyword Syn.Token.Sig
    | Syn.Token.Keyword Syn.Token.Object
    | Syn.Token.Keyword Syn.Token.Begin
    | Syn.Token.OpenDelim Syn.Token.Brace -> true
    | _ -> false
  in
  
  let needs_dedent_before =
    match tok with
    | Syn.Token.Keyword Syn.Token.End
    | Syn.Token.CloseDelim Syn.Token.Brace -> true
    | _ -> false
  in
  
  if needs_dedent_before then ctx.indent <- max 0 (ctx.indent - 1);
  if needs_newline_before then emit_newline ctx
  else if needs_space_before && not ctx.last_was_newline then emit ctx " ";
  

  
  (* Debug: Print current token type *)
  (*
  (match tok with
   | Syn.Token.Ident s -> Printf.eprintf "Token: Ident(%s) space=%b prev=%s\n" s needs_space_before 
       (match prev_token with None -> "None" | Some _ -> "Some")
   | Syn.Token.Keyword _k -> Printf.eprintf "Token: Keyword space=%b\n" needs_space_before
   | _ -> ());
  *)
  
  (* Emit the actual token *)
  (match tok with
   | Syn.Token.Comment { value; _ } -> 
       emit ctx (format "(* %s *)" value)
   | Syn.Token.Docstring { value; _ } -> 
       emit ctx (format "(** %s *)" value)
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

and is_binop = function
  | Syn.Token.Plus | Syn.Token.Minus | Syn.Token.Star | Syn.Token.Slash
  | Syn.Token.Percent | Syn.Token.Caret | Syn.Token.Lt | Syn.Token.Gt
  | Syn.Token.And | Syn.Token.Or | Syn.Token.ColonColon -> true
  | _ -> false

let format trees =
  let ctx = create_context () in
  let _ = List.fold_left (fun prev tree ->
    format_token_tree ctx prev tree
  ) None trees in
  Buffer.contents ctx.buffer