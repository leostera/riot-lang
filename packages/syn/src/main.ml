open Std

(* Format a token for display *)
let format_token = function
  | Token.Keyword kw ->
      Printf.sprintf "Keyword(%s)" 
        (match kw with
         | Token.Let -> "let" | Token.Type -> "type" | Token.Module -> "module"
         | Token.Open -> "open" | Token.If -> "if" | Token.Then -> "then"
         | Token.Else -> "else" | Token.Match -> "match" | Token.With -> "with"
         | Token.Fun -> "fun" | Token.Function -> "function" | Token.Rec -> "rec"
         | Token.And -> "and" | Token.Or -> "or" | Token.In -> "in"
         | Token.Struct -> "struct" | Token.End -> "end" | Token.Sig -> "sig"
         | Token.Begin -> "begin" | Token.Do -> "do" | Token.Done -> "done"
         | Token.While -> "while" | Token.For -> "for" | Token.To -> "to"
         | Token.Downto -> "downto" | Token.Try -> "try" | Token.Exception -> "exception"
         | Token.As -> "as" | Token.Of -> "of" | Token.When -> "when"
         | Token.Class -> "class" | Token.New -> "new" | Token.Object -> "object"
         | Token.Method -> "method" | Token.Private -> "private" | Token.Virtual -> "virtual"
         | Token.Inherit -> "inherit" | Token.Initializer -> "initializer"
         | Token.Constraint -> "constraint" | Token.Mutable -> "mutable"
         | Token.Nonrec -> "nonrec" | Token.Include -> "include"
         | Token.External -> "external" | Token.Lazy -> "lazy"
         | Token.Assert -> "assert" | Token.True -> "true" | Token.False -> "false"
         | Token.Asr -> "asr" | Token.Land -> "land" | Token.Lor -> "lor"
         | Token.Lsl -> "lsl" | Token.Lsr -> "lsr" | Token.Lxor -> "lxor"
         | Token.Mod -> "mod" | Token.Val -> "val" | Token.Functor -> "functor")
  | Token.Ident s -> Printf.sprintf "Ident(%s)" s
  | Token.Literal (Token.String { value; terminated }) -> 
      Printf.sprintf "String(%S, terminated=%b)" value terminated
  | Token.Literal (Token.Int i) -> Printf.sprintf "Int(%d)" i
  | Token.Literal (Token.Float f) -> Printf.sprintf "Float(%f)" f
  | Token.Literal (Token.Char c) -> Printf.sprintf "Char(%C)" c
  | Token.Comment { value; terminated } -> 
      Printf.sprintf "Comment(%S, terminated=%b)" value terminated
  | Token.Docstring { value; terminated } -> 
      Printf.sprintf "Docstring(%S, terminated=%b)" value terminated
  | Token.OpenDelim Token.Paren -> "OpenDelim(Paren)"
  | Token.OpenDelim Token.Bracket -> "OpenDelim(Bracket)"
  | Token.OpenDelim Token.Brace -> "OpenDelim(Brace)"
  | Token.OpenDelim Token.BeginEnd -> "OpenDelim(BeginEnd)"
  | Token.OpenDelim Token.StructEnd -> "OpenDelim(StructEnd)"
  | Token.OpenDelim Token.SigEnd -> "OpenDelim(SigEnd)"
  | Token.OpenDelim Token.ObjectEnd -> "OpenDelim(ObjectEnd)"
  | Token.CloseDelim Token.Paren -> "CloseDelim(Paren)"
  | Token.CloseDelim Token.Bracket -> "CloseDelim(Bracket)"
  | Token.CloseDelim Token.Brace -> "CloseDelim(Brace)"
  | Token.CloseDelim Token.BeginEnd -> "CloseDelim(BeginEnd)"
  | Token.CloseDelim Token.StructEnd -> "CloseDelim(StructEnd)"
  | Token.CloseDelim Token.SigEnd -> "CloseDelim(SigEnd)"
  | Token.CloseDelim Token.ObjectEnd -> "CloseDelim(ObjectEnd)"
  | Token.Plus -> "Plus"
  | Token.Minus -> "Minus"
  | Token.Star -> "Star"
  | Token.Slash -> "Slash"
  | Token.Percent -> "Percent"
  | Token.Caret -> "Caret"
  | Token.Eq -> "Eq"
  | Token.Lt -> "Lt"
  | Token.Gt -> "Gt"
  | Token.LtEq -> "LtEq"
  | Token.GtEq -> "GtEq"
  | Token.Ne -> "Ne"
  | Token.Bang -> "Bang"
  | Token.And -> "And"
  | Token.Or -> "Or"
  | Token.Colon -> "Colon"
  | Token.Semi -> "Semi"
  | Token.Comma -> "Comma"
  | Token.Dot -> "Dot"
  | Token.Arrow -> "Arrow"
  | Token.FatArrow -> "FatArrow"
  | Token.ColonColon -> "ColonColon"
  | Token.ColonEq -> "ColonEq"
  | Token.Question -> "Question"
  | Token.At -> "At"
  | Token.Hash -> "Hash"
  | Token.Tilde -> "Tilde"
  | Token.Dollar -> "Dollar"
  | Token.Pipe -> "Pipe"
  | Token.Ampersand -> "Ampersand"
  | Token.Underscore -> "Underscore"
  | Token.Whitespace -> "Whitespace"
  | Token.EOF -> "EOF"
  | Token.Unknown c -> Printf.sprintf "Unknown(%C)" c

(* Format token tree *)
let rec format_token_tree indent tree =
  let indent_str = String.make (indent * 2) ' ' in
  match tree with
  | Token_tree.Token tok ->
      Printf.sprintf "%s%s" indent_str (format_token tok)
  | Token_tree.Tree (delim, children) ->
      let delim_str = 
        match delim with
        | Token.Paren -> "Paren"
        | Token.Bracket -> "Bracket"
        | Token.Brace -> "Brace"
        | Token.BeginEnd -> "BeginEnd"
        | Token.StructEnd -> "StructEnd"
        | Token.SigEnd -> "SigEnd"
        | Token.ObjectEnd -> "ObjectEnd"
      in
      let children_str = 
        children 
        |> List.map (format_token_tree (indent + 1))
        |> String.concat "\n"
      in
      Printf.sprintf "%sTree(%s) [\n%s\n%s]" indent_str delim_str children_str indent_str

(* JSON output for tokens *)
let token_to_json tok =
  let open Data.Json in
  match tok with
  | Token.Keyword kw ->
      obj ["type", string "keyword"; 
           "value", string (format_token (Token.Keyword kw))]
  | Token.Ident s ->
      obj ["type", string "ident"; "value", string s]
  | Token.Literal (Token.String { value; terminated }) ->
      obj ["type", string "string"; "value", string value; "terminated", bool terminated]
  | Token.Literal (Token.Int i) ->
      obj ["type", string "int"; "value", int i]
  | Token.Literal (Token.Float f) ->
      obj ["type", string "float"; "value", float f]
  | Token.Literal (Token.Char c) ->
      obj ["type", string "char"; "value", string (String.make 1 c)]
  | Token.Comment { value; terminated } ->
      obj ["type", string "comment"; "value", string value; "terminated", bool terminated]
  | Token.Docstring { value; terminated } ->
      obj ["type", string "docstring"; "value", string value; "terminated", bool terminated]
  | Token.Whitespace ->
      obj ["type", string "whitespace"]
  | Token.EOF ->
      obj ["type", string "eof"]
  | tok ->
      obj ["type", string "token"; "value", string (format_token tok)]

(* JSON output for token trees *)
let rec token_tree_to_json = function
  | Token_tree.Token tok ->
      Data.Json.obj ["type", Data.Json.string "token"; "token", token_to_json tok]
  | Token_tree.Tree (delim, children) ->
      let delim_str = Token_tree.delimiter_to_string delim in
      Data.Json.obj [
        "type", Data.Json.string "tree";
        "delimiter", Data.Json.string delim_str;
        "children", Data.Json.array (List.map token_tree_to_json children)
      ]

let () =
  (* Parse command line arguments *)
  let cmd =
    let open ArgParser in
    let open Arg in
    command "syn" 
    |> version "0.1.0"
    |> about "OCaml syntax analysis tool"
    |> subcommands [
        (* token-stream subcommand *)
        command "token-stream"
        |> about "Print token stream for a file"
        |> args [
            positional "FILE" 
            |> help "OCaml source file to tokenize"
            |> required true;
            
            flag "json"
            |> long "json"
            |> help "Output in JSON format";
          ];
        
        (* token-tree subcommand *)
        command "token-tree"
        |> about "Print token tree for a file"
        |> args [
            positional "FILE"
            |> help "OCaml source file to parse"
            |> required true;
            
            flag "json"
            |> long "json"
            |> help "Output in JSON format";
          ];
      ]
  in

  match ArgParser.get_matches cmd Env.args with
  | Error err ->
      ArgParser.print_error err;
      ArgParser.print_help cmd;
      exit 1
  | Ok matches ->
      match ArgParser.get_subcommand matches with
      | None ->
          ArgParser.print_help cmd;
          exit 1
      | Some ("token-stream", sub_matches) ->
          let file = ArgParser.get_one sub_matches "FILE" |> Option.expect ~msg:"FILE required" in
          let json = ArgParser.get_flag sub_matches "json" in
          (match Fs.read (Path.v file) with
           | Error _err ->
               Printf.eprintf "Error reading file %s\n" file;
               exit 1
           | Ok content ->
               let tokens = Lexer.tokenize content in
               if json then
                 let json_tokens = Data.Json.array (List.map token_to_json tokens) in
                 println "%s" (Data.Json.to_string json_tokens)
               else
                 List.iter (fun tok -> println "%s" (format_token tok)) tokens)
      | Some ("token-tree", sub_matches) ->
          let file = ArgParser.get_one sub_matches "FILE" |> Option.expect ~msg:"FILE required" in
          let json = ArgParser.get_flag sub_matches "json" in
          (match Fs.read (Path.v file) with
           | Error _err ->
               Printf.eprintf "Error reading file %s\n" file;
               exit 1
           | Ok content ->
               let tokens = Lexer.tokenize content in
               let trees = Token_tree.of_tokens tokens in
               if json then
                 let json_trees = Data.Json.array (List.map token_tree_to_json trees) in
                 println "%s" (Data.Json.to_string json_trees)
               else
                 List.iteri (fun i tree ->
                   println "Tree #%d:" i;
                   println "%s" (format_token_tree 0 tree);
                   println ""
                 ) trees)
      | Some (cmd, _) ->
          Printf.eprintf "Unknown subcommand: %s\n" cmd;
          exit 1