open Std
open Std.Collections

type format_error =
  | Cannot_build_cst of Syn.build_cst_error

module Doc = struct
  type t =
    | Empty
    | Text of string
    | Line
    | Concat of t list
    | Indent of int * t

  let empty = Empty
  let text value = if value = "" then Empty else Text value
  let line = Line
  let indent spaces doc = if spaces <= 0 then doc else Indent (spaces, doc)

  let concat docs =
    let rec flatten acc = function
      | [] ->
          List.rev acc
      | Empty :: rest ->
          flatten acc rest
      | Concat nested :: rest ->
          flatten acc (nested @ rest)
      | doc :: rest ->
          flatten (doc :: acc) rest
    in
    match flatten [] docs with
    | [] ->
        Empty
    | [ doc ] ->
        doc
    | docs ->
        Concat docs

  let join separator docs =
    match docs with
    | [] ->
        Empty
    | first :: rest ->
        concat
          (first
          :: (rest
             |> List.map (fun doc -> [ separator; doc ])
             |> List.flatten))

  let rec is_multiline = function
    | Empty ->
        false
    | Text value ->
        String.contains value "\n"
    | Line ->
        true
    | Concat docs ->
        List.exists is_multiline docs
    | Indent (_, doc) ->
        is_multiline doc

  let to_string doc =
    let buffer = IO.Buffer.create 1024 in
    let rec write ~line_start ~indent = function
      | Empty ->
          line_start
      | Text value ->
          write_text ~line_start ~indent value
      | Line ->
          IO.Buffer.add_char buffer '\n';
          true
      | Concat docs ->
          List.fold_left (fun line_start doc -> write ~line_start ~indent doc) line_start docs
      | Indent (extra, doc) ->
          write ~line_start ~indent:(indent + extra) doc
    and write_text ~line_start ~indent value =
      let rec write_lines line_start = function
        | [] ->
            line_start
        | [ line ] ->
            if line_start && String.length line > 0 then
              IO.Buffer.add_string buffer (String.make indent ' ');
            IO.Buffer.add_string buffer line;
            line_start && String.length line = 0
        | line :: rest ->
            if line_start && String.length line > 0 then
              IO.Buffer.add_string buffer (String.make indent ' ');
            IO.Buffer.add_string buffer line;
            IO.Buffer.add_char buffer '\n';
            write_lines true rest
      in
      write_lines line_start (String.split_on_char '\n' value)
    in
    ignore (write ~line_start:true ~indent:0 doc);
    IO.Buffer.contents buffer
end

let trim_trailing_newlines text =
  let rec loop index =
    if index <= 0 then
      ""
    else
      match text.[index - 1] with
      | '\n' | '\r' ->
          loop (index - 1)
      | _ ->
          String.sub text 0 index
  in
  loop (String.length text)

let source_of_syntax_node (node : Syn.Cst.syntax_node) =
  let buffer = IO.Buffer.create 1024 in
  Syn.Ceibo.Red.SyntaxNode.preorder node (function
    | Syn.Ceibo.Red.Token token ->
        IO.Buffer.add_string buffer (Syn.Ceibo.Red.SyntaxToken.text token)
    | Syn.Ceibo.Red.Node _ ->
        ());
  IO.Buffer.contents buffer |> trim_trailing_newlines

let source_of_token token = Syn.Cst.Token.text token
let source_of_ident ident = Syn.Cst.Ident.segments ident |> List.map source_of_token |> String.concat "."
let source_of_result (result : Syn.Parser.parse_result) = result.source
let source_of_pattern pattern = source_of_syntax_node (Syn.Cst.Pattern.syntax_node pattern) |> String.trim
let source_of_parameter parameter = source_of_syntax_node (Syn.Cst.Parameter.syntax_node parameter) |> String.trim

let identifier_character = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '\'' ->
      true
  | _ ->
      false

let source_mentions_identifier source identifier =
  let source_length = String.length source in
  let identifier_length = String.length identifier in
  let rec loop index =
    if index + identifier_length > source_length then
      false
    else if String.sub source index identifier_length = identifier then
      let before_ok =
        index = 0 || not (identifier_character source.[index - 1])
      in
      let after_index = index + identifier_length in
      let after_ok =
        after_index = source_length
        || not (identifier_character source.[after_index])
      in
      if before_ok && after_ok then
        true
      else loop (index + 1)
    else loop (index + 1)
  in
  loop 0

let contains_substring source needle =
  let source_length = String.length source in
  let needle_length = String.length needle in
  if needle_length = 0 then
    true
  else
    let rec loop index =
      if index + needle_length > source_length then
        false
      else if String.sub source index needle_length = needle then
        true
      else loop (index + 1)
    in
    loop 0

let fresh_match_parameter_name syntax_node =
  let source = source_of_syntax_node syntax_node in
  let rec pick = function
    | [] ->
        "value"
    | name :: rest ->
        if source_mentions_identifier source name then
          pick rest
        else name
  in
  pick
    [
      "x";
      "value";
      "arg";
      "input";
      "subject";
      "subject0";
      "subject1";
    ]

let syntax_node_has_comment_like_trivia (node : Syn.Cst.syntax_node) =
  let found = ref false in
  Syn.Ceibo.Red.SyntaxNode.preorder node (function
    | Syn.Ceibo.Red.Token token -> (
        match Syn.Ceibo.Red.SyntaxToken.kind token with
        | Syn.SyntaxKind.COMMENT | Syn.SyntaxKind.DOCSTRING ->
            found := true
        | _ ->
            ())
    | Syn.Ceibo.Red.Node _ ->
        ());
  !found

let source_of_span source (span : Syn.Ceibo.Span.t) =
  if span.end_ <= span.start then
    ""
  else
    String.sub source span.start (span.end_ - span.start)

let source_between source ~start ~end_ =
  source_of_span source (Syn.Ceibo.Span.make ~start ~end_)

let is_whitespace_only text = String.trim text = ""
let contains_comment_like_text text = contains_substring text "(*"

let render_int (literal : Syn.Cst.integer_constant) =
  let prefix =
    match literal.base with
    | Syn.Cst.Decimal -> Option.unwrap_or literal.prefix ~default:""
    | Syn.Cst.Hexadecimal -> "0x"
    | Syn.Cst.Octal -> "0o"
    | Syn.Cst.Binary -> "0b"
  in
  let digits =
    match literal.base with
    | Syn.Cst.Hexadecimal -> String.lowercase_ascii literal.digits
    | Syn.Cst.Decimal | Syn.Cst.Octal | Syn.Cst.Binary -> literal.digits
  in
  let suffix = Option.unwrap_or literal.suffix ~default:"" in
  prefix ^ digits ^ suffix

let render_float (literal : Syn.Cst.float_constant) =
  let exponent =
    match literal.exponent with
    | None -> ""
    | Some exponent ->
        let sign =
          match exponent.sign with
          | None -> ""
          | Some Syn.Cst.Positive -> "+"
          | Some Syn.Cst.Negative -> "-"
        in
        exponent.marker ^ sign ^ exponent.digits
  in
  let suffix = Option.unwrap_or literal.suffix ~default:"" in
  literal.integral_digits ^ "." ^ literal.fractional_digits ^ exponent ^ suffix

let render_literal = function
  | Syn.Cst.Literal.Int literal -> Some (render_int literal)
  | Syn.Cst.Literal.Float literal -> Some (render_float literal)
  | Syn.Cst.Literal.String literal ->
      Some (source_of_syntax_node literal.syntax_node)
  | Syn.Cst.Literal.Char literal ->
      Some (source_of_syntax_node literal.syntax_node)
  | Syn.Cst.Literal.Bool literal ->
      Some (if literal.value then "true" else "false")
  | Syn.Cst.Literal.Unit _ -> Some "()"

let indent_string n = String.make n ' '

let needs_fun_parameter_parens =
  let open Syn.Cst.Pattern in
  function
  | Tuple _ | Or _ | Alias _ | Typed _ | Cons _ | Effect _ ->
      true
  | Identifier _ | Wildcard _ | Literal _ | Extension _ | Lazy _ | Exception _
  | Range _ | Operator _ | FirstClassModule _ | PolyVariant _
  | PolyVariantInherit _ | Constructor _ | List _ | Array _ | Record _
  | LocalOpen _ | Parenthesized _ ->
      false

let render_fun_parameter_pattern pattern =
  let rendered =
    match pattern with
    | Syn.Cst.Pattern.Parenthesized { inner; _ } -> source_of_pattern inner
    | _ -> source_of_pattern pattern
  in
  if needs_fun_parameter_parens pattern then
    "(" ^ rendered ^ ")"
  else rendered

let parameter_is_labeled_or_optional parameter =
  let text = source_of_parameter parameter in
  String.starts_with ~prefix:"~" text || String.starts_with ~prefix:"?" text

let render_binding ~indent ~keyword binding_pattern value =
  let prefix = indent_string indent ^ keyword ^ binding_pattern in
  if Doc.is_multiline value then
    Doc.concat [ Doc.text (prefix ^ " ="); Doc.line; value ]
  else Doc.concat [ Doc.text (prefix ^ " = "); value ]

let indent_block spaces text =
  text |> String.split_on_char '\n'
  |> List.map (fun line -> if line = "" then line else indent_string spaces ^ line)
  |> String.concat "\n"

let indent_first_line spaces text =
  match String.split_on_char '\n' text with
  | [] ->
      text
  | first :: rest ->
      String.concat "\n" ((indent_string spaces ^ first) :: rest)

let collapse_horizontal_spaces text =
  let buffer = IO.Buffer.create (String.length text) in
  let rec loop index previous_was_space =
    if index >= String.length text then
      ()
    else
      match text.[index] with
      | ' ' | '\t' ->
          if not previous_was_space then
            IO.Buffer.add_char buffer ' ';
          loop (index + 1) true
      | ch ->
          IO.Buffer.add_char buffer ch;
          loop (index + 1) false
  in
  loop 0 false;
  IO.Buffer.contents buffer

let normalize_recursive_let_source text =
  match String.index_opt text '=' with
  | None ->
      text
  | Some equals_index ->
      let lhs = String.sub text 0 equals_index |> collapse_horizontal_spaces |> String.trim in
      let rhs =
        String.sub text (equals_index + 1) (String.length text - equals_index - 1)
        |> String.trim
      in
      lhs ^ " = " ^ rhs

let render_guarded_function_cases ~indent source =
  let trimmed = String.trim source in
  let prefix = "function " in
  if not (String.starts_with ~prefix trimmed) then
    Doc.text (indent_string indent ^ trimmed)
  else
    let payload =
      String.sub trimmed (String.length prefix) (String.length trimmed - String.length prefix)
    in
    let cases =
      payload |> String.split_on_char '|'
      |> List.map String.trim
      |> List.filter (fun part -> String.length part > 0)
    in
    Doc.concat
      [
        Doc.text (indent_string indent ^ "function ");
        Doc.line;
        (cases
        |> List.mapi (fun index case ->
               Doc.text
                 (indent_string indent ^ "| " ^ case
                ^
                if index < List.length cases - 1 then
                  " "
                else ""))
        |> Doc.join Doc.line);
      ]

let source_of_or_pattern_alternatives { Syn.Cst.alternatives; _ } =
  alternatives |> List.map source_of_pattern

let rec flatten_sequence_expression = function
  | Syn.Cst.Expression.Sequence { left; right; _ } ->
      flatten_sequence_expression left @ flatten_sequence_expression right
  | expression ->
      [ expression ]

let rec render_expression ~indent = function
  | Syn.Cst.Expression.Literal literal ->
      render_literal literal
      |> Option.expect ~msg:"literal rendering should always succeed"
      |> Doc.text
  | Syn.Cst.Expression.Path { path; _ } ->
      Doc.text (source_of_ident path)
  | Syn.Cst.Expression.Parenthesized { inner = Function function_; _ } ->
      Doc.concat [ Doc.text "("; render_function_expression ~indent:0 function_; Doc.text ")" ]
  | Syn.Cst.Expression.Parenthesized { syntax_node; inner = Tuple _; _ } ->
      Doc.text (source_of_syntax_node syntax_node)
  | Syn.Cst.Expression.Parenthesized { inner; _ } ->
      render_expression ~indent inner
  | Syn.Cst.Expression.Prefix { operator_token; operand = Literal literal; _ } ->
      let rendered =
        render_literal literal |> Option.expect ~msg:"literal rendering should always succeed"
      in
      Doc.text ("(" ^ source_of_token operator_token ^ rendered ^ ")")
  | Syn.Cst.Expression.Let let_ ->
      render_let_expression ~indent let_
  | Syn.Cst.Expression.Fun fun_ ->
      render_fun_expression ~indent fun_
  | Syn.Cst.Expression.Function function_ ->
      render_function_expression ~indent function_
  | Syn.Cst.Expression.Match match_ ->
      render_match_expression ~indent ~keyword_trailing_space:true match_
  | Syn.Cst.Expression.If if_ ->
      render_if_expression ~indent if_
  | Syn.Cst.Expression.Sequence sequence ->
      render_sequence_expression ~indent sequence
  | Syn.Cst.Expression.Apply apply ->
      render_apply_expression ~indent apply
  | expression ->
      Doc.text (source_of_syntax_node (Syn.Cst.Expression.syntax_node expression))

and multiline_case_lines ~case_indent ~or_indent ~is_last_case (case : Syn.Cst.match_case) =
  let guard =
    match case.guard with
    | None -> Doc.empty
    | Some guard -> Doc.concat [ Doc.text " when "; render_expression ~indent:0 guard ]
  in
  let body = render_expression ~indent:0 case.body in
  let trailing_space = if is_last_case then Doc.empty else Doc.text " " in
  match case.pattern with
  | Syn.Cst.Pattern.Or or_pattern -> (
      match source_of_or_pattern_alternatives or_pattern |> List.rev with
      | [] ->
          []
      | last :: rest_reversed ->
          let alternatives = List.rev rest_reversed in
          let rendered_alternatives =
            alternatives
            |> List.map (fun alternative -> Doc.text (indent_string or_indent ^ "| " ^ alternative ^ " "))
          in
          rendered_alternatives
          @ [
              Doc.concat
                [
                  Doc.text (indent_string or_indent ^ "| " ^ last);
                  guard;
                  Doc.text " -> ";
                  body;
                  trailing_space;
                ];
            ])
  | _ ->
      [
        Doc.concat
          [
            Doc.text (indent_string case_indent ^ "| " ^ source_of_pattern case.pattern);
            guard;
            Doc.text " -> ";
            body;
            trailing_space;
          ];
      ]

and multiline_cases ~case_indent ~or_indent cases =
  cases
  |> List.mapi (fun index case ->
         multiline_case_lines ~case_indent ~or_indent
           ~is_last_case:(index = List.length cases - 1) case)
  |> List.flatten

and render_let_expression ~indent
    ({ syntax_node; binding_pattern; bound_value; and_bindings; body; is_recursive; _ } :
      Syn.Cst.let_expression) =
  if is_recursive then
    Doc.text
      (indent_first_line indent
         (source_of_syntax_node syntax_node |> normalize_recursive_let_source))
  else if List.length and_bindings > 0 then
    Doc.text (indent_first_line indent (source_of_syntax_node syntax_node))
  else
    let rendered_bound_value = render_expression ~indent:(indent + 2) bound_value in
    let current =
      render_binding ~indent ~keyword:"let "
        (source_of_pattern binding_pattern)
        rendered_bound_value
    in
    let rendered_body = render_expression ~indent body in
    let rendered_body =
      if Doc.is_multiline rendered_body then
        rendered_body
      else Doc.concat [ Doc.text (indent_string indent); rendered_body ]
    in
    if Doc.is_multiline rendered_bound_value then
      Doc.concat [ current; Doc.line; Doc.text (indent_string indent ^ "in"); Doc.line; rendered_body ]
    else Doc.concat [ current; Doc.text " in"; Doc.line; rendered_body ]

and render_fun_expression ~indent
    ({ syntax_node; parameters; body; attributes } : Syn.Cst.fun_expression) =
  if List.length attributes > 0 then
    Doc.text (source_of_syntax_node syntax_node)
  else
    let rec collect parameters = function
      | Syn.Cst.Expression (Syn.Cst.Expression.Fun nested) ->
          collect (parameters @ nested.parameters) nested.body
      | body ->
          (parameters, body)
    in
    let parameters, body = collect parameters body in
    let rendered_parameters =
      parameters |> List.map source_of_parameter |> String.concat " "
    in
    match body with
    | Syn.Cst.Expression (Syn.Cst.Expression.Match match_)
      when List.exists parameter_is_labeled_or_optional parameters ->
        Doc.concat
          [
            Doc.text ("fun " ^ rendered_parameters ^ " -> ");
            Doc.text (source_of_syntax_node match_.syntax_node);
          ]
    | Syn.Cst.Expression (Syn.Cst.Expression.Match match_) ->
        Doc.concat
          [
            Doc.text ("fun " ^ rendered_parameters ^ " -> ");
            Doc.line;
            render_match_expression ~indent:(indent + 2) ~keyword_trailing_space:true match_;
          ]
    | Syn.Cst.Expression expression ->
        let rendered_body = render_expression ~indent:(indent + 2) expression in
        if Doc.is_multiline rendered_body then
          Doc.concat [ Doc.text ("fun " ^ rendered_parameters ^ " -> "); Doc.line; rendered_body ]
        else Doc.concat [ Doc.text ("fun " ^ rendered_parameters ^ " -> "); rendered_body ]
    | Syn.Cst.Cases case_body ->
        Doc.text (source_of_syntax_node case_body.syntax_node)

and render_function_expression ~indent
    ({ syntax_node; cases; _ } : Syn.Cst.function_expression) =
  let has_or_patterns =
    List.exists
      (fun (case : Syn.Cst.match_case) ->
        match case.pattern with
        | Syn.Cst.Pattern.Or _ -> true
        | _ -> false)
      cases
  in
  let should_lower_to_fun_match =
    List.length cases > 1
    && not has_or_patterns
    && List.for_all
         (fun (case : Syn.Cst.match_case) ->
           case.guard = None
           &&
           match case.pattern with
           | Syn.Cst.Pattern.Literal _ | Syn.Cst.Pattern.Wildcard _ ->
               true
           | _ ->
               false)
         cases
  in
  match cases with
  | [ case ] when case.guard = None ->
      Doc.concat
        [
          Doc.text ("fun " ^ render_fun_parameter_pattern case.pattern ^ " -> ");
          render_expression ~indent case.body;
        ]
  | cases when should_lower_to_fun_match ->
      let parameter_name = fresh_match_parameter_name syntax_node in
      let scrutinee = parameter_name in
      Doc.concat
        [
          Doc.text ("fun " ^ parameter_name ^ " ->");
          Doc.line;
          render_match_expression ~indent:(indent + 2) ~keyword_trailing_space:false
            {
              syntax_node;
              scrutinee =
                Syn.Cst.Expression.Path
                  {
                    syntax_node;
                    path = Syn.Cst.Ident.from_string scrutinee;
                    attributes = [];
                  };
              cases;
              attributes = [];
            };
        ]
  | (cases : Syn.Cst.match_case list) ->
      let case_indent = if has_or_patterns then indent + 1 else indent + 2 in
      Doc.concat
        [
          Doc.text (indent_string indent ^ "function ");
          Doc.line;
          (multiline_cases ~case_indent ~or_indent:(indent + 1) cases |> Doc.join Doc.line);
        ]

and render_match_expression ~indent ~keyword_trailing_space
    ({ scrutinee; cases; _ } : Syn.Cst.match_expression) =
  Doc.concat
    [
      Doc.text (indent_string indent ^ "match ");
      render_expression ~indent:0 scrutinee;
      Doc.text (if keyword_trailing_space then " with " else " with");
      Doc.line;
      (multiline_cases ~case_indent:indent ~or_indent:indent cases |> Doc.join Doc.line);
    ]

and render_if_expression ~indent:_ ({ condition; then_branch; else_branch; _ } : Syn.Cst.if_expression) =
  let rendered_condition = render_expression ~indent:0 condition in
  let rendered_then = render_expression ~indent:0 then_branch in
  match else_branch with
  | None ->
      Doc.concat [ Doc.text "if "; rendered_condition; Doc.text " then "; rendered_then ]
  | Some else_branch ->
      let rendered_else = render_expression ~indent:0 else_branch in
      Doc.concat
        [
          Doc.text "if ";
          rendered_condition;
          Doc.text " then ";
          rendered_then;
          Doc.text " else ";
          rendered_else;
        ]

and render_apply_expression ~indent
    ({ syntax_node; callee; argument; _ } : Syn.Cst.apply_expression) =
  match argument with
  | Syn.Cst.Positional argument ->
      let rendered_callee =
        match callee with
        | Syn.Cst.Expression.Parenthesized { inner; _ } ->
            Doc.concat [ Doc.text "("; render_expression ~indent:0 inner; Doc.text ")" ]
        | _ ->
            render_expression ~indent:0 callee
      in
      let rendered_argument = render_expression ~indent:0 argument in
      if Doc.is_multiline rendered_callee || Doc.is_multiline rendered_argument
      then
        Doc.text (source_of_syntax_node syntax_node)
      else Doc.concat [ rendered_callee; Doc.text " "; rendered_argument ]
  | Syn.Cst.Labeled _ | Syn.Cst.Optional _ ->
      Doc.text (source_of_syntax_node syntax_node)

and render_sequence_expression ~indent ({ left; right; _ } : Syn.Cst.sequence_expression) =
  let expressions =
    flatten_sequence_expression (Syn.Cst.Expression.Sequence { left; right; attributes = []; syntax_node = Syn.Cst.Expression.syntax_node left })
  in
  expressions
  |> List.mapi (fun index expression ->
         let rendered = render_expression ~indent expression in
         if index < List.length expressions - 1 then
           Doc.concat [ rendered; Doc.text ";"; Doc.line ]
         else rendered)
  |> Doc.concat

let render_let_binding (binding : Syn.Cst.LetBinding.t) =
  if List.length binding.attributes > 0 || List.length binding.parameters > 0 then
    None
  else
    let keyword = if binding.is_recursive then "let rec " else "let " in
    let pattern = source_of_pattern binding.binding_pattern in
    Some
      (match binding.value with
      | Syn.Cst.Expression.Function { cases; _ } as function_
        when
          List.exists (fun (case : Syn.Cst.match_case) -> Option.is_some case.guard) cases
          || contains_substring
               (source_of_syntax_node (Syn.Cst.Expression.syntax_node function_))
               " when "
          ->
          let source = source_of_syntax_node (Syn.Cst.Expression.syntax_node function_) in
          Doc.concat
            [
              Doc.text (keyword ^ pattern ^ " = ");
              Doc.line;
              render_guarded_function_cases ~indent:2 source;
            ]
      | Syn.Cst.Expression.Let _ ->
          let value = render_expression ~indent:2 binding.value in
          render_binding ~indent:0 ~keyword pattern value
      | Syn.Cst.Expression.Sequence sequence ->
          let value = render_sequence_expression ~indent:2 sequence in
          render_binding ~indent:0 ~keyword pattern value
      | _ ->
          let value = render_expression ~indent:0 binding.value in
          Doc.concat [ Doc.text (keyword ^ pattern ^ " = "); value ])

let source_of_node_from_source source node =
  source_of_span source (Syn.Ceibo.Red.SyntaxNode.span node)

let render_structure_item ~source = function
  | Syn.Cst.StructureItem.LetBinding binding ->
      if syntax_node_has_comment_like_trivia binding.syntax_node then
        Some (Doc.text (source_of_node_from_source source binding.syntax_node))
      else (
        match render_let_binding binding with
        | Some rendered ->
            Some rendered
        | None ->
            Some (Doc.text (source_of_node_from_source source binding.syntax_node)))
  | _ ->
      None

let render_source_file ~source (source_file : Syn.Cst.source_file) =
  match source_file with
  | Syn.Cst.Implementation { syntax_node; items } ->
      let file_span = Syn.Ceibo.Red.SyntaxNode.span syntax_node in
      let rec render_items acc previous_end is_first_item = function
        | [] ->
            let trailing_source = source_between source ~start:previous_end ~end_:file_span.end_ in
            let acc =
              if is_whitespace_only trailing_source then
                acc
              else Doc.text trailing_source :: acc
            in
            Some (List.rev acc |> Doc.concat)
        | item :: rest ->
            let item_node = Syn.Cst.StructureItem.syntax_node item in
            let item_span = Syn.Ceibo.Red.SyntaxNode.span item_node in
            let interstitial =
              source_between source ~start:previous_end ~end_:item_span.start
            in
            let acc =
              if is_first_item then
                if interstitial = "" || is_whitespace_only interstitial then
                  acc
                else
                  Doc.text interstitial :: acc
              else if contains_comment_like_text interstitial then
                Doc.text interstitial :: acc
              else
                Doc.concat [ Doc.line; Doc.line ] :: acc
            in
            (match render_structure_item ~source item with
            | Some rendered ->
                render_items (rendered :: acc) item_span.end_ false rest
            | None ->
                None)
      in
      render_items [] file_span.start true items
  | Syn.Cst.Interface _ -> None

let format (result : Syn.Parser.parse_result) =
  match Syn.build_cst result with
  | Error err -> Error (Cannot_build_cst err)
  | Ok source_file ->
      let original_source = source_of_result result in
      Ok
        (match render_source_file ~source:original_source source_file with
        | Some rendered ->
            let rendered = Doc.to_string rendered in
            if String.ends_with ~suffix:"\n" original_source
               && String.ends_with ~suffix:"\n" rendered
            then
              rendered
            else if String.ends_with ~suffix:"\n" original_source then
              rendered ^ "\n"
            else rendered
        | None -> original_source)

let syntax_hash (result : Syn.Parser.parse_result) =
  let buffer = IO.Buffer.create 1024 in
  let rec write_element = function
    | Syn.Ceibo.Green.Token _ as element -> (
        match Syn.Ceibo.Green.kind element with
        | Syn.SyntaxKind.WHITESPACE -> ()
        | kind ->
            IO.Buffer.add_string buffer "T(";
            IO.Buffer.add_string buffer (Syn.SyntaxKind.to_string kind);
            IO.Buffer.add_string buffer ":";
            IO.Buffer.add_string buffer
              (Syn.Ceibo.Green.text element |> Option.expect ~msg:"token text");
            IO.Buffer.add_string buffer ")")
    | Syn.Ceibo.Green.Node node as element ->
        IO.Buffer.add_string buffer "N(";
        IO.Buffer.add_string buffer
          (Syn.SyntaxKind.to_string (Syn.Ceibo.Green.kind element));
        IO.Buffer.add_string buffer "[";
        Array.iter write_element (Syn.Ceibo.Green.children node);
        IO.Buffer.add_string buffer "])"
  in
  write_element (Syn.Ceibo.Green.Node result.tree);
  IO.Buffer.contents buffer |> Crypto.hash_string |> Crypto.Digest.hex

let write ~writer result =
  match format result with
  | Error err -> Error (`Format err)
  | Ok formatted -> IO.write_all writer ~buf:formatted |> Result.map_error (fun err -> `Write err)
