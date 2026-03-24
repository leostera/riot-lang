open Std
open Std.Collections

module Doc = Doc
module Source = Source

(* Lowering is not purely CST -> Doc yet. Mixed-file preservation still needs
   original source slices so unsupported items and comment-heavy boundaries can
   stay verbatim when the formatter cannot safely rewrite them. *)

module Expression = struct
  let source_of_syntax_node = Source.source_of_syntax_node
  let source_of_token = Source.source_of_token
  let source_of_ident = Source.source_of_ident
  let source_of_pattern = Source.source_of_pattern
  let source_of_parameter = Source.source_of_parameter
  let contains_substring = Source.contains_substring
  let fresh_match_parameter_name = Source.fresh_match_parameter_name

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

  let render_record_pattern_field (field : Syn.Cst.record_pattern_field) =
    let name = Source.source_of_ident field.field_path in
    match field.pattern with
    | None ->
        name
    | Some pattern ->
        name ^ " = " ^ source_of_pattern pattern

  let render_large_record_pattern ~indent ({ fields; closedness; _ } : Syn.Cst.record_pattern) =
    let rendered_fields =
      fields
      |> List.mapi (fun index field ->
             let suffix =
               if index < List.length fields - 1 || not (closedness = Syn.Cst.Closed) then
                 "; "
               else
                 " "
             in
             Doc.text
               (indent_string (indent + 2) ^ render_record_pattern_field field ^ suffix))
    in
    let closing =
      match closedness with
      | Syn.Cst.Closed ->
          Doc.text (indent_string indent ^ "}")
      | Syn.Cst.Open _ ->
          Doc.text (indent_string (indent + 2) ^ "_ }")
    in
    Doc.concat
      [
        Doc.text (indent_string indent ^ "{ ");
        Doc.line;
        (rendered_fields |> Doc.join Doc.line);
        Doc.line;
        closing;
      ]

  let render_fun_parameter_pattern_doc ~indent pattern =
    match pattern with
    | Syn.Cst.Pattern.Record record when List.length record.fields >= 4 ->
        render_large_record_pattern ~indent record
    | _ ->
        Doc.text (render_fun_parameter_pattern pattern)

  let parameter_is_labeled_or_optional parameter =
    let text = source_of_parameter parameter in
    String.starts_with ~prefix:"~" text || String.starts_with ~prefix:"?" text

  let parameters_are_labeled_or_optional parameters =
    List.exists parameter_is_labeled_or_optional parameters

  let rec pattern_contains_range = function
    | Syn.Cst.Pattern.Range _ ->
        true
    | Syn.Cst.Pattern.Or { alternatives; _ } ->
        List.exists pattern_contains_range alternatives
    | _ ->
        false

  let function_requires_block_layout ({ cases; _ } : Syn.Cst.function_expression) =
    List.exists
      (fun (case : Syn.Cst.match_case) ->
        pattern_contains_range case.pattern
        ||
        match case.pattern with
        | Syn.Cst.Pattern.Or { alternatives; _ } ->
            List.length alternatives >= 5
        | _ ->
            false)
      cases

  let render_binding ~indent ~keyword binding_pattern value =
    let prefix = Doc.indent indent (Doc.text (keyword ^ binding_pattern)) in
    if Doc.is_multiline value then
      Doc.concat [ prefix; Doc.space; Doc.equal; Doc.line; value ]
    else
      Doc.concat [ prefix; Doc.space; Doc.equal; Doc.space; value ]

  let wrap_breaking_value ~indent value =
    Doc.group
      (Doc.concat
         [
           Doc.break ~flat:"" ();
           Doc.indent indent value;
         ])

  let indent_block spaces text =
    text |> String.split_on_char '\n'
    |> List.map (fun line -> if line = "" then line else indent_string spaces ^ line)
    |> String.concat "\n"

  let dedent_multiline_block text =
    match String.split_on_char '\n' text with
    | [] | [ _ ] ->
        text
    | first :: rest ->
        let minimum_indent =
          rest
          |> List.filter (fun line -> not (String.trim line = ""))
          |> List.fold_left
               (fun minimum line ->
                 let rec count index =
                   if index >= String.length line then
                     index
                   else
                     match line.[index] with
                     | ' ' ->
                         count (index + 1)
                     | _ ->
                         index
                 in
                 let indentation = count 0 in
                 match minimum with
                 | None ->
                     Some indentation
                 | Some current ->
                     Some (Int.min current indentation))
               None
        in
        (match minimum_indent with
        | None ->
            text
        | Some indentation ->
            first
            :: List.map
                 (fun line ->
                   if line = "" then
                     line
                   else if indentation <= 0 then
                     line
                   else
                     let removable = Int.min indentation (String.length line) in
                     String.sub line removable (String.length line - removable))
                 rest
            |> String.concat "\n")

  let preserve_multiline_source ~indent text =
    text |> dedent_multiline_block |> indent_block indent |> Doc.text

  let indent_first_line spaces text =
    match String.split_on_char '\n' text with
    | [] ->
        text
    | first :: rest ->
        String.concat "\n" ((indent_string spaces ^ first) :: rest)

  let count_substring_occurrences text needle =
    let text_length = String.length text in
    let needle_length = String.length needle in
    let rec loop index count =
      if needle_length = 0 || index + needle_length > text_length then
        count
      else if String.sub text index needle_length = needle then
        loop (index + needle_length) (count + 1)
      else
        loop (index + 1) count
    in
    loop 0 0

  let find_substring_index text needle =
    let text_length = String.length text in
    let needle_length = String.length needle in
    let rec loop index =
      if needle_length = 0 || index + needle_length > text_length then
        None
      else if String.sub text index needle_length = needle then
        Some index
      else
        loop (index + 1)
    in
    loop 0

  let leading_layout_whitespace text =
    let rec loop index =
      if index >= String.length text then
        text
      else
        match text.[index] with
        | ' ' | '\t' | '\n' | '\r' ->
            loop (index + 1)
        | _ ->
            String.sub text 0 index
    in
    loop 0

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
    let normalize_line line =
      match String.index_opt line '=' with
      | None ->
          collapse_horizontal_spaces line |> String.trim
      | Some equals_index ->
          let lhs = String.sub line 0 equals_index |> collapse_horizontal_spaces |> String.trim in
          let rhs =
            String.sub line (equals_index + 1) (String.length line - equals_index - 1)
            |> collapse_horizontal_spaces |> String.trim
          in
          lhs ^ " = " ^ rhs
    in
    match String.split_on_char '\n' text with
    | [] ->
        text
    | [ line ] ->
        normalize_line line
    | first :: rest ->
        String.concat "\n" (normalize_line first :: rest)

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

  let boolean_chain_operator = function
    | Syn.Cst.Expression.Infix { operator_token; _ } ->
        let operator = source_of_token operator_token in
        if operator = "&&" || operator = "||" then
          Some operator
        else
          None
    | _ ->
        None

  let rec collect_boolean_chain operator = function
    | Syn.Cst.Expression.Infix { left; operator_token; right; _ }
      when source_of_token operator_token = operator ->
        collect_boolean_chain operator left @ collect_boolean_chain operator right
    | expression ->
        [ expression ]

  let render_boolean_chain ~indent operator expression =
    let operands =
      collect_boolean_chain operator expression
      |> List.map (fun operand ->
             Doc.text
               (String.trim
                  (source_of_syntax_node (Syn.Cst.Expression.syntax_node operand))))
    in
    match operands with
    | [] ->
        Doc.empty
    | first :: rest ->
        Doc.group
          (Doc.concat
             [
               Doc.break ~flat:"" ();
               Doc.indent indent
                 (Doc.concat
                    (first
                    :: List.map
                         (fun operand ->
                           Doc.concat [ Doc.break (); Doc.text operator; Doc.space; operand ])
                         rest));
             ])

  let rec expression_contains_sequence = function
    | Syn.Cst.Expression.Sequence _ ->
        true
    | Syn.Cst.Expression.Parenthesized { inner; _ } ->
        expression_contains_sequence inner
    | Syn.Cst.Expression.Let { bound_value; body; and_bindings; _ } ->
        expression_contains_sequence bound_value
        || expression_contains_sequence body
        ||
        List.exists
          (fun (binding : Syn.Cst.let_binding) ->
            expression_contains_sequence binding.value)
          and_bindings
    | Syn.Cst.Expression.Fun { body; _ } -> (
        match body with
        | Syn.Cst.Expression expression ->
            expression_contains_sequence expression
        | Syn.Cst.Cases _ ->
            false)
    | Syn.Cst.Expression.Function { cases; _ } ->
        List.exists
          (fun (case : Syn.Cst.match_case) ->
            expression_contains_sequence case.body
            ||
            match case.guard with
            | None ->
                false
            | Some guard ->
                expression_contains_sequence guard)
          cases
    | Syn.Cst.Expression.Match { scrutinee; cases; _ } ->
        expression_contains_sequence scrutinee
        ||
        List.exists
          (fun (case : Syn.Cst.match_case) ->
            expression_contains_sequence case.body
            ||
            match case.guard with
            | None ->
                false
            | Some guard ->
                expression_contains_sequence guard)
          cases
    | Syn.Cst.Expression.If { condition; then_branch; else_branch; _ } ->
        expression_contains_sequence condition
        || expression_contains_sequence then_branch
        ||
        (match else_branch with
        | None ->
            false
        | Some else_branch ->
            expression_contains_sequence else_branch)
    | Syn.Cst.Expression.Prefix { operand; _ } ->
        expression_contains_sequence operand
    | Syn.Cst.Expression.Infix { left; right; _ } ->
        expression_contains_sequence left || expression_contains_sequence right
    | Syn.Cst.Expression.Path _
    | Syn.Cst.Expression.Literal _
    | _ ->
        false

  let rec render ~indent = function
    | Syn.Cst.Expression.Literal literal ->
        render_literal literal
        |> Option.expect ~msg:"literal rendering should always succeed"
        |> Doc.text
    | Syn.Cst.Expression.Path { path; _ } ->
        Doc.text (source_of_ident path)
    | Syn.Cst.Expression.Tuple { syntax_node; elements; _ } ->
        let rendered = elements |> List.map (render ~indent:0) in
        if List.exists Doc.is_multiline rendered then
          Doc.text (source_of_syntax_node syntax_node)
        else Doc.join (Doc.text ", ") rendered
    | Syn.Cst.Expression.PolyVariant { syntax_node; _ } ->
        Doc.text (String.trim (source_of_syntax_node syntax_node))
    | Syn.Cst.Expression.Parenthesized { syntax_node; inner = Sequence _; _ } ->
        Doc.text (source_of_syntax_node syntax_node)
    | Syn.Cst.Expression.Parenthesized { inner = Function function_; _ } ->
        Doc.concat [ Doc.text "("; render_function_expression ~indent:0 function_; Doc.text ")" ]
    | Syn.Cst.Expression.Parenthesized { syntax_node; inner = Tuple _; _ } ->
        Doc.text (source_of_syntax_node syntax_node)
    | Syn.Cst.Expression.Parenthesized { inner; _ } ->
        render ~indent inner
    | Syn.Cst.Expression.Prefix { operator_token; operand = Literal literal; _ } ->
        let rendered =
          render_literal literal |> Option.expect ~msg:"literal rendering should always succeed"
        in
        Doc.text ("(" ^ source_of_token operator_token ^ rendered ^ ")")
    | Syn.Cst.Expression.Infix infix -> (
        match boolean_chain_operator (Syn.Cst.Expression.Infix infix) with
        | Some operator ->
            render_boolean_chain ~indent operator (Syn.Cst.Expression.Infix infix)
        | None ->
            Doc.text
              (source_of_syntax_node
                 (Syn.Cst.Expression.syntax_node (Syn.Cst.Expression.Infix infix))))
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
      | Some guard -> Doc.concat [ Doc.space; Doc.text "when"; Doc.space; render ~indent:0 guard ]
    in
    let body = render ~indent:(case_indent + 4) case.body in
    let _ = is_last_case in
    match case.pattern with
    | Syn.Cst.Pattern.Or or_pattern -> (
        match source_of_or_pattern_alternatives or_pattern |> List.rev with
        | [] ->
            []
        | last :: rest_reversed ->
            let alternatives = List.rev rest_reversed in
            let rendered_alternatives =
              alternatives
              |> List.map (fun alternative ->
                     Doc.indent or_indent
                       (Doc.concat [ Doc.bar; Doc.space; Doc.text alternative; Doc.space ]))
            in
            rendered_alternatives
            @ [
                if Doc.is_multiline body then
                  Doc.concat
                    [
                      Doc.indent or_indent (Doc.concat [ Doc.bar; Doc.space; Doc.text last ]);
                      guard;
                      Doc.space;
                      Doc.arrow;
                      Doc.line;
                      body;
                    ]
                else
                  Doc.concat
                    [
                      Doc.indent or_indent (Doc.concat [ Doc.bar; Doc.space; Doc.text last ]);
                      guard;
                      Doc.space;
                      Doc.arrow;
                      Doc.space;
                      body;
                    ];
              ])
    | _ ->
        if Doc.is_multiline body then
          [
            Doc.concat
              [
                Doc.indent case_indent
                  (Doc.concat [ Doc.bar; Doc.space; Doc.text (source_of_pattern case.pattern) ]);
                guard;
                Doc.space;
                Doc.arrow;
                Doc.line;
                body;
              ];
          ]
        else
          [
            Doc.concat
              [
                Doc.indent case_indent
                  (Doc.concat [ Doc.bar; Doc.space; Doc.text (source_of_pattern case.pattern) ]);
                guard;
                Doc.space;
                Doc.arrow;
                Doc.space;
                body;
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
      let rendered_bound_value = render ~indent:(indent + 2) bound_value in
      let current =
        render_binding ~indent ~keyword:(if is_recursive then "let rec " else "let ")
          (source_of_pattern binding_pattern)
          rendered_bound_value
      in
      let rendered_body = render ~indent body in
      let rendered_body =
        if Doc.is_multiline rendered_body then
          rendered_body
        else Doc.concat [ Doc.text (indent_string indent); rendered_body ]
      in
      if Doc.is_multiline rendered_bound_value then
        Doc.concat
          [ current; Doc.line; Doc.text (indent_string indent ^ "in"); Doc.line; rendered_body ]
      else Doc.concat [ current; Doc.text " in"; Doc.line; rendered_body ]

  and render_explicit_fun_expression ?(prefix_columns = 0) ~indent parameters expression =
    let rec collect parameters = function
      | Syn.Cst.Expression.Fun nested ->
          (match nested.body with
          | Syn.Cst.Expression nested_expression ->
              collect (parameters @ nested.parameters) nested_expression
          | Syn.Cst.Cases _ ->
              (parameters, Syn.Cst.Expression.Fun nested))
      | expression ->
          (parameters, expression)
    in
    let parameters, expression = collect parameters expression in
    let rendered_parameters = parameters |> List.map source_of_parameter |> String.concat " " in
    match expression with
    | Syn.Cst.Expression.Match match_
      when parameters_are_labeled_or_optional parameters ->
        Doc.concat
          [
            Doc.text ("fun " ^ rendered_parameters ^ " -> ");
            Doc.text (source_of_syntax_node match_.syntax_node);
          ]
    | Syn.Cst.Expression.Match match_ ->
        Doc.concat
          [
            Doc.text ("fun " ^ rendered_parameters ^ " -> ");
            Doc.line;
            render_match_expression ~indent:(indent + 2) ~keyword_trailing_space:true match_;
          ]
    | expression ->
        let rendered_body =
          match expression with
          | Syn.Cst.Expression.Sequence _ ->
              render ~indent:(indent + 1) expression
          | _ ->
              render ~indent:(indent + 2) expression
        in
        let _ = prefix_columns in
        let prefers_multiline =
          parameters_are_labeled_or_optional parameters
          &&
          String.length
            (source_of_syntax_node (Syn.Cst.Expression.syntax_node expression))
          > 40
        in
        if prefers_multiline then
          Doc.concat
            [
              Doc.text ("fun " ^ rendered_parameters ^ " ->");
              Doc.line;
              Doc.indent 2 rendered_body;
            ]
        else
          Doc.group
            (Doc.concat
               [
                 Doc.text ("fun " ^ rendered_parameters ^ " ->");
                 Doc.indent 2 (Doc.concat [ Doc.break (); rendered_body ]);
               ])

  and render_fun_body ~indent parameters body =
    let rec collect parameters = function
      | Syn.Cst.Expression (Syn.Cst.Expression.Fun nested) ->
          collect (parameters @ nested.parameters) nested.body
      | body ->
          (parameters, body)
    in
    let parameters, body = collect parameters body in
    match body with
    | Syn.Cst.Expression expression ->
        render_explicit_fun_expression ~indent parameters expression
    | Syn.Cst.Cases case_body ->
        Doc.text (source_of_syntax_node case_body.syntax_node)

  and render_fun_expression ~indent
      ({ syntax_node; parameters; body; attributes } : Syn.Cst.fun_expression) =
    if List.length attributes > 0 then
      Doc.text (source_of_syntax_node syntax_node)
    else render_fun_body ~indent parameters body

  and render_function_expression ~indent
      (({ syntax_node; cases; _ } as function_) : Syn.Cst.function_expression) =
    let has_or_patterns =
      List.exists
        (fun (case : Syn.Cst.match_case) ->
          match case.pattern with
          | Syn.Cst.Pattern.Or _ -> true
          | _ -> false)
        cases
    in
    let requires_block_layout = function_requires_block_layout function_ in
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
        let rendered_pattern = render_fun_parameter_pattern_doc ~indent:(indent + 2) case.pattern in
        if Doc.is_multiline rendered_pattern then
          let rendered_body = render ~indent:(indent + 2) case.body in
          Doc.concat
            [
              Doc.text "fun ";
              Doc.line;
              rendered_pattern;
              Doc.text " -> ";
              Doc.line;
              rendered_body;
            ]
        else
          Doc.concat
            [
              Doc.text ("fun " ^ render_fun_parameter_pattern case.pattern ^ " -> ");
              render ~indent case.body;
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
        let case_indent, or_indent =
          if requires_block_layout then
            (indent, indent)
          else if has_or_patterns then
            (indent + 1, indent + 1)
          else
            (indent + 2, indent)
        in
        Doc.concat
          [
            Doc.indent indent (Doc.concat [ Doc.text "function"; Doc.space ]);
            Doc.line;
            (multiline_cases ~case_indent ~or_indent cases |> Doc.join Doc.line);
          ]

  and render_match_expression ~indent ~keyword_trailing_space
      ({ syntax_node; scrutinee; cases; _ } : Syn.Cst.match_expression) =
    let source = source_of_syntax_node syntax_node in
    if
      Source.syntax_node_has_comment_like_trivia syntax_node
      || contains_substring source " when "
    then
      preserve_multiline_source ~indent source
    else
      Doc.concat
        [
          Doc.indent indent (Doc.concat [ Doc.text "match"; Doc.space ]);
          render ~indent:0 scrutinee;
          Doc.text (if keyword_trailing_space then " with " else " with");
          Doc.line;
          (multiline_cases ~case_indent:indent ~or_indent:indent cases |> Doc.join Doc.line);
        ]

  and render_if_expression ~indent
      ({ syntax_node; condition; then_branch; else_branch; _ } : Syn.Cst.if_expression) =
    let source = source_of_syntax_node syntax_node in
    if Source.syntax_node_has_comment_like_trivia syntax_node then
      preserve_multiline_source ~indent source
    else
      let rendered_condition = render ~indent:0 condition in
      let rendered_then = render ~indent:(indent + 2) then_branch in
      match else_branch with
      | None ->
          Doc.concat [ Doc.text "if "; rendered_condition; Doc.text " then "; rendered_then ]
      | Some else_branch ->
          let rendered_else = render ~indent:(indent + 2) else_branch in
          let indented_then =
            if Doc.is_multiline rendered_then then
              rendered_then
            else
              Doc.concat [ Doc.text (indent_string (indent + 2)); rendered_then ]
          in
          let indented_else =
            if Doc.is_multiline rendered_else then
              rendered_else
            else
              Doc.concat [ Doc.text (indent_string (indent + 2)); rendered_else ]
          in
          let prefers_multiline =
            Doc.is_multiline rendered_then
            || Doc.is_multiline rendered_else
            ||
            match (then_branch, else_branch) with
            | Syn.Cst.Expression.If _, _
            | Syn.Cst.Expression.Match _, _
            | Syn.Cst.Expression.Sequence _, _ ->
                true
            | _, (Syn.Cst.Expression.If _ | Syn.Cst.Expression.Match _ | Syn.Cst.Expression.Sequence _) ->
                true
            | _ ->
                false
          in
          if prefers_multiline then
            Doc.concat
              [
                Doc.indent indent (Doc.concat [ Doc.text "if"; Doc.space ]);
                rendered_condition;
                Doc.text " then";
                Doc.line;
                indented_then;
                Doc.line;
                Doc.indent indent (Doc.text "else");
                Doc.line;
                indented_else;
              ]
          else
            Doc.group
              (Doc.concat
                 [
                   Doc.text "if ";
                   rendered_condition;
                   Doc.text " then ";
                   rendered_then;
                   Doc.text " else ";
                   rendered_else;
                 ])

  and render_apply_expression ~indent
      ({ syntax_node; callee; argument; _ } : Syn.Cst.apply_expression) =
    match argument with
    | Syn.Cst.Positional argument ->
        let rendered_callee =
          match callee with
          | Syn.Cst.Expression.Parenthesized { inner; _ } ->
              Doc.concat [ Doc.text "("; render ~indent:0 inner; Doc.text ")" ]
          | _ ->
              render ~indent:0 callee
        in
        let rendered_argument = render ~indent:0 argument in
        if Source.syntax_node_has_comment_like_trivia syntax_node then
          Doc.text (source_of_syntax_node syntax_node)
        else
          Doc.group
            (Doc.concat
               [
                 rendered_callee;
                 Doc.indent 2 (Doc.concat [ Doc.break (); rendered_argument ]);
               ])
    | Syn.Cst.Labeled _ | Syn.Cst.Optional _ ->
        Doc.text (source_of_syntax_node syntax_node)

  and render_sequence_expression ~indent ({ left; right; _ } : Syn.Cst.sequence_expression) =
    let rec flatten acc = function
      | Syn.Cst.Expression.Sequence { left; right; _ } ->
          flatten (flatten acc left) right
      | expression ->
          acc @ [ expression ]
    in
    let expressions = flatten [] left @ [ right ] in
    expressions
    |> List.mapi (fun index expression ->
           let rendered = render ~indent:0 expression in
           let suffix = if index < List.length expressions - 1 then Doc.text ";" else Doc.empty in
           if Doc.is_multiline rendered then
             Doc.concat [ Doc.indent indent rendered; suffix ]
           else
             Doc.concat [ Doc.indent indent rendered; suffix ])
    |> Doc.join Doc.line

  let render_parameterized_let_binding ~keyword ~pattern ~parameters ~value =
    let rendered_parameters = parameters |> List.map source_of_parameter |> String.concat " " in
    if parameters_are_labeled_or_optional parameters then
      let prefix = keyword ^ pattern ^ " =" in
      let rendered_value =
        render_explicit_fun_expression ~prefix_columns:(String.length prefix) ~indent:0
          parameters value
      in
      Doc.concat [ Doc.text (keyword ^ pattern); Doc.space; Doc.equal; Doc.space; rendered_value ]
    else
      let binding_pattern = pattern ^ " " ^ rendered_parameters in
      let rendered_value =
        match value with
        | Syn.Cst.Expression.If if_ ->
            render_if_expression ~indent:2 if_
        | Syn.Cst.Expression.Match match_ ->
            render_match_expression ~indent:2 ~keyword_trailing_space:false match_
        | Syn.Cst.Expression.Sequence sequence ->
            render_sequence_expression ~indent:2 sequence
        | _ ->
            render ~indent:0 value
      in
      render_binding ~indent:0 ~keyword binding_pattern rendered_value

  let render_let_binding (binding : Syn.Cst.LetBinding.t) =
    if List.length binding.attributes > 0 then
      None
    else
      let keyword = if binding.is_recursive then "let rec " else "let " in
      let pattern = source_of_pattern binding.binding_pattern in
      let rendered =
        if List.length binding.parameters > 0 then
          render_parameterized_let_binding ~keyword ~pattern ~parameters:binding.parameters
            ~value:binding.value
        else
          match binding.value with
          | Syn.Cst.Expression.Infix infix -> (
              match boolean_chain_operator (Syn.Cst.Expression.Infix infix) with
              | Some _ ->
                  let value = render ~indent:2 binding.value in
                  Doc.concat
                    [
                      Doc.text (keyword ^ pattern);
                      Doc.space;
                      Doc.equal;
                      Doc.space;
                      value;
                    ]
              | None ->
                  let value = render ~indent:0 binding.value in
                  Doc.concat
                    [
                      Doc.text (keyword ^ pattern);
                      Doc.space;
                      Doc.equal;
                      Doc.space;
                      value;
                    ])
          | Syn.Cst.Expression.Function function_ ->
              let source = source_of_syntax_node (Syn.Cst.Expression.syntax_node binding.value) in
              if
                List.exists (fun (case : Syn.Cst.match_case) -> Option.is_some case.guard)
                  function_.cases
                || contains_substring source " when "
              then
                Doc.concat
                  [
                    Doc.text (keyword ^ pattern);
                    Doc.space;
                    Doc.equal;
                    Doc.line;
                    render_guarded_function_cases ~indent:2 source;
                  ]
              else
                let value =
                  render_function_expression
                    ~indent:(if List.length function_.cases > 1 then 2 else 0)
                    function_
                in
                if function_requires_block_layout function_ || List.length function_.cases > 1
                then
                  render_binding ~indent:0 ~keyword pattern value
                else
                  Doc.concat [ Doc.text (keyword ^ pattern); Doc.space; Doc.equal; Doc.space; value ]
          | Syn.Cst.Expression.Let _ ->
              let value = render ~indent:2 binding.value in
              render_binding ~indent:0 ~keyword pattern value
          | Syn.Cst.Expression.Fun _ ->
              if expression_contains_sequence binding.value then
                let value = render ~indent:1 binding.value in
                render_binding ~indent:0 ~keyword pattern value
              else
                let value = render ~indent:0 binding.value in
                Doc.concat [ Doc.text (keyword ^ pattern); Doc.space; Doc.equal; Doc.space; value ]
          | Syn.Cst.Expression.If if_ ->
              let value = render_if_expression ~indent:2 if_ in
              if Doc.is_multiline value then
                render_binding ~indent:0 ~keyword pattern value
              else
                Doc.concat [ Doc.text (keyword ^ pattern); Doc.space; Doc.equal; Doc.space; value ]
          | Syn.Cst.Expression.Match match_ ->
              let value =
                render_match_expression ~indent:2 ~keyword_trailing_space:false match_
              in
              if Doc.is_multiline value then
                render_binding ~indent:0 ~keyword pattern value
              else
                Doc.concat [ Doc.text (keyword ^ pattern); Doc.space; Doc.equal; Doc.space; value ]
          | Syn.Cst.Expression.Try { syntax_node; _ } ->
              let source = source_of_syntax_node syntax_node in
              if contains_substring source "\n" then
                let value = preserve_multiline_source ~indent:2 source in
                render_binding ~indent:0 ~keyword pattern value
              else
                let value = render ~indent:0 binding.value in
                Doc.concat [ Doc.text (keyword ^ pattern); Doc.space; Doc.equal; Doc.space; value ]
          | Syn.Cst.Expression.Sequence sequence ->
              let value = render_sequence_expression ~indent:2 sequence in
              render_binding ~indent:0 ~keyword pattern value
          | _ ->
              let value = render ~indent:0 binding.value in
              if Doc.is_multiline value then
                (match binding.value with
                | Syn.Cst.Expression.Literal (Syn.Cst.Literal.String _) ->
                    Doc.concat [ Doc.text (keyword ^ pattern); Doc.space; Doc.equal; Doc.space; value ]
                | _ ->
                    render_binding ~indent:0 ~keyword pattern value)
              else
                Doc.concat [ Doc.text (keyword ^ pattern); Doc.space; Doc.equal; Doc.space; value ]
      in
      Some rendered
end

module Structure = struct
  type rendered_structure_item = {
    doc : Doc.t;
    preserves_layout : bool;
    trailing_layout : string;
    consumed_end : int option;
    start_override : int option;
  }

  let fallback_source_of_core_type type_ =
    Source.source_of_syntax_node (Syn.Cst.CoreType.syntax_node type_) |> String.trim

  let render_type_parameter parameter =
    let variance =
      match Syn.Cst.TypeParameter.variance parameter with
      | Some (Syn.Cst.TypeParameterVariance.Covariant { marker_token }) ->
          Source.source_of_token marker_token
      | Some (Syn.Cst.TypeParameterVariance.Contravariant { marker_token }) ->
          Source.source_of_token marker_token
      | None ->
          ""
    in
    let injective = if Syn.Cst.TypeParameter.is_injective parameter then "!" else "" in
    let variable =
      match Syn.Cst.TypeParameter.type_variable parameter with
      | Some variable ->
          Syn.Cst.TypeVariable.text variable
      | None ->
          "_"
    in
    variance ^ injective ^ variable

  let source_of_module_type ~source module_type =
    Source.source_of_node_from_source source (Syn.Cst.ModuleType.syntax_node module_type)
    |> String.trim

  let indent_string n = String.make n ' '

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

  let count_substring_occurrences text needle =
    let text_length = String.length text in
    let needle_length = String.length needle in
    let rec loop index count =
      if needle_length = 0 || index + needle_length > text_length then
        count
      else if String.sub text index needle_length = needle then
        loop (index + needle_length) (count + 1)
      else
        loop (index + 1) count
    in
    loop 0 0

  let find_substring_index text needle =
    let text_length = String.length text in
    let needle_length = String.length needle in
    let rec loop index =
      if needle_length = 0 || index + needle_length > text_length then
        None
      else if String.sub text index needle_length = needle then
        Some index
      else
        loop (index + 1)
    in
    loop 0

  let leading_layout_whitespace text =
    let rec loop index =
      if index >= String.length text then
        text
      else
        match text.[index] with
        | ' ' | '\t' | '\n' | '\r' ->
            loop (index + 1)
        | _ ->
            String.sub text 0 index
    in
    loop 0

  let rec render_core_type ~source ?(context = `top) type_ =
    let text =
      match type_ with
      | Syn.Cst.CoreType.Wildcard _ ->
          "_"
      | Syn.Cst.CoreType.Var { name_token; _ } ->
          let name = Source.source_of_token name_token in
          if name = "_" then "_" else "'" ^ name
      | Syn.Cst.CoreType.Constr { constructor_path; arguments; _ } ->
          let path = Source.source_of_ident constructor_path in
          (match arguments with
          | [] ->
              path
          | [ Syn.Cst.CoreType.Tuple { elements; _ } ] ->
              "("
              ^ (elements
                |> List.map (fun element -> render_core_type ~source element)
                |> String.concat ", ")
              ^ ") "
              ^ path
          | [ argument ] ->
              render_type_argument ~source argument ^ " " ^ path
          | arguments ->
              "("
              ^ (arguments
                |> List.map (fun argument -> render_core_type ~source argument)
                |> String.concat ", ")
              ^ ") "
              ^ path)
      | Syn.Cst.CoreType.Class _ ->
          fallback_source_of_core_type type_
      | Syn.Cst.CoreType.Alias { type_; name_token; _ } ->
          render_core_type ~source type_ ^ " as " ^ Source.source_of_token name_token
      | Syn.Cst.CoreType.Attribute { type_; _ } ->
          render_core_type ~source type_
      | Syn.Cst.CoreType.Extension _ ->
          fallback_source_of_core_type type_
      | Syn.Cst.CoreType.Poly { binders; body; _ } ->
          (binders |> List.map Syn.Cst.TypeBinder.text |> String.concat " ")
          ^ ". "
          ^ render_core_type ~source body
      | Syn.Cst.CoreType.Arrow _ as arrow_type ->
          render_arrow_type ~source arrow_type
      | Syn.Cst.CoreType.Tuple { elements; _ } ->
          elements |> List.map (render_tuple_element_type ~source) |> String.concat " * "
      | Syn.Cst.CoreType.Parenthesized { inner; _ } ->
          "(" ^ render_core_type ~source inner ^ ")"
      | Syn.Cst.CoreType.LocalOpen _ ->
          fallback_source_of_core_type type_
      | Syn.Cst.CoreType.PolyVariant { kind; fields; _ } ->
          render_core_poly_variant_kind kind
          ^ (fields |> List.map (render_core_poly_variant_field ~source) |> String.concat "")
          ^ "]"
      | Syn.Cst.CoreType.Record { syntax_node; _ }
      | Syn.Cst.CoreType.Object { syntax_node; _ } ->
          Source.source_of_node_from_source source syntax_node |> String.trim
      | Syn.Cst.CoreType.FirstClassModule { syntax_node; _ } ->
          Source.source_of_node_from_source source syntax_node |> String.trim
    in
    if core_type_needs_parentheses ~context type_ then
      "(" ^ text ^ ")"
    else
      text

  and core_type_needs_parentheses ~context = function
    | Syn.Cst.CoreType.Arrow _ | Syn.Cst.CoreType.Poly _ | Syn.Cst.CoreType.Alias _ -> (
        match context with
        | `top ->
            false
        | `arrow_parameter | `type_argument ->
            true)
    | Syn.Cst.CoreType.Tuple _ | Syn.Cst.CoreType.PolyVariant _ -> (
        match context with
        | `type_argument ->
            true
        | `top | `arrow_parameter ->
            false)
    | _ ->
        false

  and render_type_argument ~source type_ =
    render_core_type ~source ~context:`type_argument type_

  and render_tuple_element_type ~source type_ =
    render_core_type ~source ~context:`arrow_parameter type_

  and render_arrow_parameter ~source label parameter_type =
    let parameter_type = render_core_type ~source ~context:`arrow_parameter parameter_type in
    match label with
    | None ->
        parameter_type
    | Some label ->
        let prefix =
          if Syn.Cst.ArrowLabel.is_optional label then
            "?"
          else
            ""
        in
        prefix ^ Syn.Cst.ArrowLabel.name label ^ ":" ^ parameter_type

  and decompose_arrow parameters = function
    | Syn.Cst.CoreType.Arrow { label; parameter_type; result_type; _ } ->
        decompose_arrow ((label, parameter_type) :: parameters) result_type
    | result_type ->
        (List.rev parameters, result_type)

  and render_arrow_type ~source arrow_type =
    let parameters, result_type =
      decompose_arrow [] arrow_type
    in
    let segments =
      parameters
      |> List.map (fun (label, parameter_type) ->
             render_arrow_parameter ~source label parameter_type)
    in
    let result = render_core_type ~source result_type in
    let flat = String.concat " -> " (segments @ [ result ]) in
    let should_break =
      List.length segments > 5
      || Source.contains_substring flat "\n"
      || String.length flat > 100
    in
    if should_break then
      match segments with
      | [] ->
          result
      | first :: rest ->
          first
          ^ " ->\n"
          ^ ((rest @ [ result ])
            |> List.mapi (fun index segment ->
                   if index < List.length rest then
                     "  " ^ segment ^ " ->"
                   else "  " ^ segment)
            |> String.concat "\n")
    else
      flat

  and render_core_poly_variant_kind = function
    | Syn.Cst.PolyVariantBound.Exact ->
        "[ "
    | Syn.Cst.PolyVariantBound.UpperBound { marker_token }
    | Syn.Cst.PolyVariantBound.LowerBound { marker_token } ->
        "[" ^ Source.source_of_token marker_token ^ " "

  and render_core_poly_variant_field ~source = function
    | Syn.Cst.RowField.Tag tag ->
        let name = "`" ^ Syn.Cst.PolyVariantTag.name tag in
        (match Syn.Cst.PolyVariantTag.payload_type tag with
        | None ->
            "| " ^ name ^ " "
        | Some payload_type ->
            "| " ^ name ^ " of " ^ render_core_type ~source payload_type ^ " ")
    | Syn.Cst.RowField.Inherit { type_; _ } ->
        "| " ^ render_core_type ~source type_ ^ " "

  let render_type_parameters ~source parameters =
    match parameters |> List.map render_type_parameter with
    | [] ->
        ""
    | [ parameter ] ->
        parameter ^ " "
    | [ left; right ] ->
        "(" ^ left ^ ", " ^ right ^ ") "
    | parameters ->
        "(\n"
        ^ (parameters
          |> List.mapi (fun index parameter ->
                 "  " ^ parameter
                 ^
                 if index < List.length parameters - 1 then
                   ","
                 else "")
          |> String.concat "\n")
        ^ "\n) "

  let render_type_constraints ~source constraints =
    match constraints with
    | [] ->
        ""
    | constraints ->
        "\n"
        ^ (constraints
          |> List.map (fun ({ Syn.Cst.left; right; _ } : Syn.Cst.type_constraint) ->
                 "  constraint "
                 ^ render_core_type ~source left
                 ^ " = "
                 ^ render_core_type ~source right)
          |> String.concat "\n")

  let render_record_field ~source ~indent ~always_terminate field =
    let prefix =
      (if Syn.Cst.RecordField.is_mutable field then "mutable " else "")
      ^ Syn.Cst.RecordField.name field
      ^ " :"
    in
    let field_type = render_core_type ~source (Syn.Cst.RecordField.field_type field) in
    let terminator = if always_terminate then ";" else "" in
    if Source.contains_substring field_type "\n"
       || String.length field_type > 30
       || String.length prefix + 1 + String.length field_type > 100
    then
      indent_string indent
      ^ prefix
      ^ "\n"
      ^ indent_block (indent + 2) field_type
      ^ terminator
    else
      indent_string indent ^ prefix ^ " " ^ field_type ^ terminator

  let render_inline_record_fields ~source fields =
    let count = List.length fields in
    fields
    |> List.mapi (fun index field ->
           let suffix = if index < count - 1 then ";" else "" in
           let base =
             (if Syn.Cst.RecordField.is_mutable field then "mutable " else "")
             ^ Syn.Cst.RecordField.name field
             ^ " : "
             ^ render_core_type ~source (Syn.Cst.RecordField.field_type field)
           in
           "      " ^ base ^ suffix ^ " ")
    |> String.concat "\n"

  let render_variant_constructor ~source constructor =
    let name = Syn.Cst.VariantConstructor.name constructor in
    match Syn.Cst.VariantConstructor.result_type constructor with
    | Some _ ->
        "  | "
        ^ name
        ^ " : "
        ^ render_core_type ~source
            (Option.expect
               (Syn.Cst.VariantConstructor.payload_type constructor)
               ~msg:"GADT constructors should expose payload_type")
    | None -> (
        match Syn.Cst.VariantConstructor.arguments constructor with
        | None ->
            "  | " ^ name
        | Some (Syn.Cst.ConstructorArguments.Tuple elements) ->
            "  | "
            ^ name
            ^ " of "
            ^ (elements
              |> List.map (fun element ->
                     render_core_type ~source ~context:`arrow_parameter element)
              |> String.concat " * ")
        | Some (Syn.Cst.ConstructorArguments.Record fields) ->
            "  | "
            ^ name
            ^ " of { \n"
            ^ render_inline_record_fields ~source fields
            ^ "\n    }")

  let render_poly_variant_kind = function
    | Syn.Cst.PolyVariantBound.Exact ->
        "[ "
    | Syn.Cst.PolyVariantBound.UpperBound { marker_token }
    | Syn.Cst.PolyVariantBound.LowerBound { marker_token } ->
        "[" ^ Source.source_of_token marker_token ^ " "

  let render_poly_variant_field ~source = function
    | Syn.Cst.RowField.Tag tag ->
        let name = "`" ^ Syn.Cst.PolyVariantTag.name tag in
        (match Syn.Cst.PolyVariantTag.payload_type tag with
        | None ->
            "  | " ^ name ^ " "
        | Some payload_type ->
            "  | " ^ name ^ " of " ^ render_core_type ~source payload_type ^ " ")
    | Syn.Cst.RowField.Inherit { type_; _ } ->
        "  | " ^ render_core_type ~source type_ ^ " "

  let render_type_definition ~source = function
    | Syn.Cst.TypeDefinition.Abstract ->
        None
    | Syn.Cst.TypeDefinition.Alias { manifest; _ } ->
        let manifest = render_core_type ~source manifest in
        Some
          (if Source.contains_substring manifest "\n" then
             "=\n" ^ indent_first_line 2 manifest
           else "= " ^ manifest)
    | Syn.Cst.TypeDefinition.Record { fields; _ } ->
        Some
          ("= {\n"
          ^ (fields |> List.map (render_record_field ~source ~indent:2 ~always_terminate:true)
            |> String.concat "\n")
          ^ "\n}")
    | Syn.Cst.TypeDefinition.Variant { constructors; _ } ->
        Some ("=\n" ^ (constructors |> List.map (render_variant_constructor ~source) |> String.concat "\n"))
    | Syn.Cst.TypeDefinition.PolyVariant { kind; fields; _ } ->
        Some
          ("= "
          ^ render_poly_variant_kind kind
          ^ "\n"
          ^ (fields |> List.map (render_poly_variant_field ~source) |> String.concat "\n")
          ^ "\n]")
    | Syn.Cst.TypeDefinition.FirstClassModule { module_type; _ } ->
        Some ("= " ^ source_of_module_type ~source module_type)
    | Syn.Cst.TypeDefinition.Object { syntax_node; _ }
    | Syn.Cst.TypeDefinition.Extensible { syntax_node } ->
        Some ("= " ^ Source.source_of_node_from_source source syntax_node)

  let line_start_before text index =
    let rec loop cursor =
      if cursor <= 0 then
        0
      else if text.[cursor - 1] = '\n' then
        cursor
      else
        loop (cursor - 1)
    in
    loop index

  let line_end_after text index =
    let rec loop cursor =
      if cursor >= String.length text then
        cursor
      else if text.[cursor] = '\n' then
        cursor
      else
        loop (cursor + 1)
    in
    loop index

  let line_prefix_before text index =
    let line_start = line_start_before text index in
    String.sub text line_start (index - line_start) |> String.trim

  let type_declaration_keyword_prefix ~source declaration =
    let span = Syn.Ceibo.Red.SyntaxNode.span (Syn.Cst.TypeDeclaration.syntax_node declaration) in
    let prefix = line_prefix_before source span.start in
    if String.starts_with ~prefix:"type " prefix then
      "type "
    else if String.starts_with ~prefix:"and " prefix then
      ""
    else
      "type "

  let source_of_type_declaration ~source declaration =
    let text =
      Source.source_of_node_from_source source (Syn.Cst.TypeDeclaration.syntax_node declaration)
    in
    let prefix = type_declaration_keyword_prefix ~source declaration in
    if prefix = "" || String.starts_with ~prefix:"type " text || String.starts_with ~prefix:"and " text
    then
      text
    else
      prefix ^ text

  let normalize_recursive_type_group text =
    let rec loop acc previous_was_blank = function
      | [] ->
          List.rev acc |> String.concat "\n"
      | line :: rest ->
          let trimmed = String.trim line in
          if String.starts_with ~prefix:"and " trimmed && not previous_was_blank then
            loop (line :: "" :: acc) false rest
          else
            loop (line :: acc) (trimmed = "") rest
    in
    text |> String.split_on_char '\n' |> loop [] false

  let source_of_recursive_type_group ~source declaration =
    let marker =
      "type " ^ Source.source_of_ident (Syn.Cst.TypeDeclaration.type_name declaration) ^ " ="
    in
    match find_substring_index source marker with
    | None ->
        None
    | Some start ->
        let source_length = String.length source in
        let rec skip_blank_lines index =
          if index >= source_length then
            index
          else
            match source.[index] with
            | '\n' | '\r' ->
                skip_blank_lines (index + 1)
            | _ ->
                index
        in
        let rec find_blank_run index =
          if index + 1 >= source_length then
            None
          else if source.[index] = '\n' && source.[index + 1] = '\n' then
            Some index
          else
            find_blank_run (index + 1)
        in
        let rec find_group_end cursor =
          match find_blank_run cursor with
          | None ->
              source_length
          | Some blank_start ->
              let next_line_start = skip_blank_lines blank_start in
              if next_line_start >= source_length then
                blank_start
              else
                let next_line_end = line_end_after source next_line_start in
                let next_line =
                  String.sub source next_line_start (next_line_end - next_line_start) |> String.trim
                in
                if String.starts_with ~prefix:"and " next_line then
                  find_group_end next_line_end
                else
                  blank_start
        in
        let end_ = find_group_end start in
        let text = String.sub source start (end_ - start) in
        if Source.contains_substring text "\nand " || Source.contains_substring text "\n\nand " then
          Some (normalize_recursive_type_group text, end_)
        else
          None

  let render_type_declaration ~source declaration =
    let declaration_source = source_of_type_declaration ~source declaration in
    if Syn.Cst.TypeDeclaration.is_private declaration
       || Syn.Cst.TypeDeclaration.is_destructive_substitution declaration
       || Source.contains_substring declaration_source "\nand "
    then
      None
    else
      let keyword = type_declaration_keyword_prefix ~source declaration in
      let constraints = render_type_constraints ~source (Syn.Cst.TypeDeclaration.constraints declaration) in
      let type_definition = Syn.Cst.TypeDeclaration.type_definition declaration in
      let finalize rendered =
        if
          (match type_definition with
          | Syn.Cst.TypeDefinition.Variant _ ->
              count_substring_occurrences rendered " = " > 1
          | _ ->
              false)
        then
          None
        else
          Some rendered
      in
      match render_type_definition ~source type_definition with
      | None ->
          finalize
            (keyword
            ^ render_type_parameters ~source (Syn.Cst.TypeDeclaration.type_params declaration)
            ^ Source.source_of_ident (Syn.Cst.TypeDeclaration.type_name declaration)
            ^ (if constraints = "" then "" else " " ^ constraints))
      | Some definition ->
          finalize
            (keyword
            ^ render_type_parameters ~source (Syn.Cst.TypeDeclaration.type_params declaration)
            ^ Source.source_of_ident (Syn.Cst.TypeDeclaration.type_name declaration)
            ^ " "
            ^ definition
            ^ (if constraints = "" then "" else " " ^ constraints))

  let render_external_declaration ~source declaration =
    let ({ Syn.Cst.name_token; type_; primitive_name_tokens; _ } :
          Syn.Cst.external_declaration) =
      declaration
    in
    let primitives =
      primitive_name_tokens |> List.map Source.source_of_token |> String.concat " "
    in
    "external "
    ^ Source.source_of_token name_token
    ^ " : "
    ^ render_core_type ~source type_
    ^ " = "
    ^ primitives

  let verbatim_structure_item_from_text text =
    let body, trailing_layout = Source.split_trailing_comment_block text in
    {
      doc = Doc.text body;
      preserves_layout = true;
      trailing_layout;
      consumed_end = None;
      start_override = None;
    }

  let verbatim_structure_item ~source item =
    let node = Syn.Cst.StructureItem.syntax_node item in
    verbatim_structure_item_from_text (Source.source_of_node_from_source source node)

  let render_structure_item ~source ~allow_rewrite = function
    | Syn.Cst.StructureItem.LetBinding binding ->
        if Source.syntax_node_has_comment_like_trivia binding.syntax_node then
          Some
            (verbatim_structure_item_from_text
               (Source.source_of_node_from_source source binding.syntax_node))
        else (
          match Expression.render_let_binding binding with
          | Some rendered ->
              let original = Source.source_of_node_from_source source binding.syntax_node in
              let rendered_text = Printer.to_string rendered in
              if allow_rewrite || rendered_text = original then
                Some
                  {
                    doc = rendered;
                    preserves_layout = false;
                    trailing_layout = "";
                    consumed_end = None;
                    start_override = None;
                  }
              else
                Some (verbatim_structure_item_from_text original)
          | None ->
              Some
                (verbatim_structure_item_from_text
                   (Source.source_of_node_from_source source binding.syntax_node)))
    | Syn.Cst.StructureItem.TypeDeclaration declaration ->
        (match source_of_recursive_type_group ~source declaration with
        | Some (group, consumed_end) ->
            let rendered = verbatim_structure_item_from_text group in
            Some { rendered with consumed_end = Some consumed_end; start_override = None }
        | None ->
            if
              Source.syntax_node_has_comment_like_trivia
                (Syn.Cst.TypeDeclaration.syntax_node declaration)
            then
              Some
                (verbatim_structure_item_from_text (source_of_type_declaration ~source declaration))
            else
              match render_type_declaration ~source declaration with
              | Some rendered ->
                  Some
                    {
                      doc = Doc.text rendered;
                      preserves_layout = false;
                      trailing_layout = "";
                      consumed_end = None;
                      start_override = None;
                    }
              | None ->
                  Some
                    (verbatim_structure_item_from_text
                       (source_of_type_declaration ~source declaration)))
    | Syn.Cst.StructureItem.ExternalDeclaration declaration ->
        if Source.syntax_node_has_comment_like_trivia declaration.syntax_node then
          Some
            (verbatim_structure_item_from_text
               (Source.source_of_node_from_source source declaration.syntax_node))
        else
          Some
            {
              doc = Doc.text (render_external_declaration ~source declaration);
              preserves_layout = false;
              trailing_layout = "";
              consumed_end = None;
              start_override = None;
            }
    | item ->
        Some (verbatim_structure_item ~source item)

  let render_source_file ~source (source_file : Syn.Cst.source_file) =
    match source_file with
    | Syn.Cst.Implementation { syntax_node; items } ->
        let file_span = Syn.Ceibo.Red.SyntaxNode.span syntax_node in
        let allow_rewrite =
          not
            (List.exists
               (function
                 | Syn.Cst.StructureItem.LetBinding _ ->
                     false
                 | _ ->
                     true)
               items)
        in
        let rec render_items acc previous_end previous_preserves_layout previous_trailing_layout
            is_first_item = function
          | [] ->
              let trailing_source =
                previous_trailing_layout
                ^ Source.source_between source ~start:previous_end ~end_:file_span.end_
              in
              let acc =
                if Source.is_whitespace_only trailing_source then
                  acc
                else Doc.text trailing_source :: acc
              in
              Some (List.rev acc |> Doc.concat)
          | item :: rest ->
              let item_node = Syn.Cst.StructureItem.syntax_node item in
              let item_span = Syn.Ceibo.Red.SyntaxNode.span item_node in
              (match render_structure_item ~source ~allow_rewrite item with
              | Some rendered ->
                  let item_start =
                    match rendered.start_override with
                    | Some start ->
                        start
                    | None ->
                        item_span.start
                  in
                  if item_start < previous_end then
                    render_items
                      acc
                      previous_end
                      previous_preserves_layout
                      previous_trailing_layout
                      is_first_item
                      rest
                  else
                    let interstitial =
                      previous_trailing_layout
                      ^
                      Source.source_between source ~start:previous_end ~end_:item_start
                    in
                    let preserve_interstitial =
                      is_first_item
                      || Source.contains_comment_like_text interstitial
                      || (rendered.preserves_layout && previous_preserves_layout)
                    in
                    let acc =
                      if is_first_item then
                        if interstitial = "" then
                          acc
                        else if Source.is_whitespace_only interstitial then
                          acc
                        else
                          Doc.text interstitial :: acc
                      else if preserve_interstitial then
                        if interstitial = "" then
                          acc
                        else if
                          Source.contains_comment_like_text interstitial
                          && (not rendered.preserves_layout)
                        then
                          Doc.text (Source.trim_trailing_layout_whitespace interstitial ^ "\n")
                          :: acc
                        else
                          Doc.text interstitial :: acc
                      else if Source.is_whitespace_only interstitial then
                        Doc.concat [ Doc.line; Doc.line ] :: acc
                      else
                        let layout = leading_layout_whitespace interstitial in
                        if layout = "" then acc else Doc.text layout :: acc
                    in
                    render_items
                      (rendered.doc :: acc)
                      (match rendered.consumed_end with
                      | Some consumed_end ->
                          consumed_end
                      | None ->
                          item_span.end_)
                      rendered.preserves_layout
                      rendered.trailing_layout
                      false
                      rest
              | None ->
                  None)
        in
        render_items [] file_span.start true "" true items
    | Syn.Cst.Interface _ -> None
end

let source_file = Structure.render_source_file
