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

  let strip_digit_separators text =
    let buffer = IO.Buffer.create (String.length text) in
    let rec loop index =
      if index >= String.length text then
        IO.Buffer.contents buffer
      else (
        let char = text.[index] in
        if not (char = '_') then
          IO.Buffer.add_char buffer char;
        loop (index + 1))
    in
    loop 0

  let group_digits_from_left ~group_size digits =
    let digits = strip_digit_separators digits in
    let digits_length = String.length digits in
    if digits_length <= group_size then
      digits
    else
      let buffer = IO.Buffer.create (digits_length + (digits_length / group_size)) in
      let rec loop index =
        if index >= digits_length then
          IO.Buffer.contents buffer
        else (
          if index > 0 then
            IO.Buffer.add_char buffer '_';
          let chunk_size = Int.min group_size (digits_length - index) in
          IO.Buffer.add_string buffer (String.sub digits index chunk_size);
          loop (index + chunk_size))
      in
      loop 0

  let group_digits_from_right ~group_size digits =
    let digits = strip_digit_separators digits in
    let digits_length = String.length digits in
    if digits_length <= group_size then
      digits
    else
      let first_group_size =
        match digits_length mod group_size with
        | 0 -> group_size
        | remainder -> remainder
      in
      let buffer = IO.Buffer.create (digits_length + (digits_length / group_size)) in
      IO.Buffer.add_string buffer (String.sub digits 0 first_group_size);
      let rec loop index =
        if index >= digits_length then
          IO.Buffer.contents buffer
        else (
          IO.Buffer.add_char buffer '_';
          IO.Buffer.add_string buffer (String.sub digits index group_size);
          loop (index + group_size))
      in
      loop first_group_size

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
      | Syn.Cst.Decimal ->
          group_digits_from_right ~group_size:3 literal.digits
      | Syn.Cst.Octal ->
          group_digits_from_right ~group_size:3 literal.digits
      | Syn.Cst.Binary ->
          group_digits_from_right ~group_size:4 literal.digits
      | Syn.Cst.Hexadecimal ->
          literal.digits |> String.lowercase_ascii |> group_digits_from_right ~group_size:4
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
    let integral_digits = group_digits_from_right ~group_size:3 literal.integral_digits in
    let fractional_digits = group_digits_from_left ~group_size:3 literal.fractional_digits in
    integral_digits ^ "." ^ fractional_digits ^ exponent ^ suffix

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

  let parameter_requires_explicit_fun_syntax parameter =
    match parameter with
    | Syn.Cst.Parameter.Positional _ ->
        let text = source_of_parameter parameter |> String.trim in
        String.starts_with ~prefix:"(" text && String.contains text ","
    | Syn.Cst.Parameter.Labeled _
    | Syn.Cst.Parameter.Optional _
    | Syn.Cst.Parameter.LocallyAbstract _ ->
        false

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

  let rec trim_leading_doc_layout = function
    | Doc.Empty | Doc.Space | Doc.Line | Doc.Break _ ->
        Doc.empty
    | Doc.Text text when String.trim text = "" ->
        Doc.empty
    | Doc.Concat [] ->
        Doc.empty
    | Doc.Concat (doc :: rest) -> (
        match trim_leading_doc_layout doc with
        | Doc.Empty ->
            trim_leading_doc_layout (Doc.concat rest)
        | trimmed ->
            Doc.concat (trimmed :: rest))
    | Doc.Group doc ->
        Doc.group (trim_leading_doc_layout doc)
    | Doc.Indent (spaces, doc) ->
        Doc.indent spaces (trim_leading_doc_layout doc)
    | doc ->
        doc

  let rec trim_inline_doc = function
    | Doc.Text text ->
        Doc.text (String.trim text)
    | Doc.Group doc ->
        Doc.group (trim_inline_doc doc)
    | Doc.Indent (spaces, doc) ->
        Doc.indent spaces (trim_inline_doc doc)
    | doc ->
        doc

  let wrap_breaking_value ~indent value =
    Doc.group
      (Doc.concat
         [
           Doc.break ~flat:"" ();
           Doc.indent indent value;
         ])

  let normalize_comment_preserved_apply_source text =
    match String.split_on_char '\n' (String.trim text) with
    | [] ->
        ""
    | first :: rest ->
        String.concat "\n"
          (String.trim first
          :: List.map
               (fun line ->
                 let trimmed = String.trim line in
                 if trimmed = "" then
                   ""
                 else
                   "  " ^ trimmed)
               rest)

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
          if rhs = "" then
            lhs ^ " ="
          else
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
          Doc.text (indent_string indent ^ "function");
          Doc.line;
          (cases
          |> List.mapi (fun _ case ->
                 Doc.text (indent_string indent ^ "| " ^ case))
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
    | Syn.Cst.Expression.Parenthesized
        { syntax_node; inner = (Match _ | If _ | Try _ | Let _); _ } ->
        Doc.text (source_of_syntax_node syntax_node)
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
    | Syn.Cst.Expression.Try try_ ->
        render_try_expression ~indent try_
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
    let wrapped_multiline_body =
      match case.body with
      | Syn.Cst.Expression.Match _ ->
          Doc.concat
            [
              Doc.lparen;
              Doc.line;
              body;
              Doc.line;
              Doc.indent case_indent Doc.rparen;
            ]
      | _ ->
          body
    in
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
                       (Doc.concat [ Doc.bar; Doc.space; Doc.text alternative ]))
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
                      Doc.space;
                      wrapped_multiline_body;
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
                Doc.space;
                wrapped_multiline_body;
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
            Doc.indent indent (Doc.text ("fun " ^ rendered_parameters ^ " ->"));
            Doc.line;
            render_match_expression ~indent:(indent + 2) ~keyword_trailing_space:true match_;
          ]
    | Syn.Cst.Expression.Try try_ ->
        Doc.concat
          [
            Doc.indent indent (Doc.text ("fun " ^ rendered_parameters ^ " ->"));
            Doc.line;
            render_try_expression ~indent:(indent + 2) try_;
          ]
    | expression ->
        let rendered_body =
          match expression with
          | Syn.Cst.Expression.Sequence _ ->
              render ~indent:0 expression
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
        if prefers_multiline || Doc.is_multiline rendered_body then
          Doc.concat
            [
              Doc.indent indent (Doc.text ("fun " ^ rendered_parameters ^ " ->"));
              Doc.line;
              Doc.indent 2 rendered_body;
            ]
        else
          Doc.concat [ Doc.text ("fun " ^ rendered_parameters ^ " -> "); rendered_body ]

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
    let source = source_of_syntax_node syntax_node in
    if List.length attributes > 0 then
      Doc.text (indent_first_line indent source)
    else if contains_substring source "\n" then
      Doc.text (indent_first_line indent source)
    else render_fun_body ~indent parameters body

  and render_function_expression ~indent
      (({ syntax_node; cases; _ } as function_) : Syn.Cst.function_expression) =
    let source = source_of_syntax_node syntax_node in
    if contains_substring source "\n" then
      Doc.text (indent_first_line indent source)
    else
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
            let body =
              if Doc.is_multiline rendered_body then
                rendered_body
              else
                Doc.indent 2 rendered_body
            in
            Doc.concat
              [
                Doc.text "fun ";
                Doc.line;
                rendered_pattern;
                Doc.text " -> ";
                Doc.line;
                body;
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
              Doc.indent indent (Doc.text ("fun " ^ parameter_name ^ " ->"));
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
            let base_indent = if indent = 0 then 2 else indent in
            if requires_block_layout then
              (base_indent, base_indent)
            else
              (base_indent, base_indent)
          in
          Doc.concat
            [
              Doc.indent indent (Doc.text "function");
              Doc.line;
              (multiline_cases ~case_indent ~or_indent cases |> Doc.join Doc.line);
            ]

  and render_match_expression ~indent ~keyword_trailing_space
      ({ syntax_node; scrutinee; cases; _ } : Syn.Cst.match_expression) =
    let source = source_of_syntax_node syntax_node in
    if
      Source.syntax_node_has_comment_like_trivia syntax_node
      || contains_substring source "\n"
      || contains_substring source " when "
    then
      Doc.text (indent_first_line indent source)
    else
      Doc.concat
        [
          Doc.indent indent (Doc.concat [ Doc.text "match"; Doc.space ]);
          render ~indent:0 scrutinee;
          Doc.space;
          Doc.text "with";
          Doc.line;
          (multiline_cases ~case_indent:indent ~or_indent:indent cases |> Doc.join Doc.line);
        ]

  and render_if_expression ?(force_multiline = false) ~indent
      ({ syntax_node; condition; then_branch; else_branch; _ } : Syn.Cst.if_expression) =
    let source = source_of_syntax_node syntax_node in
    if
      Source.syntax_node_has_comment_like_trivia syntax_node
      || contains_substring source "\n"
    then
      Doc.text (indent_first_line indent source)
    else
      let rendered_condition = render ~indent:0 condition in
      let rendered_then =
        match then_branch with
        | Syn.Cst.Expression.If if_ ->
            render_if_expression ~force_multiline:true ~indent:(indent + 2) if_
        | _ ->
            render ~indent:(indent + 2) then_branch
      in
      match else_branch with
      | None ->
          Doc.concat [ Doc.text "if "; rendered_condition; Doc.text " then "; rendered_then ]
      | Some else_branch ->
          let rendered_else =
            match else_branch with
            | Syn.Cst.Expression.If if_ ->
                render_if_expression ~force_multiline:true ~indent:(indent + 2) if_
            | _ ->
                render ~indent:(indent + 2) else_branch
          in
          let indented_then =
            if Doc.is_multiline rendered_then then
              rendered_then
            else
              Doc.indent (indent + 2) rendered_then
          in
          let indented_else =
            if Doc.is_multiline rendered_else then
              rendered_else
            else
              Doc.indent (indent + 2) rendered_else
          in
          let prefers_multiline =
            force_multiline
            || Doc.is_multiline rendered_then
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

  and render_try_expression ~indent
      (({ syntax_node; body; cases; _ } : Syn.Cst.try_expression) as _try) =
    let source = source_of_syntax_node syntax_node in
    if contains_substring source "\n" then
      Doc.text (indent_first_line indent source)
    else
      let rendered_body = render ~indent:(indent + 2) body in
      let indented_body =
        if Doc.is_multiline rendered_body then
          rendered_body
        else
          Doc.indent (indent + 2) rendered_body
      in
      let prefers_multiline =
        Doc.is_multiline rendered_body
        ||
        match body with
        | Syn.Cst.Expression.Try _
        | Syn.Cst.Expression.Match _
        | Syn.Cst.Expression.If _
        | Syn.Cst.Expression.Sequence _ ->
            true
        | _ ->
            false
      in
      if prefers_multiline then
        Doc.concat
          [
            Doc.indent indent (Doc.text "try");
            Doc.line;
            indented_body;
            Doc.line;
            Doc.indent indent (Doc.text "with");
            Doc.line;
            (multiline_cases ~case_indent:indent ~or_indent:indent cases |> Doc.join Doc.line);
          ]
      else
        Doc.concat
          [
            Doc.indent indent (Doc.concat [ Doc.text "try"; Doc.space ]);
            render ~indent:0 body;
            Doc.space;
            Doc.text "with";
            Doc.line;
            (multiline_cases ~case_indent:indent ~or_indent:indent cases |> Doc.join Doc.line);
          ]

  and render_apply_expression ~indent
      ({ syntax_node; callee; argument; _ } : Syn.Cst.apply_expression) =
    let rec collect_apply_parts args = function
      | Syn.Cst.Expression.Apply { callee; argument = Syn.Cst.Positional argument; _ } ->
          collect_apply_parts (argument :: args) callee
      | expression ->
          (expression, args)
    in
    let render_apply_atom = function
      | Syn.Cst.Expression.Parenthesized { inner = Syn.Cst.Expression.Apply apply; _ } ->
          render_apply_expression ~indent:0 apply
      | Syn.Cst.Expression.Parenthesized { inner; _ } ->
          Doc.concat [ Doc.lparen; render ~indent:0 inner; Doc.rparen ]
      | expression ->
          render ~indent:0 expression
    in
    if Source.syntax_node_has_comment_like_trivia syntax_node then
      Doc.indent indent
        (Doc.text
           (source_of_syntax_node syntax_node |> normalize_comment_preserved_apply_source))
    else
      match argument with
    | Syn.Cst.Positional _ ->
        let head, arguments =
          collect_apply_parts [] (Syn.Cst.Expression.Apply { syntax_node; callee; argument; attributes = [] })
        in
        let rendered_head = render_apply_atom head |> trim_inline_doc in
        let rendered_arguments =
          arguments |> List.map (fun argument -> render_apply_atom argument |> trim_inline_doc)
        in
        if
          not (Doc.is_multiline rendered_head)
          && List.for_all (fun argument -> not (Doc.is_multiline argument)) rendered_arguments
        then
          Doc.words (rendered_head :: rendered_arguments)
        else
          Doc.concat
            (Doc.indent indent rendered_head
            :: List.map
                 (fun argument ->
                   Doc.concat [ Doc.line; Doc.indent (indent + 2) argument ])
                 rendered_arguments)
    | Syn.Cst.Labeled _ | Syn.Cst.Optional _ ->
        Doc.text (String.trim (source_of_syntax_node syntax_node))

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
           let rendered = render ~indent expression in
           let suffix = if index < List.length expressions - 1 then Doc.text ";" else Doc.empty in
           if Doc.is_multiline rendered then
             Doc.concat [ rendered; suffix ]
           else
             Doc.concat [ Doc.indent indent rendered; suffix ])
    |> Doc.join Doc.line

  let render_parameterized_let_binding ~keyword ~pattern ~parameters ~value =
    let rendered_parameters = parameters |> List.map source_of_parameter |> String.concat " " in
    let should_lower_to_fun =
      List.exists parameter_requires_explicit_fun_syntax parameters
    in
    if should_lower_to_fun then
      let rendered_value =
        render_explicit_fun_expression ~prefix_columns:0 ~indent:0 parameters value
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
                let value = render_function_expression ~indent:2 function_ in
                if function_requires_block_layout function_ || List.length function_.cases > 1 then
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
                let value = render ~indent:2 binding.value in
                render_binding ~indent:0 ~keyword pattern value
              else
                let value = render ~indent:0 binding.value in
                Doc.concat [ Doc.text (keyword ^ pattern); Doc.space; Doc.equal; Doc.space; value ]
          | Syn.Cst.Expression.Apply apply ->
              let value = render_apply_expression ~indent:2 apply in
              if Doc.is_multiline value then
                render_binding ~indent:0 ~keyword pattern value
              else
                Doc.concat [ Doc.text (keyword ^ pattern); Doc.space; Doc.equal; Doc.space; value ]
          | _ when
              let source =
                source_of_syntax_node (Syn.Cst.Expression.syntax_node binding.value) |> String.trim
              in
              String.starts_with ~prefix:"begin" source && contains_substring source "\n" ->
              let value =
                Doc.text (source_of_syntax_node (Syn.Cst.Expression.syntax_node binding.value))
              in
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
          (match (path, arguments) with
          | ( "t",
              [ Syn.Cst.CoreType.Parenthesized
                  { inner = Syn.Cst.CoreType.PolyVariant { kind; fields; _ }; _ } ] )
          | ("t", [ Syn.Cst.CoreType.PolyVariant { kind; fields; _ } ]) ->
              render_core_poly_variant_kind kind
              ^ "\n"
              ^ (fields
                |> List.map (fun field -> "  " ^ render_core_poly_variant_field ~source field)
                |> String.concat "\n")
              ^ "\n]"
          | (_, []) ->
              path
          | (_, [ Syn.Cst.CoreType.Tuple { elements; _ } ]) ->
              "("
              ^ (elements
                |> List.map (fun element -> render_core_type ~source element)
                |> String.concat ", ")
              ^ ") "
              ^ path
          | (_, [ argument ]) ->
              render_type_argument ~source argument ^ " " ^ path
          | (_, arguments) ->
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
    if
      prefix = ""
      || String.starts_with ~prefix:"type " text
      || String.starts_with ~prefix:"and " text
      ||
      (Source.contains_comment_like_text text
      && not (find_substring_index text "type " = None))
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

  let source_of_module_expression ~source module_expression =
    Source.source_of_node_from_source source (Syn.Cst.ModuleExpression.syntax_node module_expression)
    |> String.trim

  let parse_fragment ~filename source_text =
    let result = Syn.parse ~filename:(Path.v filename) source_text in
    match Syn.build_cst result with
    | Error _ ->
        None
    | Ok source_file ->
        Some (result.source, source_file)

  let strip_wrapping_keyword_block ~opening ~closing text =
    let trimmed = String.trim text in
    let opening_length = String.length opening in
    let closing_length = String.length closing in
    if
      String.length trimmed >= opening_length + closing_length
      && String.starts_with ~prefix:opening trimmed
      && String.ends_with ~suffix:closing trimmed
    then
      Some
        (String.sub trimmed opening_length
           (String.length trimmed - opening_length - closing_length)
        |> String.trim)
    else
      None

  let remove_blank_lines text =
    text |> String.split_on_char '\n'
    |> List.filter (fun line -> not (String.trim line = ""))
    |> String.concat "\n"

  let indent_following_lines spaces text =
    match String.split_on_char '\n' text with
    | [] | [ _ ] ->
        text
    | first :: rest ->
        first
        :: List.map
             (fun line -> if line = "" then line else indent_string spaces ^ line)
             rest
        |> String.concat "\n"

  let split_recursive_group text =
    let rec loop current acc = function
      | [] ->
          List.rev
            (if current = [] then
               acc
             else
               String.concat "\n" (List.rev current) :: acc)
      | line :: rest ->
          if String.starts_with ~prefix:"and " (String.trim line) && not (current = []) then
            loop [ line ] (String.concat "\n" (List.rev current) :: acc) rest
          else
            loop (line :: current) acc rest
    in
    text |> String.split_on_char '\n' |> loop [] []

  let normalize_top_level_trailing_comment_layout layout =
    let trimmed = String.trim layout in
    if trimmed = "" then
      ""
    else
      "\n\n" ^ trimmed

  let keyword_start_override text keyword node =
    match find_substring_index text keyword with
    | None ->
        None
    | Some index ->
        let span = Syn.Ceibo.Red.SyntaxNode.span node in
        Some (span.start + index)

  let strip_leading_comment_blocks text =
    let length = String.length text in
    let rec skip_layout index =
      if index >= length then
        index
      else
        match text.[index] with
        | ' ' | '\t' | '\n' | '\r' ->
            skip_layout (index + 1)
        | _ ->
            index
    in
    let rec skip_comment index depth =
      if index + 1 >= length then
        length
      else if text.[index] = '(' && text.[index + 1] = '*' then
        skip_comment (index + 2) (depth + 1)
      else if text.[index] = '*' && text.[index + 1] = ')' then
        if depth = 1 then
          index + 2
        else
          skip_comment (index + 2) (depth - 1)
      else
        skip_comment (index + 1) depth
    in
    let rec loop index =
      let index = skip_layout index in
      if index + 1 < length && text.[index] = '(' && text.[index + 1] = '*' then
        loop (skip_comment (index + 2) 1)
      else if index <= 0 then
        text
      else
        String.sub text index (length - index)
    in
    loop 0

  let contains_comment_after_keyword text keyword =
    let trimmed = String.trim text in
    match find_substring_index trimmed keyword with
    | None ->
        Source.contains_comment_like_text trimmed
    | Some index ->
        let keyword_text =
          String.sub trimmed index (String.length trimmed - index)
        in
        let start = String.length keyword in
        if start >= String.length keyword_text then
          false
        else
          String.sub keyword_text start (String.length keyword_text - start)
          |> Source.contains_comment_like_text

  let render_value_declaration ~source ({ name_token; type_; _ } : Syn.Cst.value_declaration) =
    "val " ^ Source.source_of_token name_token ^ " : " ^ render_core_type ~source type_

  let rec render_simple_signature_item ~source = function
    | Syn.Cst.SignatureItem.TypeDeclaration declaration ->
        render_type_declaration ~source declaration
    | Syn.Cst.SignatureItem.ValueDeclaration declaration ->
        Some (render_value_declaration ~source declaration)
    | Syn.Cst.SignatureItem.ModuleDeclaration declaration ->
        render_module_declaration ~source ~keyword:"module " declaration
    | Syn.Cst.SignatureItem.RecursiveModuleDeclaration declaration ->
        render_recursive_module_declaration ~source declaration
    | Syn.Cst.SignatureItem.ModuleTypeDeclaration declaration ->
        render_module_type_declaration ~source declaration
    | Syn.Cst.SignatureItem.OpenStatement statement ->
        render_open_statement ~source statement
    | Syn.Cst.SignatureItem.IncludeStatement statement ->
        render_include_statement ~source statement
    | item ->
        Some
          (Source.source_of_node_from_source source (Syn.Cst.SignatureItem.syntax_node item)
          |> String.trim)

  and render_simple_structure_item ~source = function
    | Syn.Cst.StructureItem.LetBinding binding ->
        let original_source = Source.source_of_node_from_source source binding.syntax_node in
        if Source.contains_substring original_source "\n" then
          Some
            (dedent_multiline_block original_source |> indent_following_lines 2 |> String.trim)
        else (
          match Expression.render_let_binding binding with
          | Some rendered ->
              Some (Printer.to_string rendered)
          | None ->
              Some (String.trim original_source))
    | Syn.Cst.StructureItem.TypeDeclaration declaration ->
        render_type_declaration ~source declaration
    | Syn.Cst.StructureItem.ExternalDeclaration declaration ->
        Some (render_external_declaration ~source declaration)
    | Syn.Cst.StructureItem.ModuleDeclaration declaration ->
        render_module_declaration ~source ~keyword:"module " declaration
    | Syn.Cst.StructureItem.RecursiveModuleDeclaration declaration ->
        render_recursive_module_declaration ~source declaration
    | Syn.Cst.StructureItem.ModuleTypeDeclaration declaration ->
        render_module_type_declaration ~source declaration
    | Syn.Cst.StructureItem.OpenStatement statement ->
        render_open_statement ~source statement
    | Syn.Cst.StructureItem.IncludeStatement statement ->
        render_include_statement ~source statement
    | item ->
        Some
          (Source.source_of_node_from_source source (Syn.Cst.StructureItem.syntax_node item)
          |> String.trim)

  and render_simple_signature_items ~source items =
    let rec loop acc = function
      | [] ->
          Some (List.rev acc |> String.concat "\n\n")
      | item :: rest -> (
          match render_simple_signature_item ~source item with
          | Some rendered ->
              loop (rendered :: acc) rest
          | None ->
              None)
    in
    loop [] items

  and render_simple_structure_items ~source items ~separator =
    let rec loop acc = function
      | [] ->
          Some (List.rev acc |> String.concat separator)
      | item :: rest -> (
          match render_simple_structure_item ~source item with
          | Some rendered ->
              loop (rendered :: acc) rest
          | None ->
              None)
    in
    loop [] items

  and render_signature_fragment source_text =
    match parse_fragment ~filename:"inline.mli" source_text with
    | None ->
        None
    | Some (fragment_source, Syn.Cst.Interface { items; _ }) ->
        render_simple_signature_items ~source:fragment_source items
    | Some _ ->
        None

  and render_structure_fragment ~separator source_text =
    match parse_fragment ~filename:"inline.ml" source_text with
    | None ->
        None
    | Some (fragment_source, Syn.Cst.Implementation { items; _ }) ->
        render_simple_structure_items ~source:fragment_source items ~separator
    | Some _ ->
        None

  and render_module_type ~source = function
    | Syn.Cst.ModuleType.Path path ->
        Some (Source.source_of_ident path)
    | Syn.Cst.ModuleType.Signature { syntax_node; _ } ->
        let raw = Source.source_of_node_from_source source syntax_node |> String.trim in
        let body =
          match strip_wrapping_keyword_block ~opening:"sig" ~closing:"end" raw with
          | Some inner -> (
              match render_signature_fragment inner with
              | Some rendered ->
                  rendered
              | None ->
                  inner)
          | None ->
              raw
        in
        Some ("sig\n" ^ indent_block 2 body ^ "\nend")
    | Syn.Cst.ModuleType.Parenthesized { inner; _ } ->
        render_module_type ~source inner |> Option.map (fun rendered -> "(" ^ rendered ^ ")")
    | module_type ->
        Some (Source.source_of_node_from_source source (Syn.Cst.ModuleType.syntax_node module_type) |> String.trim)

  and render_module_apply ~source module_expression =
    let rec collect arguments = function
      | Syn.Cst.ModuleExpression.Apply { callee; argument; _ } ->
          collect (argument :: arguments) callee
      | expression ->
          (expression, arguments)
    in
    let head, arguments = collect [] module_expression in
    match render_module_expression ~source head with
    | None ->
        None
    | Some rendered_head ->
        let rec render_arguments acc = function
          | [] ->
              Some (rendered_head ^ String.concat "" (List.rev acc))
          | argument :: rest -> (
              match render_module_expression ~source argument with
              | None ->
                  None
              | Some rendered ->
                  render_arguments ((" (" ^ rendered ^ ")") :: acc) rest)
        in
        render_arguments [] arguments

  and render_module_expression ~source = function
    | Syn.Cst.ModuleExpression.Path path ->
        Some (Source.source_of_ident path)
    | Syn.Cst.ModuleExpression.Structure { syntax_node; _ } ->
        let raw = Source.source_of_node_from_source source syntax_node |> String.trim in
        let body =
          match strip_wrapping_keyword_block ~opening:"struct" ~closing:"end" raw with
          | Some inner -> (
              match render_structure_fragment ~separator:"\n\n" inner with
              | Some rendered ->
                  rendered
              | None ->
                  inner)
          | None ->
              raw
        in
        Some ("struct\n" ^ indent_block 2 body ^ "\nend")
    | Syn.Cst.ModuleExpression.Apply _ as apply ->
        render_module_apply ~source apply
    | Syn.Cst.ModuleExpression.Constraint { module_expression; module_type; _ } -> (
        match (render_module_expression ~source module_expression, render_module_type ~source module_type) with
        | Some rendered_expression, Some rendered_type ->
            Some (rendered_expression ^ " : " ^ rendered_type)
        | _ ->
            None)
    | Syn.Cst.ModuleExpression.Parenthesized { inner; _ } ->
        render_module_expression ~source inner |> Option.map (fun rendered -> "(" ^ rendered ^ ")")
    | module_expression ->
        Some (source_of_module_expression ~source module_expression)

  and render_functor_parameter ~source ({ Syn.Cst.name_token; module_type; _ } : Syn.Cst.functor_parameter) =
    match render_module_type ~source module_type with
    | Some rendered_module_type ->
        Some
          ("(" ^ Source.source_of_token name_token ^ " : " ^ rendered_module_type ^ ")")
    | None ->
        None

  and render_module_declaration ~source ~keyword declaration =
    let name = Syn.Cst.ModuleDeclaration.name declaration in
    let rec render_parameters acc = function
      | [] ->
          Some (List.rev acc |> String.concat "")
      | parameter :: rest -> (
          match render_functor_parameter ~source parameter with
          | Some rendered ->
              render_parameters ((" " ^ rendered) :: acc) rest
          | None ->
              None)
    in
    match render_parameters [] (Syn.Cst.ModuleDeclaration.functor_parameters declaration) with
    | None ->
        None
    | Some parameters -> (
        let module_expression =
          match Syn.Cst.ModuleDeclaration.module_expression declaration with
          | Some (Syn.Cst.ModuleExpression.Constraint { module_expression; _ })
            when Option.is_some (Syn.Cst.ModuleDeclaration.module_type declaration) ->
              Some module_expression
          | module_expression ->
              module_expression
        in
        match
          ( Option.map (render_module_type ~source) (Syn.Cst.ModuleDeclaration.module_type declaration),
            Option.map (render_module_expression ~source) module_expression )
        with
        | Some None, _ | _, Some None ->
            None
        | module_type, module_expression ->
            let header = keyword ^ name ^ parameters in
            let header =
              match module_type with
              | Some (Some rendered_module_type) ->
                  header ^ " : " ^ rendered_module_type
              | Some None | None ->
                  header
            in
            Some
              (match module_expression with
              | Some (Some rendered_module_expression) ->
                  header ^ " = " ^ rendered_module_expression
              | Some None | None ->
                  header))

  and render_recursive_module_declaration ~source declaration =
    let declarations = Syn.Cst.RecursiveModuleDeclaration.declarations declaration in
    let rec loop index acc = function
      | [] ->
          Some (List.rev acc |> String.concat "\n\n")
      | module_declaration :: rest ->
          let keyword = if index = 0 then "module rec " else "and " in
          (match render_module_declaration ~source ~keyword module_declaration with
          | Some rendered ->
              loop (index + 1) (rendered :: acc) rest
          | None ->
              None)
    in
    loop 0 [] declarations

  and render_module_type_declaration ~source declaration =
    match Syn.Cst.ModuleTypeDeclaration.module_type declaration with
    | Some module_type ->
        render_module_type ~source module_type
        |> Option.map (fun rendered ->
               "module type "
               ^ Syn.Cst.ModuleTypeDeclaration.name declaration
               ^ " = "
               ^ rendered)
    | None ->
        Some ("module type " ^ Syn.Cst.ModuleTypeDeclaration.name declaration)

  and render_open_statement ~source statement =
    let keyword =
      if Syn.Cst.OpenStatement.has_bang statement then
        "open!"
      else
        "open"
    in
    match Syn.Cst.OpenStatement.target statement with
    | Syn.Cst.OpenStatement.Path path ->
        Some (keyword ^ " " ^ Source.source_of_ident path)
    | Syn.Cst.OpenStatement.ModuleExpression module_expression ->
        render_module_expression ~source module_expression
        |> Option.map (fun rendered -> keyword ^ " " ^ rendered)

  and render_include_statement ~source statement =
    match statement.Syn.Cst.target with
    | Syn.Cst.ModuleExpression module_expression ->
        render_module_expression ~source module_expression
        |> Option.map (fun rendered -> "include " ^ rendered)
    | Syn.Cst.ModuleType module_type ->
        render_module_type ~source module_type |> Option.map (fun rendered -> "include " ^ rendered)

  let render_recursive_type_group group_text =
    let declarations = split_recursive_group group_text in
    let rec loop index acc = function
      | [] ->
          Some (List.rev acc |> String.concat "\n\n")
      | declaration_text :: rest ->
          let normalized_source =
            if index = 0 then
              declaration_text
            else
              "type "
              ^ String.sub declaration_text 4 (String.length declaration_text - 4)
          in
          (match render_structure_fragment ~separator:"\n\n" normalized_source with
          | Some rendered ->
              let rendered =
                if index = 0 then
                  rendered
                else if String.starts_with ~prefix:"type " rendered then
                  "and " ^ String.sub rendered 5 (String.length rendered - 5)
                else
                  rendered
              in
              loop (index + 1) (rendered :: acc) rest
          | None ->
              None)
    in
    loop 0 [] declarations

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

  let structure_item_supports_mixed_rewrite = function
    | Syn.Cst.StructureItem.LetBinding _
    | Syn.Cst.StructureItem.TypeDeclaration _
    | Syn.Cst.StructureItem.ExternalDeclaration _
    | Syn.Cst.StructureItem.ModuleDeclaration _
    | Syn.Cst.StructureItem.RecursiveModuleDeclaration _
    | Syn.Cst.StructureItem.ModuleTypeDeclaration _
    | Syn.Cst.StructureItem.OpenStatement _
    | Syn.Cst.StructureItem.IncludeStatement _ ->
        true
    | _ ->
        false

  let render_structure_item ~source ~allow_rewrite = function
    | Syn.Cst.StructureItem.LetBinding binding -> (
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
            let rendered =
              if Source.contains_comment_like_text group then
                verbatim_structure_item_from_text group
              else
                match render_recursive_type_group group with
                | Some rendered ->
                    {
                      doc = Doc.text rendered;
                      preserves_layout = false;
                      trailing_layout = "";
                      consumed_end = Some consumed_end;
                      start_override = None;
                    }
                | None ->
                    let rendered = verbatim_structure_item_from_text group in
                    { rendered with consumed_end = Some consumed_end; start_override = None }
            in
            Some rendered
        | None ->
            let raw_declaration_source =
              Source.source_of_node_from_source source
                (Syn.Cst.TypeDeclaration.syntax_node declaration)
            in
            let declaration_source = source_of_type_declaration ~source declaration in
            let declaration_body, trailing_layout =
              Source.split_trailing_comment_block declaration_source
            in
            let trailing_layout =
              normalize_top_level_trailing_comment_layout trailing_layout
            in
            if contains_comment_after_keyword declaration_body "type " then
              Some
                {
                  (verbatim_structure_item_from_text declaration_body) with
                  trailing_layout;
                }
            else
              let normalized_source =
                let trimmed = String.trim declaration_body in
                match find_substring_index trimmed "type " with
                | Some index ->
                    String.sub trimmed index (String.length trimmed - index)
                | None ->
                    strip_leading_comment_blocks declaration_body |> String.trim
              in
              match render_structure_fragment ~separator:"\n\n" normalized_source with
              | Some rendered ->
                  Some
                    {
                      doc = Doc.text rendered;
                      preserves_layout = false;
                      trailing_layout;
                      consumed_end = None;
                      start_override =
                        keyword_start_override raw_declaration_source "type "
                          (Syn.Cst.TypeDeclaration.syntax_node declaration);
                    }
              | None -> (
                  match render_type_declaration ~source declaration with
                  | Some rendered ->
                      Some
                        {
                          doc = Doc.text rendered;
                          preserves_layout = false;
                          trailing_layout;
                          consumed_end = None;
                          start_override =
                            keyword_start_override raw_declaration_source "type "
                              (Syn.Cst.TypeDeclaration.syntax_node declaration);
                        }
                  | None ->
                      Some
                        {
                          (verbatim_structure_item_from_text declaration_body) with
                          trailing_layout;
                        }))
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
    | Syn.Cst.StructureItem.ModuleDeclaration declaration ->
        let raw_declaration_source =
          Source.source_of_node_from_source source (Syn.Cst.ModuleDeclaration.syntax_node declaration)
        in
        let declaration_source =
          raw_declaration_source
          |> String.trim
        in
        let declaration_body, trailing_layout =
          Source.split_trailing_comment_block declaration_source
        in
        let trailing_layout =
          normalize_top_level_trailing_comment_layout trailing_layout
        in
        if contains_comment_after_keyword declaration_body "module " then
          Some
            {
              (verbatim_structure_item_from_text declaration_body) with
              trailing_layout;
            }
        else
          render_module_declaration ~source ~keyword:"module " declaration
          |> Option.map (fun rendered ->
                 {
                   doc = Doc.text rendered;
                   preserves_layout = false;
                   trailing_layout;
                   consumed_end = None;
                   start_override =
                     keyword_start_override raw_declaration_source "module "
                       (Syn.Cst.ModuleDeclaration.syntax_node declaration);
                 })
    | Syn.Cst.StructureItem.RecursiveModuleDeclaration declaration ->
        let declaration_source =
          Source.source_of_node_from_source source
            (Syn.Cst.RecursiveModuleDeclaration.syntax_node declaration)
          |> String.trim
        in
        if Source.contains_comment_like_text declaration_source then
          Some (verbatim_structure_item_from_text declaration_source)
        else
          render_recursive_module_declaration ~source declaration
          |> Option.map (fun rendered ->
                 {
                   doc = Doc.text rendered;
                   preserves_layout = false;
                   trailing_layout = "";
                   consumed_end = None;
                   start_override = None;
                 })
    | Syn.Cst.StructureItem.ModuleTypeDeclaration declaration ->
        let raw_declaration_source =
          Source.source_of_node_from_source source
            (Syn.Cst.ModuleTypeDeclaration.syntax_node declaration)
        in
        let declaration_source =
          raw_declaration_source
          |> String.trim
        in
        let declaration_body, trailing_layout =
          Source.split_trailing_comment_block declaration_source
        in
        let trailing_layout =
          normalize_top_level_trailing_comment_layout trailing_layout
        in
        if contains_comment_after_keyword declaration_body "module type " then
          Some
            {
              (verbatim_structure_item_from_text declaration_body) with
              trailing_layout;
            }
        else
          render_module_type_declaration ~source declaration
          |> Option.map (fun rendered ->
                 {
                   doc = Doc.text rendered;
                   preserves_layout = false;
                   trailing_layout;
                   consumed_end = None;
                   start_override =
                     keyword_start_override raw_declaration_source "module type "
                       (Syn.Cst.ModuleTypeDeclaration.syntax_node declaration);
                 })
    | Syn.Cst.StructureItem.OpenStatement statement ->
        let statement_source =
          Source.source_of_node_from_source source (Syn.Cst.OpenStatement.syntax_node statement)
        in
        let _, trailing_layout =
          Source.split_trailing_comment_block statement_source
        in
        let trailing_layout =
          normalize_top_level_trailing_comment_layout trailing_layout
        in
        render_open_statement ~source statement
        |> Option.map (fun rendered ->
               {
                 doc = Doc.text rendered;
                 preserves_layout = false;
                 trailing_layout;
                 consumed_end = None;
                 start_override = None;
               })
    | Syn.Cst.StructureItem.IncludeStatement statement ->
        let statement_source =
          Source.source_of_node_from_source source statement.syntax_node
        in
        let _, trailing_layout =
          Source.split_trailing_comment_block statement_source
        in
        let trailing_layout =
          normalize_top_level_trailing_comment_layout trailing_layout
        in
        render_include_statement ~source statement
        |> Option.map (fun rendered ->
               {
                 doc = Doc.text rendered;
                 preserves_layout = false;
                 trailing_layout;
                 consumed_end = None;
                 start_override = None;
               })
    | item ->
        Some (verbatim_structure_item ~source item)

  let render_source_file ~source (source_file : Syn.Cst.source_file) =
    match source_file with
    | Syn.Cst.Implementation { syntax_node; items } ->
        let file_span = Syn.Ceibo.Red.SyntaxNode.span syntax_node in
        let allow_rewrite = List.for_all structure_item_supports_mixed_rewrite items in
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
                        else if Source.contains_comment_like_text interstitial then
                          let interstitial =
                            Source.trim_trailing_layout_whitespace interstitial
                          in
                          let interstitial =
                            if String.ends_with ~suffix:"\n" interstitial then
                              interstitial
                            else
                              interstitial ^ "\n"
                          in
                          Doc.text interstitial :: acc
                        else
                          Doc.text interstitial :: acc
                      else if Source.is_whitespace_only interstitial then
                        Doc.concat [ Doc.line; Doc.line ] :: acc
                      else
                        Doc.concat [ Doc.line; Doc.line ] :: acc
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
