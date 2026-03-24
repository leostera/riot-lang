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
      | Some guard -> Doc.concat [ Doc.text " when "; render ~indent:0 guard ]
    in
    let body = render ~indent:(case_indent + 4) case.body in
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
                if Doc.is_multiline body then
                  Doc.concat
                    [
                      Doc.text (indent_string or_indent ^ "| " ^ last);
                      guard;
                      Doc.text " ->";
                      Doc.line;
                      body;
                    ]
                else
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
        if Doc.is_multiline body then
          [
            Doc.concat
              [
                Doc.text (indent_string case_indent ^ "| " ^ source_of_pattern case.pattern);
                guard;
                Doc.text " ->";
                Doc.line;
                body;
              ];
          ]
        else
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
      let rendered_bound_value = render ~indent:(indent + 2) bound_value in
      let current =
        render_binding ~indent ~keyword:"let "
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
          let rendered_body =
            match expression with
            | Syn.Cst.Expression.Sequence _ ->
                render ~indent:(indent + 1) expression
            | _ ->
                render ~indent:(indent + 2) expression
          in
          if Doc.is_multiline rendered_body then
            let header =
              match expression with
              | Syn.Cst.Expression.Sequence _ ->
                  Doc.text (indent_string indent ^ "fun " ^ rendered_parameters ^ " ->")
              | _ ->
                  Doc.text ("fun " ^ rendered_parameters ^ " -> ")
            in
            Doc.concat [ header; Doc.line; rendered_body ]
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
        render ~indent:0 scrutinee;
        Doc.text (if keyword_trailing_space then " with " else " with");
        Doc.line;
        (multiline_cases ~case_indent:indent ~or_indent:indent cases |> Doc.join Doc.line);
      ]

  and render_if_expression ~indent ({ condition; then_branch; else_branch; _ } : Syn.Cst.if_expression) =
    let rendered_condition = render ~indent:0 condition in
    let rendered_then = render ~indent then_branch in
    match else_branch with
    | None ->
        Doc.concat [ Doc.text "if "; rendered_condition; Doc.text " then "; rendered_then ]
    | Some else_branch ->
        let rendered_else = render ~indent else_branch in
        if Doc.is_multiline rendered_then then
          Doc.concat
            [
              Doc.text (indent_string indent ^ "if ");
              rendered_condition;
              Doc.text " then ";
              rendered_then;
              Doc.line;
              Doc.text (indent_string indent ^ "else ");
              rendered_else;
            ]
        else if Doc.is_multiline rendered_else then
          let rendered_else = render ~indent:(indent + 2) else_branch in
          Doc.concat
            [
              Doc.text (indent_string indent ^ "if ");
              rendered_condition;
              Doc.text " then ";
              rendered_then;
              Doc.text " else";
              Doc.line;
              rendered_else;
            ]
        else
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
              Doc.concat [ Doc.text "("; render ~indent:0 inner; Doc.text ")" ]
          | _ ->
              render ~indent:0 callee
        in
        let rendered_argument = render ~indent:0 argument in
        if Doc.is_multiline rendered_callee || Doc.is_multiline rendered_argument
        then
          Doc.text (source_of_syntax_node syntax_node)
        else Doc.concat [ rendered_callee; Doc.text " "; rendered_argument ]
    | Syn.Cst.Labeled _ | Syn.Cst.Optional _ ->
        Doc.text (source_of_syntax_node syntax_node)

  and render_sequence_expression ~indent ({ syntax_node; _ } : Syn.Cst.sequence_expression) =
    source_of_syntax_node syntax_node |> dedent_multiline_block |> indent_block indent |> Doc.text

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
            let value = render ~indent:2 binding.value in
            render_binding ~indent:0 ~keyword pattern value
        | Syn.Cst.Expression.Fun _ ->
            let source =
              source_of_syntax_node (Syn.Cst.Expression.syntax_node binding.value)
            in
            if contains_substring source "\n" && expression_contains_sequence binding.value then
              let value = render ~indent:1 binding.value in
              render_binding ~indent:0 ~keyword pattern value
            else
              let value = render ~indent:0 binding.value in
              Doc.concat [ Doc.text (keyword ^ pattern ^ " = "); value ]
        | Syn.Cst.Expression.If if_ ->
            let source =
              source_of_syntax_node (Syn.Cst.Expression.syntax_node binding.value)
            in
            if contains_substring source "\n" && expression_contains_sequence binding.value then
              let value = render_if_expression ~indent:2 if_ in
              render_binding ~indent:0 ~keyword pattern value
            else
              let value = render ~indent:0 binding.value in
              Doc.concat [ Doc.text (keyword ^ pattern ^ " = "); value ]
        | Syn.Cst.Expression.Match match_ ->
            let source =
              source_of_syntax_node (Syn.Cst.Expression.syntax_node binding.value)
            in
            if contains_substring source "\n" && expression_contains_sequence binding.value then
              let value =
                render_match_expression ~indent:2 ~keyword_trailing_space:false match_
              in
              render_binding ~indent:0 ~keyword pattern value
            else
              let value = render ~indent:0 binding.value in
              Doc.concat [ Doc.text (keyword ^ pattern ^ " = "); value ]
        | Syn.Cst.Expression.Sequence sequence ->
            let value = render_sequence_expression ~indent:2 sequence in
            render_binding ~indent:0 ~keyword pattern value
        | _ ->
            let value = render ~indent:0 binding.value in
            Doc.concat [ Doc.text (keyword ^ pattern ^ " = "); value ])
end

module Structure = struct
  type rendered_structure_item = {
    doc : Doc.t;
    preserves_layout : bool;
    trailing_layout : string;
  }

  let verbatim_structure_item_from_text text =
    let body, trailing_layout = Source.split_trailing_comment_block text in
    {
      doc = Doc.text body;
      preserves_layout = true;
      trailing_layout;
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
                Some { doc = rendered; preserves_layout = false; trailing_layout = "" }
              else
                Some (verbatim_structure_item_from_text original)
          | None ->
              Some
                (verbatim_structure_item_from_text
                   (Source.source_of_node_from_source source binding.syntax_node)))
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
              let interstitial =
                previous_trailing_layout
                ^
                Source.source_between source ~start:previous_end ~end_:item_span.start
              in
              (match render_structure_item ~source ~allow_rewrite item with
              | Some rendered ->
                  let preserve_interstitial =
                    is_first_item
                    || (not (Source.is_whitespace_only interstitial))
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
                    else
                      if Source.is_whitespace_only interstitial then
                        Doc.concat [ Doc.line; Doc.line ] :: acc
                      else
                        Doc.text interstitial :: acc
                  in
                  render_items
                    (rendered.doc :: acc)
                    item_span.end_
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
