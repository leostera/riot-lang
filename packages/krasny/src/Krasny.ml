open Std
open Std.Collections

type format_error =
  | Cannot_build_cst of Syn.build_cst_error

let source_of_syntax_node (node : Syn.Cst.syntax_node) =
  let buffer = IO.Buffer.create 1024 in
  Syn.Ceibo.Red.SyntaxNode.preorder node (function
    | Syn.Ceibo.Red.Token token ->
        IO.Buffer.add_string buffer (Syn.Ceibo.Red.SyntaxToken.text token)
    | Syn.Ceibo.Red.Node _ ->
        ());
  IO.Buffer.contents buffer

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

let has_comment_like_trivia (result : Syn.Parser.parse_result) =
  let rec loop = function
    | Syn.Ceibo.Green.Token _ as element -> (
        match Syn.Ceibo.Green.kind element with
        | Syn.SyntaxKind.COMMENT | Syn.SyntaxKind.DOCSTRING -> true
        | _ -> false)
    | Syn.Ceibo.Green.Node node -> Array.exists loop (Syn.Ceibo.Green.children node)
  in
  loop (Syn.Ceibo.Green.Node result.tree)

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

let render_binding ~indent ~keyword binding_pattern value =
  let prefix = indent_string indent in
  if String.contains value "\n" then
    prefix ^ keyword ^ binding_pattern ^ " =\n" ^ value
  else prefix ^ keyword ^ binding_pattern ^ " = " ^ value

let indent_block spaces text =
  text |> String.split_on_char '\n'
  |> List.map (fun line -> if line = "" then line else indent_string spaces ^ line)
  |> String.concat "\n"

let source_of_or_pattern_alternatives { Syn.Cst.alternatives; _ } =
  alternatives |> List.map source_of_pattern

let rec render_expression ~indent = function
  | Syn.Cst.Expression.Literal literal ->
      render_literal literal
      |> Option.expect ~msg:"literal rendering should always succeed"
  | Syn.Cst.Expression.Path { path; _ } ->
      source_of_ident path
  | Syn.Cst.Expression.Parenthesized { inner = Function function_; _ } ->
      "(" ^ render_function_expression ~indent:0 function_ ^ ")"
  | Syn.Cst.Expression.Parenthesized { inner; _ } ->
      render_expression ~indent inner
  | Syn.Cst.Expression.Prefix { operator_token; operand = Literal literal; _ } ->
      let rendered =
        render_literal literal |> Option.expect ~msg:"literal rendering should always succeed"
      in
      "(" ^ source_of_token operator_token ^ rendered ^ ")"
  | Syn.Cst.Expression.Let let_ ->
      render_let_expression ~indent let_
  | Syn.Cst.Expression.Fun fun_ ->
      render_fun_expression ~indent fun_
  | Syn.Cst.Expression.Function function_ ->
      render_function_expression ~indent function_
  | Syn.Cst.Expression.Match match_ ->
      render_match_expression ~indent ~keyword_trailing_space:true match_
  | Syn.Cst.Expression.Apply apply ->
      render_apply_expression ~indent apply
  | expression ->
      source_of_syntax_node (Syn.Cst.Expression.syntax_node expression)

and multiline_case_lines ~case_indent ~or_indent ~is_last_case (case : Syn.Cst.match_case) =
  let guard =
    match case.guard with
    | None -> ""
    | Some guard -> " when " ^ render_expression ~indent:0 guard
  in
  let body = render_expression ~indent:0 case.body in
  let trailing_space = if is_last_case then "" else " " in
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
                   indent_string or_indent ^ "| " ^ alternative ^ " ")
          in
          rendered_alternatives
          @ [ indent_string or_indent ^ "| " ^ last ^ guard ^ " -> " ^ body ^ trailing_space ])
  | _ ->
      [ indent_string case_indent ^ "| " ^ source_of_pattern case.pattern ^ guard ^ " -> " ^ body
        ^ trailing_space ]

and multiline_cases ~case_indent ~or_indent cases =
  cases
  |> List.mapi (fun index case ->
         multiline_case_lines ~case_indent ~or_indent
           ~is_last_case:(index = List.length cases - 1) case)
  |> List.flatten

and render_let_expression ~indent
    ({ syntax_node; binding_pattern; bound_value; and_bindings; body; is_recursive; _ } :
      Syn.Cst.let_expression) =
  if List.length and_bindings > 0 then
    source_of_syntax_node syntax_node
  else
    let current =
      render_binding ~indent
        ~keyword:(if is_recursive then "let rec " else "let ")
        (source_of_pattern binding_pattern)
        (render_expression ~indent:(indent + 2) bound_value)
    in
    let rendered_body = render_expression ~indent body in
    let rendered_body =
      if String.contains rendered_body "\n" then
        rendered_body
      else indent_string indent ^ rendered_body
    in
    current ^ " in\n" ^ rendered_body

and render_fun_expression ~indent
    ({ syntax_node; parameters; body; attributes } : Syn.Cst.fun_expression) =
  if List.length attributes > 0 then
    source_of_syntax_node syntax_node
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
    | Syn.Cst.Expression (Syn.Cst.Expression.Match match_) ->
        "fun " ^ rendered_parameters ^ " -> \n"
        ^ render_match_expression ~indent:(indent + 2) ~keyword_trailing_space:true match_
    | Syn.Cst.Expression expression ->
        let rendered_body = render_expression ~indent:(indent + 2) expression in
        if String.contains rendered_body "\n" then
          "fun " ^ rendered_parameters ^ " -> \n" ^ rendered_body
        else "fun " ^ rendered_parameters ^ " -> " ^ rendered_body
    | Syn.Cst.Cases case_body ->
        source_of_syntax_node case_body.syntax_node

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
      "fun " ^ render_fun_parameter_pattern case.pattern ^ " -> "
      ^ render_expression ~indent case.body
  | cases when should_lower_to_fun_match ->
      let parameter_name = fresh_match_parameter_name syntax_node in
      let scrutinee = parameter_name in
      "fun " ^ parameter_name ^ " ->\n"
      ^ render_match_expression ~indent:(indent + 2) ~keyword_trailing_space:false
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
          }
  | (cases : Syn.Cst.match_case list) ->
      let case_indent = if has_or_patterns then indent + 1 else indent + 2 in
      indent_string indent ^ "function \n"
      ^ (multiline_cases ~case_indent ~or_indent:(indent + 1) cases
        |> String.concat "\n")

and render_match_expression ~indent ~keyword_trailing_space
    ({ scrutinee; cases; _ } : Syn.Cst.match_expression) =
  indent_string indent ^ "match " ^ render_expression ~indent:0 scrutinee
  ^ (if keyword_trailing_space then " with \n" else " with\n")
  ^ (multiline_cases ~case_indent:indent ~or_indent:indent cases |> String.concat "\n")

and render_apply_expression ~indent
    ({ syntax_node; callee; argument; _ } : Syn.Cst.apply_expression) =
  match argument with
  | Syn.Cst.Positional argument ->
      let rendered_callee =
        match callee with
        | Syn.Cst.Expression.Parenthesized { inner; _ } ->
            "(" ^ render_expression ~indent:0 inner ^ ")"
        | _ ->
            render_expression ~indent:0 callee
      in
      let rendered_argument = render_expression ~indent:0 argument in
      if String.contains rendered_callee "\n"
         || String.contains rendered_argument "\n"
      then
        source_of_syntax_node syntax_node
      else rendered_callee ^ " " ^ rendered_argument
  | Syn.Cst.Labeled _ | Syn.Cst.Optional _ ->
      source_of_syntax_node syntax_node

let render_let_binding (binding : Syn.Cst.LetBinding.t) =
  if List.length binding.attributes > 0 || List.length binding.parameters > 0 then
    None
  else
    let keyword = if binding.is_recursive then "let rec " else "let " in
    let pattern = source_of_pattern binding.binding_pattern in
    Some
      (match binding.value with
      | Syn.Cst.Expression.Function { cases; _ } as function_
        when List.exists (fun (case : Syn.Cst.match_case) -> Option.is_some case.guard) cases ->
          keyword ^ pattern ^ " = \n"
          ^ indent_block 2 (render_expression ~indent:0 function_)
      | Syn.Cst.Expression.Let _ ->
          let value = render_expression ~indent:2 binding.value in
          render_binding ~indent:0 ~keyword pattern value
      | _ ->
          let value = render_expression ~indent:0 binding.value in
          keyword ^ pattern ^ " = " ^ value)

let render_structure_item = function
  | Syn.Cst.StructureItem.LetBinding binding -> render_let_binding binding
  | _ -> None

let render_source_file (source_file : Syn.Cst.source_file) =
  match source_file with
  | Syn.Cst.Implementation { items; _ } ->
      let rec render_items acc = function
        | [] -> Some (List.rev acc |> String.concat "\n\n")
        | item :: rest -> (
            match render_structure_item item with
            | Some rendered -> render_items (rendered :: acc) rest
            | None -> None)
      in
      render_items [] items
  | Syn.Cst.Interface _ -> None

let format (result : Syn.Parser.parse_result) =
  match Syn.build_cst result with
  | Error err -> Error (Cannot_build_cst err)
  | Ok source_file ->
      let original_source = source_of_result result in
      if has_comment_like_trivia result then
        Ok original_source
      else
        Ok
          (match render_source_file source_file with
          | Some rendered when String.ends_with ~suffix:"\n" original_source ->
              rendered ^ "\n"
          | Some rendered -> rendered
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
