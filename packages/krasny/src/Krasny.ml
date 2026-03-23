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

let rec render_expression = function
  | Syn.Cst.Expression.Literal literal -> render_literal literal
  | Syn.Cst.Expression.Path { path; _ } -> Some (source_of_ident path)
  | Syn.Cst.Expression.Parenthesized { inner; _ } -> render_expression inner
  | Syn.Cst.Expression.Prefix { operator_token; operand = Literal literal; _ } ->
      render_literal literal
      |> Option.map (fun rendered -> "(" ^ source_of_token operator_token ^ rendered ^ ")")
  | _ -> None

let rec render_pattern = function
  | Syn.Cst.Pattern.Identifier { name_token; _ } -> Some (source_of_token name_token)
  | Syn.Cst.Pattern.Parenthesized { inner; _ } -> render_pattern inner
  | _ -> None

let render_let_binding (binding : Syn.Cst.LetBinding.t) =
  if List.length binding.attributes > 0 || List.length binding.parameters > 0 then
    None
  else
    match (render_pattern binding.binding_pattern, render_expression binding.value) with
    | Some binding_pattern, Some value ->
        Some
          ("let "
          ^ if binding.is_recursive then "rec " else ""
          ^ binding_pattern ^ " = " ^ value)
    | _ ->
        None

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
