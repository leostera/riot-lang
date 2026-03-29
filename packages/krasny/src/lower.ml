open Std
open Std.Collections
module Doc = Doc

let blank_line = Doc.concat [ Doc.line; Doc.line ]

let equals = Doc.concat [ Doc.space; Doc.equal; Doc.space ]

let arrow = Doc.concat [ Doc.space; Doc.arrow; Doc.space ]

let colon = Doc.concat [ Doc.space; Doc.colon; Doc.space ]

let annotation_colon = Doc.concat [ Doc.colon; Doc.space ]

let multiline_list_threshold = 10

let star = Doc.text "*"
let at = Doc.text "@"

let semicolon_text = ";"

type ctx = {
  source : string option;
}

type error = {
  message : string;
  context : string list;
}

exception Unsupported of error

let error_to_string = fun err ->
  match err.context with
  | [] ->
      err.message
  | context ->
      err.message ^ " [" ^ String.concat " > " context ^ "]"

let unsupported = fun ?(context = []) message -> raise (Unsupported {message; context})

let unsupported_syntax = fun ?(context = []) ~syntax_node message ->
  let kind =
    Syn.Ceibo.Red.SyntaxNode.kind syntax_node
    |> Syn.SyntaxKind.to_string
  in
  unsupported ~context:(context @ [ kind ]) message

type pending_trivia_entry =
  | TriviaComment of int * Doc.t
  | TriviaDocstring of int * bool * Doc.t
  | TriviaBreak of int * int

let token_text = Syn.Cst.Token.text

let doc_of_token = fun token -> Doc.text (token_text token)

let is_keyword_operator_name =
  function
  | "mod"
  | "land"
  | "lor"
  | "lxor"
  | "lsl"
  | "lsr"
  | "asr"
  | "or" ->
      true
  | _ ->
      false

let is_operator_like_text = fun text ->
  let is_identifier_char =
    function
    | 'a' .. 'z'
    | 'A' .. 'Z'
    | '0' .. '9'
    | '_'
    | '\'' ->
        true
    | _ ->
        false
  in
  let rec contains_non_identifier_char = fun index ->
    if index >= String.length text then
      false
    else if is_identifier_char text.[index] then
      contains_non_identifier_char (index + 1)
    else
      true
  in
  String.length text > 0 && (is_keyword_operator_name text || contains_non_identifier_char 0)

let text_of_syntax_node = fun syntax_node ->
  Syn.Ceibo.Red.SyntaxNode.tokens syntax_node
  |> function
  | [] ->
      ""
  | first :: rest ->
      let first_text = Syn.Ceibo.Red.SyntaxToken.text first in
      let rest_text =
        rest
        |> List.map (fun token ->
               let leading =
                 Syn.Ceibo.Red.SyntaxToken.leading_trivia token
                 |> List.map Syn.Ceibo.Red.SyntaxTrivia.text
                 |> String.concat ""
               in
               leading ^ Syn.Ceibo.Red.SyntaxToken.text token)
        |> String.concat ""
      in
      first_text ^ rest_text

let string_contains_substring = fun text pattern ->
  let text_length = String.length text in
  let pattern_length = String.length pattern in
  let rec loop = fun index ->
    if index + pattern_length > text_length then
      false
    else if String.sub text index pattern_length = pattern then
      true
    else
      loop (index + 1)
  in
  pattern_length > 0 && loop 0

let token_is_phrase_separator = fun token ->
  String.equal (Syn.Ceibo.Red.SyntaxToken.text token) semicolon_text

let normalized_source_length = fun source ->
  let rec loop = fun index in_whitespace acc ->
    if index >= String.length source then
      acc
    else
      match source.[index] with
      | ' '
      | '\t'
      | '\n'
      | '\r' ->
          if in_whitespace then
            loop (index + 1) true acc
          else
            loop (index + 1) true (acc + 1)
      | _ ->
          loop (index + 1) false (acc + 1)
  in
  loop 0 true 0

let trim_trailing_layout_whitespace = fun text ->
  let rec find_last_non_layout = fun index ->
    if index < 0 then
      (-1)
    else
      match text.[index] with
      | ' '
      | '\t'
      | '\n'
      | '\r' ->
          find_last_non_layout (index - 1)
      | _ ->
          index
  in
  let last_index = find_last_non_layout (String.length text - 1) in
  if last_index < 0 then
    ""
  else
    String.sub text 0 (last_index + 1)

let strip_outer_parens_once = fun text ->
  let text = String.trim text in
  let length = String.length text in
  if length >= 2 && text.[0] = '(' && text.[length - 1] = ')' then
    String.sub text 1 (length - 2) |> String.trim
  else
    text

let strip_module_prefix = fun text ->
  let text = String.trim text in
  if String.starts_with ~prefix:"module " text then
    String.sub text 7 (String.length text - 7) |> String.trim
  else
    text

let syntax_node_has_internal_newline = fun syntax_node ->
  let text = text_of_syntax_node syntax_node |> trim_trailing_layout_whitespace in
  String.contains text "\n"

let doc_of_ident = fun ident ->
  Syn.Cst.Ident.segments ident |> List.map doc_of_token |> Doc.join (Doc.text ".")

let doc_of_nontrivia_direct_tokens =
  fun syntax_node ->
    Syn.Ceibo.Red.SyntaxNode.direct_tokens syntax_node
    |> List.map (fun syntax_token -> Doc.text (Syn.Ceibo.Red.SyntaxToken.text syntax_token))
    |> Doc.concat

let token_requires_parenthesized_value_name = fun (token : Syn.Cst.Token.t) ->
  match Syn.Cst.Token.syntax_token token |> Syn.Ceibo.Red.SyntaxToken.kind with
  | Syn.SyntaxKind.IDENT_EXPR ->
      false
  | _ ->
      true

let render_value_declaration_name = fun (decl : Syn.Cst.value_declaration) ->
  if token_requires_parenthesized_value_name decl.name_token then
    Doc.concat [ Doc.lparen; Doc.space; doc_of_token decl.name_token; Doc.space; Doc.rparen ]
  else
    doc_of_token decl.name_token

let nontrivia_direct_tokens =
  fun syntax_node -> Syn.Ceibo.Red.SyntaxNode.direct_tokens syntax_node

let nontrivia_bounds_span_of_syntax_node = fun syntax_node ->
  let full_span = Syn.Ceibo.Red.SyntaxNode.span syntax_node in
  match Syn.Ceibo.Red.SyntaxNode.tokens syntax_node with
  | [] ->
      full_span
  | first :: rest ->
      let last = List.fold_left (fun _ token -> token) first rest in
      {
        Syn.Ceibo.Span.start = (Syn.Ceibo.Red.SyntaxToken.span first).start;
        end_ = (Syn.Ceibo.Red.SyntaxToken.span last).end_
      }

let binding_has_explicit_fun_rhs = fun (binding : Syn.Cst.let_binding) ->
  List.is_empty binding.parameters
  && match binding.value with
  | Syn.Cst.Expression.Fun _ ->
      true
  | _ ->
      false

let push_pending_break = fun pending ~position break_count ->
  if break_count <= 0 then
    pending
  else
    match pending with
    | TriviaBreak (existing_position, existing_break_count) :: rest when Int.equal
    existing_position
    position ->
        TriviaBreak (existing_position, Int.max existing_break_count break_count) :: rest
    | _ ->
        TriviaBreak (position, break_count) :: pending

let child_span =
  function
  | Syn.Ceibo.Red.Token syntax_token ->
      Syn.Ceibo.Red.SyntaxToken.span syntax_token
  | Syn.Ceibo.Red.Node syntax_node ->
      Syn.Ceibo.Red.SyntaxNode.span syntax_node

let compare_child_by_span = fun left right ->
  let left_span = child_span left in
  let right_span = child_span right in
  if not (Int.equal left_span.start right_span.start) then
    Int.compare left_span.start right_span.start
  else if not (Int.equal left_span.end_ right_span.end_) then
    Int.compare left_span.end_ right_span.end_
  else
    0

let children_in_source_order = fun syntax_node ->
  Syn.Ceibo.Red.SyntaxNode.children_list syntax_node |> List.sort compare_child_by_span

let compare_direct_token_by_span = fun left right ->
  let left_span = Syn.Ceibo.Red.SyntaxToken.span left in
  let right_span = Syn.Ceibo.Red.SyntaxToken.span right in
  if not (Int.equal left_span.start right_span.start) then
    Int.compare left_span.start right_span.start
  else
    Int.compare left_span.end_ right_span.end_

let direct_tokens_in_source_order = fun syntax_node ->
  Syn.Ceibo.Red.SyntaxNode.direct_tokens syntax_node
  |> List.sort compare_direct_token_by_span

let phrase_separator_count_between_tokens = fun tokens ~start ~end_ ->
  let rec loop count = function
    | [] ->
        count
    | token :: rest ->
        let span = Syn.Ceibo.Red.SyntaxToken.span token in
        if span.end_ <= start then
          loop count rest
        else if span.start >= end_ then
          count
        else if token_is_phrase_separator token then
          loop (count + 1) rest
        else
          -1
  in
  loop 0 tokens

let phrase_separator_doc_between_tokens = fun tokens ~start ~end_ ->
  match phrase_separator_count_between_tokens tokens ~start ~end_ with
  | count when count > 0 ->
      Some (List.init count (fun _ -> Doc.semi) |> Doc.concat)
  | _ ->
      None

let is_layout_character =
  function
  | ' '
  | '\t'
  | '\n'
  | '\r' ->
      true
  | _ ->
      false

let is_section_docstring_text = fun comment_text ->
  let len = String.length comment_text in
  if len < 5 then
    false
  else
    let body = String.sub comment_text 3 (len - 5) |> String.trim in
    String.length body > 0
    && (Char.equal body.[0] '{' || Char.equal body.[0] '#')

let parse_trivia_between_offsets = fun source ~start ~end_ pending ->
  let source_length = String.length source in
  let start = Int.max 0 (Int.min start source_length) in
  let end_ = Int.max start (Int.min end_ source_length) in
  let rec consume_whitespace = fun index newline_count ->
    if index >= end_ || index >= source_length || index < 0 then
      (index, newline_count)
    else
      match source.[index] with
      | '\n' ->
          consume_whitespace (index + 1) (newline_count + 1)
      | ' '
      | '\t'
      | '\r' ->
          consume_whitespace (index + 1) newline_count
      | _ ->
          (index, newline_count)
  in
  let rec consume_comment = fun index depth ->
    if index < 0 || index + 1 >= end_ || index + 1 >= source_length then
      Int.min end_ source_length
    else if source.[index] = '(' && source.[index + 1] = '*' then
      consume_comment (index + 2) (depth + 1)
    else if source.[index] = '*' && source.[index + 1] = ')' then
      if depth = 1 then
        index + 2
      else
        consume_comment (index + 2) (depth - 1)
    else
      consume_comment (index + 1) depth
  in
  let rec loop = fun index pending ->
    if index < 0 || index >= end_ || index >= source_length then
      pending
    else if is_layout_character source.[index] then
      let next_index, newline_count = consume_whitespace index 0 in
      let pending =
        if newline_count > 0 then
          push_pending_break pending ~position:index newline_count
        else
          pending
      in
      loop next_index pending
    else if
      index + 1 < end_
      && index + 1 < source_length
      && source.[index] = '('
      && source.[index + 1] = '*'
    then
      let comment_end = consume_comment (index + 2) 1 |> Int.max index |> Int.min source_length in
      if comment_end > index then
        let comment_text = String.sub source index (comment_end - index) in
        let pending_entry =
          if
            String.starts_with ~prefix:"(**" comment_text
            && not (String.starts_with ~prefix:"(***" comment_text)
          then
            TriviaDocstring (index, is_section_docstring_text comment_text, Doc.text comment_text)
          else
            TriviaComment (index, Doc.text comment_text)
        in
        loop comment_end (pending_entry :: pending)
      else
        loop (index + 1) pending
    else
      loop (index + 1) pending
  in
  try loop start pending with
  | Invalid_argument _ ->
      pending

let pending_trivia_position =
  function
  | TriviaComment (position, _)
  | TriviaDocstring (position, _, _) ->
      position
  | TriviaBreak (position, _) ->
      position

let compare_pending_trivia_by_position = fun left right ->
  Int.compare (pending_trivia_position left) (pending_trivia_position right)

let extract_leading_inline_comment = fun pending ->
  let pending = List.sort compare_pending_trivia_by_position pending in
  match pending with
  | TriviaComment (_, doc) :: rest ->
      (Some doc, rest)
  | _ ->
      (None, pending)

let render_pending_trivia = fun ?(strip_trailing_breaks = true) pending ->
  let break_doc = fun break_count -> List.init break_count (fun _ -> Doc.line) |> Doc.concat in
  let rec strip_trailing_blanks =
    function
    | [] ->
        []
    | [ TriviaBreak _ ] ->
        []
    | entry :: rest ->
        let rest = strip_trailing_blanks rest in
        (
          match entry, rest with
          | TriviaBreak _, [] ->
              []
          | _ ->
              entry :: rest
        )
  in
  let rec loop = fun acc separator ->
    function
    | [] ->
        acc
    | TriviaBreak (_, break_count) :: rest ->
        let separator = break_doc break_count in
        loop acc separator rest
    | (TriviaComment (_, doc) | TriviaDocstring (_, _, doc)) :: rest ->
        let acc =
          match acc with
          | None ->
              Some doc
          | Some current ->
              Some (Doc.concat [ current; separator; doc ])
        in
        loop acc Doc.line rest
  in
  let pending = List.sort compare_pending_trivia_by_position pending in
  let pending =
    if strip_trailing_breaks then
      strip_trailing_blanks pending
    else
      pending
  in
  let trailing_break =
    if strip_trailing_breaks then
      None
    else
      match List.rev pending with
      | TriviaBreak (_, break_count) :: _ ->
          Some (break_doc break_count)
      | _ ->
          None
  in
  match loop None Doc.line pending, trailing_break with
  | Some doc, Some trailing_break ->
      Some (Doc.concat [ doc; trailing_break ])
  | doc, None ->
      doc
  | None, Some _ ->
      None

let pending_entry_of_syntax_trivia = fun trivia ->
  let span = Syn.Ceibo.Red.SyntaxTrivia.span trivia in
  match Syn.Ceibo.Red.SyntaxTrivia.kind trivia with
  | Syn.SyntaxKind.WHITESPACE ->
      let newline_count =
        String.fold_left
          (fun count char -> if Char.equal char '\n' then count + 1 else count)
          0
          (Syn.Ceibo.Red.SyntaxTrivia.text trivia)
      in
      if newline_count > 0 then
        Some (TriviaBreak (span.start, newline_count))
      else
        None
  | Syn.SyntaxKind.COMMENT ->
      Some (TriviaComment (span.start, Doc.text (Syn.Ceibo.Red.SyntaxTrivia.text trivia)))
  | Syn.SyntaxKind.DOCSTRING ->
      let text = Syn.Ceibo.Red.SyntaxTrivia.text trivia in
      Some (TriviaDocstring (span.start, is_section_docstring_text text, Doc.text text))
  | _ ->
      None

let pending_entries_of_leading_trivia_after = fun ~after token ->
  Syn.Ceibo.Red.SyntaxToken.leading_trivia token
  |> List.filter_map (fun trivia ->
         let span = Syn.Ceibo.Red.SyntaxTrivia.span trivia in
         if span.start >= after then
           pending_entry_of_syntax_trivia trivia
         else
           None)

let render_leading_trivia_before_token = fun ~after token ->
  pending_entries_of_leading_trivia_after ~after token
  |> render_pending_trivia

let render_leading_trivia_before_node = fun ~after syntax_node ->
  match Syn.Ceibo.Red.SyntaxNode.tokens syntax_node with
  | [] ->
      None
  | first :: _ ->
      render_leading_trivia_before_token ~after first

let render_leading_trivia_after_token_before_node = fun ~after token syntax_node ->
  let pending = pending_entries_of_leading_trivia_after ~after token in
  let pending =
    match Syn.Ceibo.Red.SyntaxNode.tokens syntax_node with
    | [] ->
        pending
    | first :: _ ->
        pending
        @ pending_entries_of_leading_trivia_after
            ~after:(Syn.Ceibo.Red.SyntaxToken.span token).end_
            first
  in
  render_pending_trivia pending

let doc_with_trailing_trivia = fun doc trivia ->
  match trivia with
  | None ->
      doc
  | Some trivia ->
      Doc.concat [ doc; Doc.line; trivia ]

let doc_with_leading_trivia = fun trivia doc ->
  match trivia with
  | None ->
      doc
  | Some trivia ->
      Doc.concat [ trivia; Doc.line; doc ]

let separator_doc_between_offsets = fun source ~start ~end_ ->
  let source_length = String.length source in
  let start = Int.max 0 (Int.min start source_length) in
  let end_ = Int.max start (Int.min end_ source_length) in
  let rec loop = fun index saw_inline_spacing newline_count ->
    if index >= end_ then
      (saw_inline_spacing, newline_count)
    else
      match source.[index] with
      | '\n' ->
          loop (index + 1) false (newline_count + 1)
      | ' '
      | '\t'
      | '\r' ->
          loop (index + 1) true newline_count
      | _ ->
          loop (index + 1) true newline_count
  in
  let saw_inline_spacing, newline_count = loop start false 0 in
  if newline_count > 0 then
    List.init newline_count (fun _ -> Doc.line) |> Doc.concat
  else if saw_inline_spacing then
    Doc.space
  else
    Doc.empty

let doc_of_owned_trivia = fun ?start ~source trivia ->
  let trivia = trivia |> List.sort (fun left right -> Int.compare
  ((Syn.Cst.Token.span (Syn.Cst.Trivia.token left)).start)
  ((Syn.Cst.Token.span (Syn.Cst.Trivia.token right)).start)) in
  let trivia_doc =
    function
    | Syn.Cst.Trivia.Comment comment ->
        Doc.text (Syn.Cst.Comment.text comment)
    | Syn.Cst.Trivia.Docstring docstring ->
        Doc.text (Syn.Cst.Docstring.text docstring)
  in
  let rec loop = fun acc previous_end ->
    function
    | [] ->
        acc
    | trivia :: rest ->
        let span = Syn.Cst.Token.span (Syn.Cst.Trivia.token trivia) in
        let separator =
          match previous_end with
          | None ->
              Doc.empty
          | Some previous_end ->
              separator_doc_between_offsets source ~start:previous_end ~end_:span.start
        in
        let piece = Doc.concat [ separator; trivia_doc trivia ] in
        let acc =
          match acc with
          | None ->
              Some piece
          | Some current ->
              Some (Doc.concat [ current; piece ])
        in
        loop acc (Some span.end_) rest
  in
  loop None start trivia

let render_attribute_doc = fun ~floating (attribute : Syn.Cst.attribute) ->
  let sigil_doc =
    if floating then
      Doc.concat [ at; doc_of_token attribute.sigil_token ]
    else
      doc_of_token attribute.sigil_token
  in
  Doc.concat
    [ Doc.lbracket; sigil_doc; doc_of_ident attribute.name; (
        match attribute.payload_syntax_node with
        | Some payload_syntax_node ->
            let payload_text = Source.source_of_syntax_node payload_syntax_node |> String.trim in
            if payload_text = "" then
              Doc.empty
            else
              Doc.concat [ Doc.space; Doc.text payload_text ]
        | None ->
            Doc.empty
      ); Doc.rbracket ]

let render_attribute = fun attribute -> render_attribute_doc ~floating:false attribute
let render_floating_attribute = fun attribute -> render_attribute_doc ~floating:true attribute

let kw_module = Doc.text "module"

let render_first_class_module_type = fun module_type ->
  let module_type_text = Syn.Cst.ModuleType.syntax_node module_type
  |> text_of_syntax_node
  |> strip_outer_parens_once
  |> strip_module_prefix in
  Doc.concat [ Doc.lparen; kw_module; Doc.space; Doc.text module_type_text; Doc.rparen ]

let kw_let = Doc.text "let"

let kw_rec = Doc.text "rec"

let kw_and = Doc.text "and"

let kw_in = Doc.text "in"

let kw_if = Doc.text "if"

let kw_then = Doc.text "then"

let kw_else = Doc.text "else"

let kw_match = Doc.text "match"

let kw_try = Doc.text "try"

let kw_with = Doc.text "with"

let kw_when = Doc.text "when"

let kw_function =
  Doc.text "function"

let kw_fun = Doc.text "fun"

let kw_open = Doc.text "open"

let kw_val = Doc.text "val"

let kw_type = Doc.text "type"

let kw_external = Doc.text "external"

let kw_constraint = Doc.text "constraint"

let kw_of = Doc.text "of"

let kw_mutable = Doc.text "mutable"

let kw_private = Doc.text "private"

let kw_assert = Doc.text "assert"

let kw_lazy = Doc.text "lazy"

let kw_while = Doc.text "while"

let kw_for = Doc.text "for"

let kw_do = Doc.text "do"

let kw_done = Doc.text "done"

let kw_new = Doc.text "new"

let hash = Doc.text "#"

let coercion_arrow = Doc.text ":>"

let object_override_open = Doc.text "{<"

let object_override_close = Doc.text ">}"

let join_map = fun separator f ->
  function
  | [] ->
      Doc.empty
  | first :: rest ->
      Doc.concat (f first :: List.map (fun item -> Doc.concat [ separator; f item ]) rest)

let group_digits_from_left = fun ~group_size digits ->
  let digits = String.split_on_char '_' digits |> String.concat "" in
  let length = String.length digits in
  if length <= group_size then
    digits
  else
    let buffer = IO.Buffer.create (length + length / group_size) in
    let rec loop = fun index ->
      if index >= length then
        IO.Buffer.contents buffer
      else
        (
          if index > 0 then
            IO.Buffer.add_char buffer '_';
          let chunk_size = Int.min group_size (length - index) in
          IO.Buffer.add_string buffer (String.sub digits index chunk_size);
          loop (index + chunk_size)
        )
    in
    loop 0

let group_digits_from_right = fun ~group_size digits ->
  let digits = String.split_on_char '_' digits |> String.concat "" in
  let length = String.length digits in
  if length <= group_size then
    digits
  else
    let first_group_size =
      match length mod group_size with
      | 0 -> group_size
      | remainder -> remainder
    in
    let buffer = IO.Buffer.create (length + length / group_size) in
    IO.Buffer.add_string buffer (String.sub digits 0 first_group_size);
    let rec loop = fun index ->
      if index >= length then
        IO.Buffer.contents buffer
      else (
        IO.Buffer.add_char buffer '_';
        IO.Buffer.add_string buffer (String.sub digits index group_size);
        loop (index + group_size)
      )
    in
    loop first_group_size

let render_integer_constant = fun (literal : Syn.Cst.integer_constant) ->
  let prefix =
    match literal.base with
    | Syn.Cst.Decimal -> Option.unwrap_or literal.prefix ~default:""
    | Syn.Cst.Hexadecimal -> "0x"
    | Syn.Cst.Octal -> "0o"
    | Syn.Cst.Binary -> "0b"
  in
  let digits =
    match literal.base with
    | Syn.Cst.Decimal
    | Syn.Cst.Octal ->
        group_digits_from_right ~group_size:3 literal.digits
    | Syn.Cst.Binary ->
        group_digits_from_right ~group_size:4 literal.digits
    | Syn.Cst.Hexadecimal ->
        literal.digits |> String.lowercase_ascii |> group_digits_from_right ~group_size:4
  in
  let suffix = Option.unwrap_or literal.suffix ~default:"" in
  prefix ^ digits ^ suffix

let render_float_constant = fun (literal : Syn.Cst.float_constant) ->
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
  let normalized_integral_digits = String.split_on_char '_' literal.integral_digits
  |> String.concat "" in
  let integral_digits =
    if String.length normalized_integral_digits >= 8 then
      group_digits_from_right ~group_size:3 normalized_integral_digits
    else
      normalized_integral_digits
  in
  let fractional_digits = group_digits_from_left ~group_size:3 literal.fractional_digits in
  integral_digits ^ "." ^ fractional_digits ^ exponent ^ suffix

let signed_literal_text_from_syntax_node = fun ~default_text syntax_node ->
  let sign_prefix =
    nontrivia_direct_tokens syntax_node
    |> List.find_map
      (fun syntax_token ->
        let text = Syn.Ceibo.Red.SyntaxToken.text syntax_token in
        if String.equal text "-" || String.equal text "+" then
          Some text
        else
          None)
  in
  match sign_prefix with
  | Some _ when String.starts_with ~prefix:"-" default_text || String.starts_with ~prefix:"+" default_text ->
      default_text
  | Some sign ->
      sign ^ default_text
  | None ->
      default_text

let render_literal =
  function
  | Syn.Cst.Literal.Int literal ->
      let literal_text = signed_literal_text_from_syntax_node ~default_text:(render_integer_constant
      literal) literal.syntax_node in
      Doc.text literal_text
  | Syn.Cst.Literal.Float literal ->
      let literal_text = signed_literal_text_from_syntax_node ~default_text:(render_float_constant literal) literal.syntax_node in
      Doc.text literal_text
  | Syn.Cst.Literal.String literal ->
      doc_of_token literal.literal_token
  | Syn.Cst.Literal.Char literal ->
      doc_of_token literal.literal_token
  | Syn.Cst.Literal.Bool literal ->
      Doc.text
        (
          if literal.value then
            "true"
          else
            "false"
        )
  | Syn.Cst.Literal.Unit _ ->
      Doc.text "()"

let render_type_binder =
  function
  | Syn.Cst.TypeBinder.Quoted binder ->
      Doc.text (Syn.Cst.TypeBinder.text (Syn.Cst.TypeBinder.Quoted binder))
  | Syn.Cst.TypeBinder.Bare binder ->
      Doc.text (Syn.Cst.TypeBinder.text (Syn.Cst.TypeBinder.Bare binder))

let poly_type_has_explicit_type_keyword = fun syntax_node ->
  match Syn.Ceibo.Red.SyntaxNode.tokens syntax_node with
  | token :: _ ->
      Syn.Ceibo.Red.SyntaxToken.text token = "type"
  | [] ->
      false

let render_arrow_label =
  function
  | None ->
      Doc.empty
  | Some (Syn.Cst.ArrowLabel.Named { sigil_token; label_token }) ->
      Doc.concat [
        Option.unwrap_or (Option.map doc_of_token sigil_token) ~default:Doc.empty;
        doc_of_token label_token;
        Doc.colon
      ]
  | Some (Syn.Cst.ArrowLabel.OptionalNamed { sigil_token; label_token }) ->
      Doc.concat [ doc_of_token sigil_token; doc_of_token label_token; Doc.colon ]

let rec core_type_needs_parens_in_application =
  function
  | Syn.Cst.CoreType.Arrow _
  | Syn.Cst.CoreType.Tuple _
  | Syn.Cst.CoreType.PolyVariant _
  | Syn.Cst.CoreType.Record _
  | Syn.Cst.CoreType.Object _
  | Syn.Cst.CoreType.Alias _ ->
      true
  | Syn.Cst.CoreType.Attribute { type_; _ } ->
      core_type_needs_parens_in_application type_
  | Syn.Cst.CoreType.Parenthesized _ ->
      false
  | _ ->
      false

let render_type_parameter = fun parameter ->
  let variance =
    match Syn.Cst.TypeParameter.variance parameter with
    | None ->
        Doc.empty
    | Some (Syn.Cst.TypeParameterVariance.Covariant { marker_token }) ->
        doc_of_token marker_token
    | Some (Syn.Cst.TypeParameterVariance.Contravariant { marker_token }) ->
        doc_of_token marker_token
  in
  let injective =
    if Syn.Cst.TypeParameter.is_injective parameter then
      Doc.text "!"
    else
      Doc.empty
  in
  let variable =
    match Syn.Cst.TypeParameter.type_variable parameter with
    | Some type_variable ->
        Doc.text (Syn.Cst.TypeVariable.text type_variable)
    | None ->
        Doc.text "_"
  in
  Doc.concat [ variance; injective; variable ]

let render_type_parameters = fun parameters ->
  match parameters with
  | [] ->
      Doc.empty
  | [ parameter ] ->
      render_type_parameter parameter
  | parameters when List.length parameters > 6 ->
      Doc.concat [
        Doc.lparen;
        Doc.line;
        Doc.indent 2 (join_map (Doc.concat [ Doc.comma; Doc.line ]) render_type_parameter parameters);
        Doc.line;
        Doc.rparen
      ]
  | parameters ->
      Doc.concat [
        Doc.lparen;
        join_map (Doc.concat [ Doc.comma; Doc.space ]) render_type_parameter parameters;
        Doc.rparen
      ]

let rec core_type_arrow_arity =
  function
  | Syn.Cst.CoreType.Arrow { result_type; _ } ->
      1 + core_type_arrow_arity result_type
  | Syn.Cst.CoreType.Parenthesized { inner; _ } ->
      core_type_arrow_arity inner
  | _ ->
      0

let rec core_type_has_labeled_arrow =
  function
  | Syn.Cst.CoreType.Arrow { label = Some _; _ } ->
      true
  | Syn.Cst.CoreType.Arrow { result_type; _ } ->
      core_type_has_labeled_arrow result_type
  | Syn.Cst.CoreType.Parenthesized { inner; _ } ->
      core_type_has_labeled_arrow inner
  | _ ->
      false

let rec core_type_prefers_multiline =
  function
  | Syn.Cst.CoreType.Arrow arrow ->
      core_type_arrow_arity (Syn.Cst.CoreType.Arrow arrow) >= 5
      || core_type_prefers_multiline arrow.parameter_type
      || core_type_prefers_multiline arrow.result_type
  | Syn.Cst.CoreType.Tuple { elements; _ } ->
      List.length elements > 3 || List.exists core_type_prefers_multiline elements
  | Syn.Cst.CoreType.PolyVariant _
  | Syn.Cst.CoreType.Record _
  | Syn.Cst.CoreType.Object _ ->
      true
  | Syn.Cst.CoreType.Parenthesized { inner; _ } ->
      core_type_prefers_multiline inner
  | Syn.Cst.CoreType.Alias { type_; _ } ->
      core_type_prefers_multiline type_
  | Syn.Cst.CoreType.Attribute { type_; _ } ->
      core_type_prefers_multiline type_
  | Syn.Cst.CoreType.Constr { arguments; _ } ->
      List.exists core_type_prefers_multiline arguments
  | _ ->
      false

let rec core_type_is_atomic =
  function
  | Syn.Cst.CoreType.Wildcard _
  | Syn.Cst.CoreType.Var _ ->
      true
  | Syn.Cst.CoreType.Constr { arguments = []; _ } ->
      true
  | Syn.Cst.CoreType.Constr { arguments = [ Syn.Cst.CoreType.Tuple { elements; _ } ]; _ } ->
      List.for_all core_type_is_atomic elements
  | Syn.Cst.CoreType.Constr { arguments = [ argument ]; _ } ->
      core_type_is_atomic argument
  | Syn.Cst.CoreType.Parenthesized { inner; _ } ->
      core_type_is_atomic inner
  | Syn.Cst.CoreType.Attribute { type_; _ } ->
      core_type_is_atomic type_
  | _ ->
      false

let record_field_prefers_multiline = fun ~name_token ~field_type ->
  String.length (token_text name_token) > 32 && not (core_type_is_atomic field_type)

let rec render_core_type =
  function
  | Syn.Cst.CoreType.Wildcard { wildcard_token; _ } ->
      doc_of_token wildcard_token
  | Syn.Cst.CoreType.Var { syntax_node; _ } ->
      doc_of_nontrivia_direct_tokens syntax_node
  | Syn.Cst.CoreType.Constr { constructor_path; arguments; _ } ->
      let head = doc_of_ident constructor_path in
      (
        match arguments with
        | [] ->
            head
        | [ Syn.Cst.CoreType.Tuple { elements; _ } ] ->
            Doc.concat [
              Doc.lparen;
              join_map (Doc.concat [ Doc.comma; Doc.space ]) render_core_type elements;
              Doc.rparen;
              Doc.space;
              head
            ]
        | [ argument ] ->
            let argument =
              if core_type_needs_parens_in_application argument then
                Doc.concat [ Doc.lparen; render_core_type argument; Doc.rparen ]
              else
                render_core_type argument
            in
            Doc.concat [ argument; Doc.space; head ]
        | arguments ->
            Doc.concat [
              Doc.lparen;
              join_map (Doc.concat [ Doc.comma; Doc.space ]) render_core_type arguments;
              Doc.rparen;
              Doc.space;
              head
            ]
      )
  | Syn.Cst.CoreType.Class { hash_token; class_path; arguments; _ } ->
      let head = Doc.concat [ doc_of_token hash_token; doc_of_ident class_path ] in
      (
        match arguments with
        | [] ->
            head
        | [ Syn.Cst.CoreType.Tuple { elements; _ } ] ->
            Doc.concat [
              Doc.lparen;
              join_map (Doc.concat [ Doc.comma; Doc.space ]) render_core_type elements;
              Doc.rparen;
              Doc.space;
              head
            ]
        | [ argument ] ->
            let argument =
              if core_type_needs_parens_in_application argument then
                Doc.concat [ Doc.lparen; render_core_type argument; Doc.rparen ]
              else
                render_core_type argument
            in
            Doc.concat [ argument; Doc.space; head ]
        | arguments ->
            Doc.concat [
              Doc.lparen;
              join_map (Doc.concat [ Doc.comma; Doc.space ]) render_core_type arguments;
              Doc.rparen;
              Doc.space;
              head
            ]
      )
  | Syn.Cst.CoreType.Alias { type_; name_token; _ } ->
      let alias_name = token_text name_token in
      let alias_name =
        if String.starts_with ~prefix:"'" alias_name then
          alias_name
        else
          "'" ^ alias_name
      in
      Doc.concat [ render_core_type type_; Doc.space; Doc.text "as"; Doc.space; Doc.text alias_name ]
  | Syn.Cst.CoreType.Attribute { type_; attribute; _ } ->
      Doc.concat [ render_core_type type_; Doc.space; render_attribute attribute ]
  | Syn.Cst.CoreType.Poly { syntax_node; binders; body; _ } ->
      let prefix =
        if poly_type_has_explicit_type_keyword syntax_node then
          Doc.concat [ kw_type; Doc.space ]
        else
          Doc.empty
      in
      Doc.concat [
        prefix;
        join_map (Doc.concat [ Doc.space ]) render_type_binder binders;
        Doc.text ".";
        Doc.space;
        render_core_type body
      ]
  | Syn.Cst.CoreType.Arrow { label; parameter_type; result_type; _ } ->
      let render_arrow_parameter = fun label parameter_type ->
        let parameter_type =
          match parameter_type with
          | Syn.Cst.CoreType.Arrow _ ->
              Doc.concat [ Doc.lparen; render_core_type parameter_type; Doc.rparen ]
          | _ ->
              render_core_type parameter_type
        in
        Doc.concat [ render_arrow_label label; parameter_type ]
      in
      let rec collect = fun params label parameter_type result_type ->
        let params = params @ [ render_arrow_parameter label parameter_type ] in
        match result_type with
        | Syn.Cst.CoreType.Arrow { label; parameter_type; result_type; _ } ->
            collect params label parameter_type result_type
        | result_type ->
            (params, render_core_type result_type)
      in
      let parameters, result = collect [] label parameter_type result_type in
      let parts = parameters @ [ result ] in
      Doc.group (join_map (Doc.concat [ Doc.space; Doc.arrow; Doc.break () ]) (fun doc -> doc) parts)
  | Syn.Cst.CoreType.Tuple { elements; _ } ->
      Doc.group (join_map (Doc.concat [ Doc.space; star; Doc.break ~flat:" " () ]) render_core_type elements)
  | Syn.Cst.CoreType.Parenthesized { inner; _ } ->
      Doc.concat [ Doc.lparen; render_core_type inner; Doc.rparen ]
  | Syn.Cst.CoreType.LocalOpen { module_path; type_; _ } ->
      Doc.concat [ doc_of_ident module_path; Doc.text ".("; render_core_type type_; Doc.rparen ]
  | Syn.Cst.CoreType.PolyVariant poly_variant ->
      render_poly_variant_type poly_variant
  | Syn.Cst.CoreType.Record { fields; _ } ->
      render_record_type fields
  | Syn.Cst.CoreType.FirstClassModule { module_type; _ } ->
      render_first_class_module_type module_type
  | Syn.Cst.CoreType.Object { fields; _ } ->
      render_object_type fields
  | Syn.Cst.CoreType.Extension extension ->
      unsupported_syntax ~context:[ "core_type" ] ~syntax_node:extension.syntax_node
        "core-type extensions do not have a structural formatter yet"
and render_record_core_type_field = fun (field : Syn.Cst.record_type_field) ->
  let type_doc = render_core_type field.field_type in
  let separator =
    if
      core_type_prefers_multiline field.field_type
      || record_field_prefers_multiline ~name_token:field.field_name ~field_type:field.field_type
    then
      Doc.line
    else
      Doc.break ()
  in
  let prefix =
    if field.is_mutable then
      Doc.concat [ kw_mutable; Doc.space; doc_of_token field.field_name ]
    else
      doc_of_token field.field_name
  in
  Doc.group (Doc.concat [
    prefix;
    Doc.space;
    Doc.colon;
    Doc.indent 2 (Doc.concat [ separator; type_doc ])
  ])
and render_record_type = fun fields ->
  Doc.concat [
    Doc.lbrace;
    Doc.line;
    Doc.indent 2 (join_map (Doc.concat [ Doc.semi; Doc.line ]) render_record_core_type_field fields);
    Doc.line;
    Doc.rbrace
  ]
and render_record_definition_field = fun (field : Syn.Cst.RecordField.t) ->
  let field_type = Syn.Cst.RecordField.field_type field in
  let type_doc = render_core_type field_type in
  let separator =
    if
      core_type_prefers_multiline field_type
      || record_field_prefers_multiline ~name_token:(Syn.Cst.RecordField.field_name_token field) ~field_type
    then
      Doc.line
    else
      Doc.break ()
  in
  let prefix =
    if Syn.Cst.RecordField.is_mutable field then
      Doc.concat [ kw_mutable; Doc.space; doc_of_token (Syn.Cst.RecordField.field_name_token field) ]
    else
      doc_of_token (Syn.Cst.RecordField.field_name_token field)
  in
  Doc.group (Doc.concat [
    prefix;
    Doc.space;
    Doc.colon;
    Doc.indent 2 (Doc.concat [ separator; type_doc ])
  ])
and render_record_definition_field_entry =
  fun ?source ?(include_trailing_semicolon = true) (field : Syn.Cst.RecordField.t) ->
  let body =
    if include_trailing_semicolon then
      Doc.concat [ render_record_definition_field field; Doc.semi ]
    else
      render_record_definition_field field
  in
  match source with
  | Some source ->
      let owned = Syn.Cst.RecordField.owned_trivia field in
      let leading =
        doc_of_owned_trivia ~source (Syn.Cst.OwnedTrivia.leading owned)
      in
      let trailing =
        doc_of_owned_trivia ~source (Syn.Cst.OwnedTrivia.trailing owned)
      in
      body
      |> doc_with_leading_trivia leading
      |> fun body -> doc_with_trailing_trivia body trailing
  | None ->
      body
and render_record_definition_body_item = fun ?source ->
  function
  | Syn.CstBuilder.RecordField field ->
      render_record_definition_field_entry ?source field
  | Syn.CstBuilder.Comment comment ->
      Doc.text (Syn.Cst.Comment.text comment)
  | Syn.CstBuilder.Docstring docstring ->
      Doc.text (Syn.Cst.Docstring.text docstring)
and render_record_definition = fun ?source fields ->
  let body =
    fields
    |> Syn.CstBuilder.record_field_items_of_fields
    |> List.map (render_record_definition_body_item ?source)
    |> Doc.join Doc.line
  in
  Doc.concat
    [ Doc.lbrace; Doc.line; Doc.indent 2 body; Doc.line; Doc.rbrace ]
and render_inline_record_definition = fun fields ->
  if List.is_empty fields then
    Doc.concat [ Doc.lbrace; Doc.rbrace ]
  else
    Doc.group (Doc.concat [
      Doc.lbrace;
      Doc.indent 2 (Doc.concat [
        Doc.break ~flat:" " ();
        join_map (Doc.concat [ Doc.semi; Doc.break ~flat:" " () ]) render_record_definition_field fields
      ]);
      Doc.break ~flat:" " ();
      Doc.rbrace
    ])
and render_object_type_field = fun (field : Syn.Cst.object_type_field) ->
  Doc.group (Doc.concat [
    doc_of_token field.field_name;
    Doc.space;
    Doc.colon;
    Doc.indent 2 (Doc.concat [ Doc.break (); render_core_type field.field_type ])
  ])
and render_object_type = fun fields ->
  Doc.concat [
    Doc.text "<";
    Doc.line;
    Doc.indent 2 (join_map (Doc.concat [ Doc.semi; Doc.line ]) render_object_type_field fields);
    Doc.line;
    Doc.text ">"
  ]
and render_poly_variant_field =
  function
  | Syn.Cst.RowField.Tag tag ->
      let head = Doc.concat [ Doc.text "`"; doc_of_token tag.tag_name ] in
      (
        match tag.payload_type with
        | None ->
            head
        | Some payload_type ->
            Doc.concat [ head; Doc.space; kw_of; Doc.space; render_core_type payload_type ]
      )
  | Syn.Cst.RowField.Inherit { type_; _ } ->
      render_core_type type_
and render_poly_variant_type = fun ?(field_indent = 2) poly_variant ->
  let open_doc =
    match Syn.Cst.PolyVariant.kind poly_variant with
    | Syn.Cst.PolyVariantBound.Exact ->
        Doc.lbracket
    | Syn.Cst.PolyVariantBound.UpperBound { marker_token } ->
        Doc.concat [ Doc.lbracket; doc_of_token marker_token ]
    | Syn.Cst.PolyVariantBound.LowerBound { marker_token } ->
        Doc.concat [ Doc.lbracket; doc_of_token marker_token ]
  in
  let fields =
    Syn.Cst.PolyVariant.fields poly_variant
    |> List.map
         (fun field -> Doc.concat [ Doc.bar; Doc.space; render_poly_variant_field field ])
  in
  Doc.concat [
    open_doc;
    Doc.line;
    Doc.indent field_indent (Doc.join Doc.line fields);
    Doc.line;
    Doc.rbracket
  ]

let poly_variant_has_inherit_field =
  fun poly_variant ->
    Syn.Cst.PolyVariant.fields poly_variant |> List.exists
      (
        function
        | Syn.Cst.RowField.Inherit _ ->
            true
        | Syn.Cst.RowField.Tag _ ->
            false
      )

let render_type_constraint = fun (constraint_ : Syn.Cst.type_constraint) ->
  Doc.concat [
    kw_constraint;
    Doc.space;
    render_core_type constraint_.left;
    equals;
    render_core_type constraint_.right
  ]

let render_variant_constructor_arguments = fun ?source ?(prefer_multiline_inline_record = false) ->
  function
  | Syn.Cst.ConstructorArguments.Tuple types ->
      Doc.group (join_map (Doc.concat [ Doc.space; star; Doc.break ~flat:" " () ]) render_core_type types)
  | Syn.Cst.ConstructorArguments.Record fields ->
      let fields_have_owned_trivia =
        fields
        |> List.exists
             (fun field ->
               Syn.Cst.RecordField.owned_trivia field
               |> Syn.Cst.OwnedTrivia.is_empty
               |> not)
      in
      let field_items = Syn.CstBuilder.record_field_items_of_fields fields in
      let has_standalone_record_trivia =
        field_items
        |> List.exists
             (function
               | Syn.CstBuilder.RecordField _ ->
                   false
               | Syn.CstBuilder.Comment _
               | Syn.CstBuilder.Docstring _ ->
                   true)
      in
      let source_node =
        match fields with
        | [] ->
            None
        | field :: _ ->
            Syn.Ceibo.Red.SyntaxNode.parent (Syn.Cst.RecordField.syntax_node field)
      in
      (
        match source_node with
        | Some _ when fields_have_owned_trivia
        || has_standalone_record_trivia ->
            Doc.indent 2 (render_record_definition ?source fields)
        | Some _ when prefer_multiline_inline_record ->
            Doc.indent 2 (render_record_definition ?source fields)
        | Some _ ->
            Doc.indent 2 (render_inline_record_definition fields)
        | None ->
            Doc.indent 2 (render_record_definition ?source fields)
      )

let render_variant_constructor = fun ?source ?(prefer_multiline_inline_record = false) constructor ->
  let head = Doc.concat [
    Doc.bar;
    Doc.space;
    doc_of_token (Syn.Cst.VariantConstructor.constructor_name_token constructor)
  ] in
  let body =
    match Syn.Cst.VariantConstructor.arguments constructor, Syn.Cst.VariantConstructor.result_type constructor with
    | Some arguments, Some result_type ->
        let payload =
          render_variant_constructor_arguments ?source
            ~prefer_multiline_inline_record arguments
        in
        Doc.concat
        [ head; Doc.space; Doc.colon; Doc.space; payload; arrow; render_core_type result_type ]
    | Some arguments, None ->
        Doc.concat [
          head;
          Doc.space;
          kw_of;
          Doc.space;
          render_variant_constructor_arguments ?source
            ~prefer_multiline_inline_record arguments
        ]
    | None, Some result_type ->
        Doc.concat [ head; Doc.space; Doc.colon; Doc.space; render_core_type result_type ]
    | None, None ->
        head
  in
  match source with
  | Some source ->
      let owned = Syn.Cst.VariantConstructor.owned_trivia constructor in
      let leading =
        doc_of_owned_trivia ~source (Syn.Cst.OwnedTrivia.leading owned)
      in
      let trailing =
        Syn.Cst.OwnedTrivia.trailing owned
        |> doc_of_owned_trivia ~source
             ~start:
               ((nontrivia_bounds_span_of_syntax_node
                   (Syn.Cst.VariantConstructor.syntax_node constructor)).end_)
      in
      let body = body |> doc_with_leading_trivia leading in
      (match trailing with
      | None ->
          body
      | Some suffix ->
          Doc.concat [ body; suffix ])
  | None ->
      body

let render_variant_definition = fun ?source ~source_node:_ constructors ->
  let constructors_all_inline_records =
    not (List.is_empty constructors)
    && List.for_all
      (fun constructor ->
        match Syn.Cst.VariantConstructor.arguments constructor with
        | Some (Syn.Cst.ConstructorArguments.Record _) ->
            true
        | _ ->
            false)
      constructors
  in
  let constructor_docs = constructors
  |> List.map (fun constructor ->
    render_variant_constructor
      ?source
      ~prefer_multiline_inline_record:constructors_all_inline_records
      constructor) in
  constructor_docs |> Doc.join Doc.line

let render_type_definition = fun ?source ->
  function
  | Syn.Cst.TypeDefinition.Abstract ->
      None
  | Syn.Cst.TypeDefinition.Alias { manifest; _ } ->
      Some (render_core_type manifest)
  | Syn.Cst.TypeDefinition.Record { fields; _ } ->
      Some (render_record_definition ?source fields)
  | Syn.Cst.TypeDefinition.Variant { syntax_node; constructors } ->
      Some
        (render_variant_definition
           ?source
           ~source_node:syntax_node
           constructors)
  | Syn.Cst.TypeDefinition.PolyVariant poly_variant -> (
      match Syn.Cst.PolyVariant.kind poly_variant with
      | Syn.Cst.PolyVariantBound.Exact when not (poly_variant_has_inherit_field poly_variant) ->
          let fields = Syn.Cst.PolyVariant.fields poly_variant
          |> List.map (fun field -> Doc.concat
          [ Doc.bar; Doc.space; render_poly_variant_field field ]) in
          Some (Doc.concat [
            Doc.indent 2 (Doc.concat [ Doc.lbracket; Doc.line; Doc.join Doc.line fields ]);
            Doc.line;
            Doc.rbracket
          ])
      | Syn.Cst.PolyVariantBound.Exact
      | Syn.Cst.PolyVariantBound.UpperBound _
      | Syn.Cst.PolyVariantBound.LowerBound _ ->
          Some (render_poly_variant_type poly_variant)
    )
  | Syn.Cst.TypeDefinition.Extensible _ ->
      Some (Doc.text "..")
  | Syn.Cst.TypeDefinition.FirstClassModule { module_type; _ } ->
      Some (render_first_class_module_type module_type)
  | Syn.Cst.TypeDefinition.Object { fields; _ } ->
      Some (render_object_type fields)

type type_definition_layout =
  | Inline_definition
  | Inline_opening_definition
  | Broken_definition
  | Broken_definition_no_outer_indent

let type_definition_layout = fun decl ->
  match Syn.Cst.TypeDeclaration.type_definition decl with
  | Syn.Cst.TypeDefinition.Record _
  | Syn.Cst.TypeDefinition.Object _ ->
      Inline_opening_definition
  | Syn.Cst.TypeDefinition.PolyVariant poly_variant -> (
      match Syn.Cst.PolyVariant.kind poly_variant with
      | Syn.Cst.PolyVariantBound.Exact ->
          if poly_variant_has_inherit_field poly_variant then
            Inline_opening_definition
          else
            Broken_definition_no_outer_indent
      | Syn.Cst.PolyVariantBound.UpperBound _
      | Syn.Cst.PolyVariantBound.LowerBound _ ->
          Inline_opening_definition
    )
  | Syn.Cst.TypeDefinition.Variant _ ->
      Broken_definition
  | Syn.Cst.TypeDefinition.Alias { manifest; _ } ->
      if core_type_prefers_multiline manifest then
        Broken_definition
      else
        Inline_definition
  | Syn.Cst.TypeDefinition.FirstClassModule _
  | Syn.Cst.TypeDefinition.Extensible _ ->
      Inline_definition
  | Syn.Cst.TypeDefinition.Abstract ->
      Inline_definition

let render_single_type_declaration_with_keyword = fun ctx keyword decl ->
  let type_name = Syn.Cst.TypeDeclaration.type_name decl in
  let type_definition = Syn.Cst.TypeDeclaration.type_definition decl in
  let params = render_type_parameters (Syn.Cst.TypeDeclaration.type_params decl) in
  let keyword =
    if Syn.Cst.TypeDeclaration.is_nonrec decl then
      Doc.concat [ keyword; Doc.space; Doc.text "nonrec" ]
    else
      keyword
  in
  let header =
    if params = Doc.empty then
      Doc.concat [ keyword; Doc.space; doc_of_ident type_name ]
    else
      Doc.concat [ keyword; Doc.space; params; Doc.space; doc_of_ident type_name ]
  in
  let header =
    match Syn.Cst.TypeDeclaration.manifest_alias decl with
    | Some manifest_alias ->
        Doc.concat [ header; equals; render_core_type manifest_alias ]
    | None ->
        header
  in
  let definition =
    match Syn.Cst.TypeDeclaration.private_flag decl with
    | Syn.Cst.PrivateFlag.Public ->
          render_type_definition
            ?source:ctx.source
            type_definition
    | Syn.Cst.PrivateFlag.Private _ ->
          Option.map (fun definition -> Doc.concat [ kw_private; Doc.space; definition ]) (render_type_definition
          ?source:ctx.source
          type_definition)
    in
    let with_definition =
      match definition with
      | None ->
          header
      | Some definition -> (
          match type_definition_layout decl with
          | Inline_definition ->
              Doc.concat [ header; equals; definition ]
          | Inline_opening_definition ->
              Doc.concat [ header; Doc.space; Doc.equal; Doc.space; definition ]
          | Broken_definition ->
              Doc.concat [ header; Doc.space; Doc.equal; Doc.line; Doc.indent 2 definition ]
          | Broken_definition_no_outer_indent ->
              Doc.concat [ header; Doc.space; Doc.equal; Doc.line; definition ]
        )
    in
    let with_constraints = Syn.Cst.TypeDeclaration.constraints decl
    |> List.fold_left (fun acc constraint_ -> Doc.concat
    [ acc; Doc.line; Doc.indent 2 (render_type_constraint constraint_) ]) with_definition in
    with_constraints

let doc_with_type_declaration_owned_trivia = fun ctx decl doc ->
  match ctx.source with
  | None ->
      doc
  | Some source ->
      let owned = Syn.Cst.TypeDeclaration.owned_trivia decl in
      let leading =
        doc_of_owned_trivia ~source (Syn.Cst.OwnedTrivia.leading owned)
      in
      let trailing =
        doc_of_owned_trivia ~source (Syn.Cst.OwnedTrivia.trailing owned)
      in
      let trailing_separator =
        match Syn.Cst.TypeDeclaration.type_definition decl, trailing with
        | Syn.Cst.TypeDefinition.Variant { constructors; _ }, Some _ -> (
            match List.rev constructors with
            | constructor :: _ ->
                let trailing =
                  Syn.Cst.VariantConstructor.owned_trivia constructor
                  |> Syn.Cst.OwnedTrivia.trailing
                in
                if List.is_empty trailing then
                  Doc.line
                else
                  blank_line
            | [] ->
                Doc.line)
        | _ ->
            Doc.line
      in
      doc
      |> doc_with_leading_trivia leading
      |> fun doc ->
           match trailing with
           | None ->
               doc
           | Some trailing ->
               Doc.concat [ doc; trailing_separator; trailing ]

let render_type_declaration_member_with_keyword = fun ctx keyword decl ->
  render_single_type_declaration_with_keyword ctx keyword decl
  |> doc_with_type_declaration_owned_trivia ctx decl

let render_type_declaration_with_keyword = fun ctx keyword decl ->
  let and_declarations = Syn.Cst.TypeDeclaration.and_declarations decl in
  if and_declarations = [] then
    render_type_declaration_member_with_keyword ctx keyword decl
  else
    Doc.join blank_line
      (render_type_declaration_member_with_keyword ctx keyword decl
      :: List.map (render_type_declaration_member_with_keyword ctx kw_and) and_declarations)

let render_type_extension = fun ctx (decl : Syn.Cst.TypeExtension.t) ->
  let params = render_type_parameters (Syn.Cst.TypeExtension.type_params decl) in
  let header =
    if params = Doc.empty then
      Doc.concat
        [
          kw_type;
          Doc.space;
          doc_of_ident (Syn.Cst.TypeExtension.type_name decl);
          Doc.space;
          Doc.text "+=";
        ]
    else
      Doc.concat
        [
          kw_type;
          Doc.space;
          params;
          Doc.space;
          doc_of_ident (Syn.Cst.TypeExtension.type_name decl);
          Doc.space;
          Doc.text "+=";
        ]
  in
  let constructors =
    render_variant_definition ?source:ctx.source
      ~source_node:(Syn.Cst.TypeExtension.syntax_node decl)
      (Syn.Cst.TypeExtension.constructors decl)
  in
  Doc.concat [ header; Doc.line; Doc.indent 2 constructors ]

let render_external_declaration = fun (decl : Syn.Cst.external_declaration) ->
  let primitive_names = decl.primitive_name_tokens |> List.map doc_of_token |> Doc.join Doc.space in
  let attributes =
    match decl.attributes with
    | [] ->
        Doc.empty
    | attributes ->
        Doc.concat [ Doc.space; attributes |> List.map render_attribute |> Doc.join Doc.space ]
  in
  Doc.concat
    [
      kw_external;
      Doc.space;
      doc_of_token decl.name_token;
      Doc.space;
      Doc.colon;
      Doc.space;
      render_core_type decl.type_;
      Doc.space;
      Doc.equal;
      Doc.space;
      primitive_names;
      attributes;
    ]

let doc_with_pattern_attributes = fun pattern doc ->
  match Syn.Cst.Pattern.attributes pattern with
  | [] ->
      doc
  | attributes ->
      Doc.concat [ doc; Doc.space; join_map Doc.space render_attribute attributes ]

let rec render_pattern =
  fun pattern ->
  let doc =
    match pattern with
  | Syn.Cst.Pattern.Identifier { name_token; _ } ->
      doc_of_token name_token
  | Syn.Cst.Pattern.Wildcard _ ->
      Doc.text "_"
  | Syn.Cst.Pattern.Extension extension ->
      unsupported_syntax ~context:[ "pattern" ] ~syntax_node:extension.syntax_node
        "pattern extensions do not have a structural formatter yet"
  | Syn.Cst.Pattern.Literal { literal; _ } ->
      render_literal literal
  | Syn.Cst.Pattern.Lazy { pattern; _ } ->
      Doc.concat [ kw_lazy; Doc.space; render_pattern pattern ]
  | Syn.Cst.Pattern.Constructor { constructor_path; arguments; _ } ->
      let head = doc_of_ident constructor_path in
      (
        match arguments with
        | [] ->
            head
        | arguments ->
            Doc.concat [
              head;
              Doc.space;
              join_map (Doc.concat [ Doc.comma; Doc.space ]) render_pattern arguments
            ]
      )
  | Syn.Cst.Pattern.Operator { operator_tokens; _ } ->
      let operator = operator_tokens |> List.map token_text |> String.concat "" |> Doc.text in
      Doc.concat [ Doc.lparen; Doc.space; operator; Doc.space; Doc.rparen ]
  | Syn.Cst.Pattern.FirstClassModule { binding; module_type; _ } ->
      let binding_doc =
        match binding with
        | Syn.Cst.Named { name_token } ->
            doc_of_token name_token
        | Syn.Cst.Anonymous { wildcard_token } ->
            doc_of_token wildcard_token
      in
      let constraint_doc =
        match module_type with
        | None ->
            Doc.empty
        | Some module_type ->
            unsupported_syntax ~context:[ "pattern"; "first_class_module" ]
              ~syntax_node:(Syn.Cst.ModuleType.syntax_node module_type)
              "typed first-class-module patterns do not have a structural formatter yet"
      in
      Doc.concat
        [ Doc.lparen; kw_module; Doc.space; binding_doc; constraint_doc; Doc.rparen ]
  | Syn.Cst.Pattern.PolyVariantInherit { type_path; _ } ->
      Doc.concat [ hash; doc_of_ident type_path ]
  | Syn.Cst.Pattern.Tuple { elements; _ } ->
      Doc.concat
        [ Doc.lparen; join_map (Doc.concat [ Doc.comma; Doc.space ])
            (fun (element : Syn.Cst.tuple_pattern_element) ->
              match element.label_token with
              | None ->
                  render_pattern element.pattern
              | Some label_token ->
                  Doc.concat [ doc_of_token label_token; render_pattern element.pattern ])
            elements; Doc.rparen ]
  | Syn.Cst.Pattern.List { syntax_node; elements; _ } ->
      if elements = [] then
        Doc.concat [ Doc.lbracket; Doc.rbracket ]
      else
        let edge_space =
          if List.length elements = 1 then
            let text = text_of_syntax_node syntax_node in
            if string_contains_substring text "[ " || string_contains_substring text " ]" then
              " "
            else
              ""
          else
            ""
        in
        Doc.group (Doc.concat [
          Doc.lbracket;
          Doc.indent 2 (Doc.concat [
            Doc.break ~flat:edge_space ();
            join_map (Doc.concat [ Doc.semi; Doc.break ~flat:edge_space () ]) render_pattern elements
          ]);
          Doc.break ~flat:edge_space ();
          Doc.rbracket
        ])
  | Syn.Cst.Pattern.Array { elements; _ } ->
      Doc.concat [
        Doc.text "[|";
        join_map (Doc.concat [ Doc.semi; Doc.space ]) render_pattern elements;
        Doc.text "|]"
      ]
  | Syn.Cst.Pattern.Record { fields; closedness; _ } ->
      let fields =
        fields
        |> List.map
          (fun (field : Syn.Cst.record_pattern_field) ->
            match field.pattern with
            | None ->
                doc_of_ident field.field_path
            | Some pattern ->
                Doc.concat [ doc_of_ident field.field_path; equals; render_pattern pattern ])
      in
      let fields =
        match closedness with
        | Syn.Cst.Closed ->
            fields
        | Syn.Cst.Open _ ->
            fields @ [ Doc.text "_" ]
      in
      if List.length fields > 4 then
        Doc.concat [
          Doc.lbrace;
          Doc.line;
          Doc.indent 2 (join_map (Doc.concat [ Doc.semi; Doc.line ]) (fun doc -> doc) fields);
          Doc.line;
          Doc.rbrace
        ]
      else
        Doc.group (Doc.concat [
          Doc.lbrace;
          Doc.indent 2 (Doc.concat [
            Doc.break ~flat:" " ();
            join_map (Doc.concat [ Doc.semi; Doc.break ~flat:" " () ]) (fun doc -> doc) fields
          ]);
          Doc.break ~flat:" " ();
          Doc.rbrace
        ])
  | Syn.Cst.Pattern.Cons { head; tail; _ } ->
      Doc.concat [ render_pattern head; Doc.space; Doc.text "::"; Doc.space; render_pattern tail ]
  | Syn.Cst.Pattern.Or { alternatives; _ } ->
      join_map (Doc.concat [ Doc.space; Doc.bar; Doc.space ]) render_pattern alternatives
  | Syn.Cst.Pattern.Alias { pattern; name_token; _ } ->
      Doc.concat [ render_pattern pattern; Doc.space; Doc.text "as"; Doc.space; doc_of_token name_token ]
  | Syn.Cst.Pattern.Typed { pattern; type_; _ } ->
      Doc.concat [ Doc.lparen; render_pattern pattern; annotation_colon; render_core_type type_; Doc.rparen ]
  | Syn.Cst.Pattern.Effect { effect_pattern; continuation; _ } ->
      Doc.concat
        [
          Doc.text "effect";
          Doc.space;
          render_pattern effect_pattern;
          Doc.space;
          render_pattern continuation;
        ]
  | Syn.Cst.Pattern.LocalOpen { module_path; pattern; _ } ->
      Doc.concat [ doc_of_ident module_path; Doc.text ".("; render_pattern pattern; Doc.rparen ]
  | Syn.Cst.Pattern.Exception { keyword_token; pattern; _ } ->
      Doc.concat [ doc_of_token keyword_token; Doc.space; render_pattern pattern ]
  | Syn.Cst.Pattern.Range { lower; upper; _ } ->
      Doc.concat [ render_literal lower; Doc.space; Doc.text ".."; Doc.space; render_literal upper ]
  | Syn.Cst.Pattern.Parenthesized { inner; _ } -> (
      match inner with
      | Syn.Cst.Pattern.Identifier { name_token; _ } when is_keyword_operator_name
      (token_text name_token) ->
          Doc.concat [ Doc.lparen; Doc.space; doc_of_token name_token; Doc.space; Doc.rparen ]
      | Syn.Cst.Pattern.Tuple _
      | Syn.Cst.Pattern.List _
      | Syn.Cst.Pattern.Array _
      | Syn.Cst.Pattern.Record _ ->
          render_pattern inner
      | _ ->
          Doc.concat [ Doc.lparen; render_pattern inner; Doc.rparen ]
    )
  | Syn.Cst.Pattern.PolyVariant { syntax_node; payload; _ } ->
      let head = doc_of_nontrivia_direct_tokens syntax_node in
      (
        match payload with
        | None ->
            head
        | Some payload ->
            Doc.concat [ head; Doc.space; render_pattern payload ]
      )
  in
  doc_with_pattern_attributes pattern doc

let pattern_requires_parens_in_named_parameter =
  function
  | Syn.Cst.Pattern.Identifier _
  | Syn.Cst.Pattern.Wildcard _
  | Syn.Cst.Pattern.Record _
  | Syn.Cst.Pattern.Parenthesized _ ->
      false
  | _ ->
      true

let rec pattern_is_simple_function_parameter =
  function
  | Syn.Cst.Pattern.Identifier _
  | Syn.Cst.Pattern.Literal { literal = Syn.Cst.Literal.Unit _; _ }
  | Syn.Cst.Pattern.Wildcard _ ->
      true
  | Syn.Cst.Pattern.Typed { pattern; _ }
  | Syn.Cst.Pattern.Parenthesized { inner = pattern; _ } ->
      pattern_is_simple_function_parameter pattern
  | _ ->
      false

let rec pattern_supports_binding_header_parameters =
  function
  | Syn.Cst.Pattern.Identifier _
  | Syn.Cst.Pattern.Operator _ ->
      true
  | Syn.Cst.Pattern.Parenthesized { inner; _ } ->
      pattern_supports_binding_header_parameters inner
  | Syn.Cst.Pattern.Typed { pattern; _ } ->
      pattern_supports_binding_header_parameters pattern
  | _ ->
      false

let parameters_mix_complex_positional_and_named = fun parameters ->
  let has_named = List.exists Syn.Cst.Parameter.is_named parameters in
  let has_complex_positional =
    List.exists
      (
        function
        | Syn.Cst.Parameter.Positional { pattern; _ } ->
            not (pattern_is_simple_function_parameter pattern)
        | Syn.Cst.Parameter.Labeled _
        | Syn.Cst.Parameter.Optional _
        | Syn.Cst.Parameter.LocallyAbstract _ ->
            false
      )
      parameters
  in
  has_named && has_complex_positional

let is_simple_expression =
  function
  | Syn.Cst.Expression.Path _
  | Syn.Cst.Expression.Literal _
  | Syn.Cst.Expression.Operator _
  | Syn.Cst.Expression.Unreachable _
  | Syn.Cst.Expression.Extension _
  | Syn.Cst.Expression.PolyVariant _ ->
      true
  | Syn.Cst.Expression.Constructor { payload = None; _ } ->
      true
  | _ ->
      false

let expression_needs_parens_in_apply =
  function
  | Syn.Cst.Expression.If _
  | Syn.Cst.Expression.Match _
  | Syn.Cst.Expression.Try _
  | Syn.Cst.Expression.LetOperator _
  | Syn.Cst.Expression.Let _
  | Syn.Cst.Expression.Sequence _
  | Syn.Cst.Expression.Fun _
  | Syn.Cst.Expression.Function _
  | Syn.Cst.Expression.Infix _
  | Syn.Cst.Expression.Coerce _ ->
      true
  | _ ->
      false

let rec expression_needs_parens_in_labeled_argument =
  function
  | Syn.Cst.Expression.Parenthesized {
    inner = (Syn.Cst.Expression.Fun _ | Syn.Cst.Expression.Function _);
    _
  } ->
      false
  | Syn.Cst.Expression.Parenthesized { inner; _ } ->
      expression_needs_parens_in_labeled_argument inner
  | Syn.Cst.Expression.Apply _ ->
      true
  | Syn.Cst.Expression.PolyVariant { payload = Some _; _ } ->
      true
  | expression ->
      expression_needs_parens_in_apply expression

let rec expression_needs_parens_in_constructor =
  function
  | Syn.Cst.Expression.Parenthesized {
    inner = Syn.Cst.Expression.PolyVariant { payload = Some _; _ };
    _
  } ->
      true
  | Syn.Cst.Expression.Parenthesized _ ->
      false
  | Syn.Cst.Expression.PolyVariant { payload = Some _; _ } ->
      true
  | expression ->
      expression_needs_parens_in_apply expression

let expression_requires_spaced_delimited_local_open =
  function
  | Syn.Cst.Expression.Path { path; _ } -> (
      match Syn.Cst.Ident.name path with
      | Some name ->
          is_operator_like_text name
      | None ->
          false
    )
  | _ ->
      false

let expression_needs_multiline_binding =
  function
  | Syn.Cst.Expression.Match _
  | Syn.Cst.Expression.Try _
  | Syn.Cst.Expression.Function _
  | Syn.Cst.Expression.LetOperator _
  | Syn.Cst.Expression.Let _
  | Syn.Cst.Expression.Sequence _ ->
      true
  | _ ->
      false

let rec expression_prefers_multiline_layout =
  function
  | Syn.Cst.Expression.If if_ ->
      if_prefers_multiline_layout if_
  | Syn.Cst.Expression.Match _
  | Syn.Cst.Expression.Try _
  | Syn.Cst.Expression.Function _
  | Syn.Cst.Expression.LetOperator _
  | Syn.Cst.Expression.Let _
  | Syn.Cst.Expression.Sequence _ ->
      true
  | Syn.Cst.Expression.Fun { body = Syn.Cst.Expression body; _ } ->
      expression_prefers_multiline_layout body
  | Syn.Cst.Expression.Fun { body = Syn.Cst.Cases _; _ } ->
      true
  | Syn.Cst.Expression.Parenthesized { grouping = Syn.Cst.BeginEnd; _ } ->
      true
  | Syn.Cst.Expression.Parenthesized { inner; _ } ->
      expression_prefers_multiline_layout inner
  | _ ->
      false
and if_prefers_multiline_layout = fun
  ({ condition; then_branch; else_branch; _ } :
      Syn.Cst.if_expression) ->
  let else_prefers_multiline =
    match else_branch with
    | Some (Syn.Cst.Expression.If _) ->
        false
    | Some else_branch ->
        branch_prefers_multiline_layout else_branch
    | None ->
        false
  in
  expression_prefers_multiline_layout condition || branch_prefers_multiline_layout then_branch || else_prefers_multiline
and branch_prefers_multiline_layout =
  function
  | Syn.Cst.Expression.If if_ ->
      true
  | Syn.Cst.Expression.Parenthesized { inner; _ } ->
      expression_prefers_multiline_layout inner
  | Syn.Cst.Expression.Match _
  | Syn.Cst.Expression.Try _
  | Syn.Cst.Expression.Function _
  | Syn.Cst.Expression.Fun _
  | Syn.Cst.Expression.LetOperator _
  | Syn.Cst.Expression.Let _
  | Syn.Cst.Expression.Sequence _
  | Syn.Cst.Expression.Parenthesized { grouping = Syn.Cst.BeginEnd; _ } ->
      true
  | _ ->
      false

let case_body_prefers_multiline = fun ({ body; _ } : Syn.Cst.match_case) ->
  expression_prefers_multiline_layout body

let rec function_body_prefers_multiline =
  function
  | Syn.Cst.Expression.If if_ ->
      if_prefers_multiline_layout if_
  | Syn.Cst.Expression.Match _
  | Syn.Cst.Expression.Try _
  | Syn.Cst.Expression.LetOperator _
  | Syn.Cst.Expression.Let _
  | Syn.Cst.Expression.Sequence _
  | Syn.Cst.Expression.Parenthesized { grouping = Syn.Cst.BeginEnd; _ } ->
      true
  | Syn.Cst.Expression.Function { cases; _ } ->
      List.exists case_body_prefers_multiline cases
  | Syn.Cst.Expression.Fun { body = Syn.Cst.Expression body; _ } ->
      function_body_prefers_multiline body
  | Syn.Cst.Expression.Fun { body = Syn.Cst.Cases _; _ } ->
      false
  | Syn.Cst.Expression.Apply apply ->
      qualified_multi_argument_apply_prefers_multiline apply
  | Syn.Cst.Expression.Parenthesized { inner; _ } ->
      function_body_prefers_multiline inner
  | _ ->
      false
and qualified_multi_argument_apply_prefers_multiline = fun ({ callee; argument; _ } : Syn.Cst.apply_expression) ->
  let rec argument_count = fun count ->
    function
    | Syn.Cst.Expression.Apply { callee; _ } ->
        argument_count (count + 1) callee
    | _ ->
        count
  in
  let rec head_is_qualified_path =
    function
    | Syn.Cst.Expression.Apply { callee; _ } ->
        head_is_qualified_path callee
    | Syn.Cst.Expression.Path { path; _ } ->
        List.length (Syn.Cst.Ident.segments path) > 1
    | Syn.Cst.Expression.FieldAccess { receiver; _ } -> (
        match receiver with
        | Syn.Cst.Expression.Path _
        | Syn.Cst.Expression.FieldAccess _ ->
            true
        | _ ->
            false
      )
    | _ ->
        false
  in
  let rec has_non_positional_argument = fun acc ->
    function
    | Syn.Cst.Expression.Apply { callee; argument; _ } ->
        let acc =
          acc
          || match argument with
          | Syn.Cst.Positional _ ->
              false
          | Syn.Cst.Labeled _
          | Syn.Cst.Optional _ ->
              true
        in
        has_non_positional_argument acc callee
    | _ ->
        acc
  in
  let acc =
    match argument with
    | Syn.Cst.Positional _ ->
        false
    | Syn.Cst.Labeled _
    | Syn.Cst.Optional _ ->
        true
  in
  argument_count 1 callee > 1
  && head_is_qualified_path callee
  && not (has_non_positional_argument acc callee)

let rec expression_keeps_inline_binding_value =
  function
  | Syn.Cst.Expression.Literal (Syn.Cst.Literal.String _) ->
      true
  | Syn.Cst.Expression.Parenthesized { inner; _ } ->
      expression_keeps_inline_binding_value inner
  | _ ->
      false

let rec expression_is_pipeline =
  function
  | Syn.Cst.Expression.Infix { operator_token; left; right; _ } ->
      token_text operator_token = "|>" || expression_is_pipeline left || expression_is_pipeline right
  | Syn.Cst.Expression.Parenthesized { inner; _ } ->
      expression_is_pipeline inner
  | _ ->
      false

let rec expression_is_boolean_infix =
  function
  | Syn.Cst.Expression.Infix { operator_token; left; right; _ } ->
      let operator = token_text operator_token in
      operator = "&&"
      || operator = "||"
      || expression_is_boolean_infix left
      || expression_is_boolean_infix right
  | Syn.Cst.Expression.Parenthesized { inner; _ } ->
      expression_is_boolean_infix inner
  | _ ->
      false

let rec find_boolean_infix_expression =
  function
  | Syn.Cst.Expression.Infix ({ operator_token; _ } as infix) ->
      let operator = token_text operator_token in
      if operator = "&&" || operator = "||" then
        Some infix
      else
        None
  | Syn.Cst.Expression.Parenthesized { inner; _ } ->
      find_boolean_infix_expression inner
  | Syn.Cst.Expression.Prefix { operand; _ } ->
      find_boolean_infix_expression operand
  | Syn.Cst.Expression.Apply { callee; argument; _ } -> (
      match argument with
      | Syn.Cst.Positional value -> (
          match find_boolean_infix_expression value with
          | Some infix ->
              Some infix
          | None ->
              find_boolean_infix_expression callee
        )
      | Syn.Cst.Labeled { value = Some value; _ }
      | Syn.Cst.Optional { value = Some value; _ } -> (
          match find_boolean_infix_expression value with
          | Some infix ->
              Some infix
          | None ->
              find_boolean_infix_expression callee
        )
      | Syn.Cst.Labeled { value = None; _ }
      | Syn.Cst.Optional { value = None; _ } ->
          find_boolean_infix_expression callee
    )
  | _ ->
      None

let rec expression_is_function_like =
  function
  | Syn.Cst.Expression.Function _ ->
      true
  | Syn.Cst.Expression.Parenthesized { inner; _ } ->
      expression_is_function_like inner
  | _ ->
      false

let rec unwrap_parenthesized_expression =
  function
  | Syn.Cst.Expression.Parenthesized { grouping = Syn.Cst.Parens; inner; _ } ->
      unwrap_parenthesized_expression inner
  | expression ->
      expression

let rec infix_chain_term_count =
  function
  | Syn.Cst.Expression.Infix { left; right; _ } ->
      infix_chain_term_count left + infix_chain_term_count right
  | _ ->
      1

let rec expression_is_simple_after_equals =
  function
  | Syn.Cst.Expression.Infix infix ->
      infix_chain_term_count (Syn.Cst.Expression.Infix infix) <= 8
  | Syn.Cst.Expression.Path _
  | Syn.Cst.Expression.Literal _
  | Syn.Cst.Expression.Operator _
  | Syn.Cst.Expression.Unreachable _
  | Syn.Cst.Expression.Extension _
  | Syn.Cst.Expression.Constructor _
  | Syn.Cst.Expression.PolyVariant _
  | Syn.Cst.Expression.Prefix _
  | Syn.Cst.Expression.Tuple _
  | Syn.Cst.Expression.List _
  | Syn.Cst.Expression.Array _
  | Syn.Cst.Expression.Record _
  | Syn.Cst.Expression.FieldAccess _
  | Syn.Cst.Expression.Index _
  | Syn.Cst.Expression.Coerce _
  | Syn.Cst.Expression.Typed _
  | Syn.Cst.Expression.MethodCall _
  | Syn.Cst.Expression.New _
  | Syn.Cst.Expression.LocalOpen _ ->
      true
  | Syn.Cst.Expression.Apply apply ->
      apply_expression_is_simple_after_equals apply
  | Syn.Cst.Expression.Parenthesized { inner; _ } ->
      expression_is_simple_after_equals inner
  | _ ->
      false
and apply_argument_is_simple_after_equals =
  function
  | Syn.Cst.Positional value ->
      expression_is_simple_after_equals value
  | Syn.Cst.Labeled { value = Some value; _ }
  | Syn.Cst.Optional { value = Some value; _ } ->
      expression_is_simple_after_equals value
  | Syn.Cst.Labeled { value = None; _ }
  | Syn.Cst.Optional { value = None; _ } ->
      true
and apply_expression_is_simple_after_equals =
  fun ({ syntax_node; callee; argument; _ } : Syn.Cst.apply_expression) ->
  not (Source.syntax_node_has_comment_like_trivia syntax_node)
  && expression_is_simple_after_equals callee
  && apply_argument_is_simple_after_equals argument

let expression_requires_break_after_equals =
  function
  | Syn.Cst.Expression.Function _
  | Syn.Cst.Expression.If _
  | Syn.Cst.Expression.Match _
  | Syn.Cst.Expression.Try _
  | Syn.Cst.Expression.While _
  | Syn.Cst.Expression.For _
  | Syn.Cst.Expression.LetOperator _
  | Syn.Cst.Expression.Let _
  | Syn.Cst.Expression.Sequence _
  | Syn.Cst.Expression.LetModule _
  | Syn.Cst.Expression.LocalOpen _ ->
      true
  | _ ->
      false

let expression_can_use_delimited_local_open_sugar =
  function
  | Syn.Cst.Expression.List _
  | Syn.Cst.Expression.Array _
  | Syn.Cst.Expression.Record _
  | Syn.Cst.Expression.Tuple _
  | Syn.Cst.Expression.Parenthesized _ ->
      true
  | _ ->
      false

let rec collapse_redundant_parenthesized_expression =
  function
  | Syn.Cst.Expression.Parenthesized { grouping = Syn.Cst.Parens; inner; _ } ->
      collapse_redundant_parenthesized_expression inner
  | Syn.Cst.Expression.Operator _ ->
      None
  | Syn.Cst.Expression.Prefix { operator_token; operand = Syn.Cst.Expression.Literal literal; _ } when let operator = token_text
  operator_token in
  operator = "-" || operator = "~-" ->
      Some (`NegativeLiteral literal)
  | expression when is_simple_expression expression ->
      Some (`Expression expression)
  | _ ->
      None

let infix_chain = fun operator expression ->
  let rec collect = fun acc ->
    function
    | Syn.Cst.Expression.Infix { left; operator_token; right; _ } when token_text operator_token = operator ->
        collect (collect acc left) right
    | expression ->
        acc @ [ expression ]
  in
  collect [] expression

let doc_with_expression_attributes = fun expression doc ->
  match Syn.Cst.Expression.attributes expression with
  | [] ->
      doc
  | attributes ->
      Doc.concat [ doc; Doc.space; join_map Doc.space render_attribute attributes ]

type lowerer = {
  render_structure_items :
    ?source:string -> source_node:Syn.Cst.syntax_node -> Syn.Cst.StructureItem.t list -> Doc.t;
  render_signature_items :
    ?source:string -> source_node:Syn.Cst.syntax_node -> Syn.Cst.SignatureItem.t list -> Doc.t;
}

let make_lowerer ctx =
  let render_type_declaration_with_keyword keyword decl =
    render_type_declaration_with_keyword ctx keyword decl
  in
  let rec render_expression expression =
  let doc =
    match expression with
  | Syn.Cst.Expression.Path { path; _ } ->
      doc_of_ident path
  | Syn.Cst.Expression.Extension extension ->
      unsupported_syntax ~context:[ "expression" ] ~syntax_node:extension.syntax_node
        "extension expressions do not have a structural formatter yet"
  | Syn.Cst.Expression.Unreachable unreachable ->
      unsupported_syntax ~context:[ "expression" ] ~syntax_node:unreachable.syntax_node
        "unreachable expressions do not have a structural formatter yet"
  | Syn.Cst.Expression.Literal literal ->
      render_literal literal
  | Syn.Cst.Expression.Object object_ ->
      unsupported_syntax ~context:[ "expression" ] ~syntax_node:object_.syntax_node
        "object expressions do not have a structural formatter yet"
  | Syn.Cst.Expression.Constructor { constructor_path; payload; _ } ->
      let head = doc_of_ident constructor_path in
      (match payload with
      | None ->
          head
      | Some payload ->
          let payload =
            if expression_needs_parens_in_constructor payload then
              Doc.concat [ Doc.lparen; render_expression payload; Doc.rparen ]
            else
              render_expression payload
          in
          Doc.concat [ head; Doc.space; payload ])
  | Syn.Cst.Expression.ModulePack { module_expression; module_type; _ } ->
      let constraint_doc =
        match module_type with
        | None ->
            Doc.empty
        | Some module_type ->
            Doc.concat [ colon; render_module_type_doc module_type ]
      in
      Doc.concat
        [
          Doc.lparen;
          kw_module;
          Doc.space;
          render_module_expression_doc module_expression;
          constraint_doc;
          Doc.rparen;
        ]
  | Syn.Cst.Expression.Assert { asserted; _ } ->
      Doc.concat [ kw_assert; Doc.space; render_expression asserted ]
  | Syn.Cst.Expression.Lazy { body; _ } ->
      Doc.concat [ kw_lazy; Doc.space; render_expression body ]
  | Syn.Cst.Expression.While { condition; body; _ } ->
      Doc.concat
        [
          kw_while;
          Doc.space;
          render_expression condition;
          Doc.space;
          kw_do;
          Doc.line;
          Doc.indent 2 (render_block_expression body);
          Doc.line;
          kw_done;
        ]
  | Syn.Cst.Expression.For { iterator_token; start_expr; direction; end_expr; body; _ } ->
      let direction_doc =
        match direction with
        | Syn.Cst.To { direction_token }
        | Syn.Cst.Downto { direction_token } ->
            doc_of_token direction_token
      in
      Doc.concat
        [
          kw_for;
          Doc.space;
          doc_of_token iterator_token;
          equals;
          render_expression start_expr;
          Doc.space;
          direction_doc;
          Doc.space;
          render_expression end_expr;
          Doc.space;
          kw_do;
          Doc.line;
          Doc.indent 2 (render_block_expression body);
          Doc.line;
          kw_done;
        ]
  | Syn.Cst.Expression.Operator { operator_tokens; _ } ->
      let operator = operator_tokens |> List.map token_text |> String.concat "" |> Doc.text in
      Doc.concat [ Doc.lparen; Doc.space; operator; Doc.space; Doc.rparen ]
  | Syn.Cst.Expression.Tuple { elements; _ } ->
      let rendered_elements = List.map render_expression elements in
      let prefers_multiline = List.exists Doc.is_multiline rendered_elements in
      if prefers_multiline then
        let lines = join_map (Doc.concat [ Doc.comma; Doc.line ]) render_expression elements in
        Doc.concat
          [
            Doc.lparen;
            Doc.line;
            Doc.indent 2 lines;
            Doc.line;
            Doc.rparen;
          ]
      else
        Doc.group
          (Doc.concat
             [
               Doc.lparen;
               Doc.indent 2
                 (Doc.concat
                    [
                      Doc.break ~flat:"" ();
                      join_map (Doc.concat [ Doc.comma; Doc.break () ]) render_expression
                        elements;
                    ]);
               Doc.break ~flat:"" ();
               Doc.rparen;
             ])
  | Syn.Cst.Expression.List { syntax_node; elements; _ } ->
      if elements = [] then
        Doc.concat [ Doc.lbracket; Doc.rbracket ]
      else if List.length elements >= multiline_list_threshold then
        render_multiline_list_expression elements
      else
        Doc.group
          (Doc.concat
             [
               Doc.lbracket;
               Doc.indent 2
                 (Doc.concat
                    [
                      Doc.break ~flat:" " ();
                      join_map (Doc.concat [ Doc.semi; Doc.break () ]) render_expression
                        elements;
                    ]);
               Doc.break ~flat:" " ();
               Doc.rbracket;
             ])
  | Syn.Cst.Expression.Array { elements; _ } ->
      Doc.group
        (Doc.concat
           [
             Doc.text "[|";
             Doc.indent 2
               (Doc.concat
                  [
                    Doc.break ~flat:"" ();
                    join_map (Doc.concat [ Doc.semi; Doc.break () ]) render_expression
                      elements;
                  ]);
             Doc.break ~flat:"" ();
             Doc.text "|]";
           ])
  | Syn.Cst.Expression.Parenthesized { inner; _ } ->
      render_parenthesized_expression expression
  | Syn.Cst.Expression.Prefix { operator_token; operand; _ } ->
      let operator = token_text operator_token in
      (match operand with
      | Syn.Cst.Expression.Literal literal when operator = "-" || operator = "~-" ->
          Doc.concat [ Doc.lparen; Doc.text "-"; render_literal literal; Doc.rparen ]
      | _ ->
          let operand_doc =
            match operand with
            | _ when expression_needs_parens_in_apply operand ->
                Doc.concat [ Doc.lparen; render_expression operand; Doc.rparen ]
            | _ ->
                render_expression operand
          in
          Doc.concat [ Doc.text operator; operand_doc ])
  | Syn.Cst.Expression.FieldAssign { target; operator_token; value; _ } ->
      Doc.concat
        [
          render_expression (Syn.Cst.Expression.FieldAccess target);
          Doc.space;
          doc_of_token operator_token;
          Doc.space;
          render_expression value;
        ]
  | Syn.Cst.Expression.Assign { target; operator_token; value; _ } ->
      Doc.concat
        [
          render_expression target;
          Doc.space;
          doc_of_token operator_token;
          Doc.space;
          render_expression value;
        ]
  | Syn.Cst.Expression.Infix infix ->
      render_infix_expression infix
  | Syn.Cst.Expression.Apply apply ->
      render_apply_expression apply
  | Syn.Cst.Expression.If if_ ->
      render_if_expression if_
  | Syn.Cst.Expression.Match match_ ->
      render_match_expression ~keyword_token:match_.keyword_token
        ~scrutinee:match_.scrutinee ~with_token:match_.with_token
        ~cases:match_.cases
  | Syn.Cst.Expression.Try try_ ->
      render_match_expression ~keyword_token:try_.keyword_token
        ~scrutinee:try_.body ~with_token:try_.with_token ~cases:try_.cases
  | Syn.Cst.Expression.Function function_ ->
      render_function_expression function_
  | Syn.Cst.Expression.Fun fun_ ->
      render_fun_expression fun_
  | Syn.Cst.Expression.LetOperator let_operator ->
      render_let_operator_expression let_operator
  | Syn.Cst.Expression.Let let_ ->
      render_let_expression let_
  | Syn.Cst.Expression.LetException let_exception ->
      render_let_exception_expression let_exception
  | Syn.Cst.Expression.LetModule let_module ->
      render_let_module_expression let_module
  | Syn.Cst.Expression.LocalOpen local_open ->
      render_local_open_expression local_open
  | Syn.Cst.Expression.Sequence sequence ->
      render_sequence_expression sequence
  | Syn.Cst.Expression.Record record ->
      render_record_expression record
  | Syn.Cst.Expression.MethodCall { receiver; method_name; _ } ->
      let receiver =
        match receiver with
        | Syn.Cst.Expression.If _
        | Syn.Cst.Expression.Match _
        | Syn.Cst.Expression.Try _
        | Syn.Cst.Expression.LetOperator _
        | Syn.Cst.Expression.Let _
        | Syn.Cst.Expression.Sequence _
        | Syn.Cst.Expression.Fun _
        | Syn.Cst.Expression.Function _ ->
            Doc.concat [ Doc.lparen; render_expression receiver; Doc.rparen ]
        | _ ->
            render_expression receiver
      in
      Doc.concat [ receiver; hash; doc_of_token method_name ]
  | Syn.Cst.Expression.New { class_path; _ } ->
      Doc.concat [ kw_new; Doc.space; doc_of_ident class_path ]
  | Syn.Cst.Expression.ObjectOverride override ->
      render_object_override_expression override
  | Syn.Cst.Expression.InstanceVariableAssign assign ->
      Doc.concat
        [
          doc_of_token assign.name_token;
          Doc.space;
          doc_of_token assign.operator_token;
          Doc.space;
          render_expression assign.value;
        ]
  | Syn.Cst.Expression.FieldAccess { receiver; field_name; _ } ->
      let receiver =
        match receiver with
        | Syn.Cst.Expression.If _
        | Syn.Cst.Expression.Match _
        | Syn.Cst.Expression.Try _
        | Syn.Cst.Expression.LetOperator _
        | Syn.Cst.Expression.Let _
        | Syn.Cst.Expression.Sequence _
        | Syn.Cst.Expression.Fun _
        | Syn.Cst.Expression.Function _ ->
            Doc.concat [ Doc.lparen; render_expression receiver; Doc.rparen ]
        | _ ->
            render_expression receiver
      in
      Doc.concat [ receiver; Doc.text "."; doc_of_token field_name ]
  | Syn.Cst.Expression.Index index ->
      render_index_expression index
  | Syn.Cst.Expression.Typed { expression; type_; _ } ->
      Doc.concat
        [ Doc.lparen; render_expression expression; annotation_colon; render_core_type type_; Doc.rparen ]
  | Syn.Cst.Expression.Polymorphic { expression; type_; _ } ->
      Doc.concat
        [ Doc.lparen; render_expression expression; annotation_colon; render_core_type type_; Doc.rparen ]
  | Syn.Cst.Expression.Coerce { expression; from_type; to_type; _ } ->
      let from_doc =
        match from_type with
        | None ->
            Doc.empty
        | Some from_type ->
            Doc.concat [ annotation_colon; render_core_type from_type ]
      in
      Doc.concat
        [
          Doc.lparen;
          render_expression expression;
          from_doc;
          Doc.space;
          coercion_arrow;
          Doc.space;
          render_core_type to_type;
          Doc.rparen;
        ]
  | Syn.Cst.Expression.PolyVariant { syntax_node; payload; _ } ->
      let head = doc_of_nontrivia_direct_tokens syntax_node in
      (match payload with
      | None ->
          head
      | Some payload ->
          let payload =
            if expression_needs_parens_in_constructor payload then
              Doc.concat [ Doc.lparen; render_expression payload; Doc.rparen ]
            else
              render_expression payload
          in
          Doc.concat [ head; Doc.space; payload ])
  in
  doc_with_expression_attributes expression doc

and render_record_field (field : Syn.Cst.record_expression_field) =
  match field.source with
  | Syn.Cst.Punned ->
      doc_of_ident field.field_path
  | Syn.Cst.Explicit ->
      Doc.concat [ doc_of_ident field.field_path; equals; render_expression field.value ]

and render_object_override_expression ({ fields; _ } : Syn.Cst.object_override_expression) =
  let fields =
    fields
    |> List.map
      (fun ({ field_name; value; _ } : Syn.Cst.object_override_field) ->
        match value with
        | None ->
            doc_of_token field_name
        | Some value ->
            Doc.concat [ doc_of_token field_name; equals; render_expression value ])
  in
  if fields = [] then
    Doc.concat [ object_override_open; object_override_close ]
  else if List.length fields > 4 then
    Doc.concat
      [
        object_override_open;
        Doc.line;
        Doc.indent 2 (join_map (Doc.concat [ Doc.semi; Doc.line ]) (fun doc -> doc) fields);
        Doc.line;
        object_override_close;
      ]
  else
    Doc.group
      (Doc.concat
         [
           object_override_open;
           Doc.indent 2
             (Doc.concat
                [
                  Doc.break ~flat:" " ();
                  join_map (Doc.concat [ Doc.semi; Doc.break ~flat:" " () ]) (fun doc -> doc)
                    fields;
                ]);
           Doc.break ~flat:" " ();
           object_override_close;
         ])

and render_index_expression ({ syntax_node; collection; index; _ } : Syn.Cst.index_expression) =
  let collection_doc =
    match collection with
    | Syn.Cst.Expression.If _
    | Syn.Cst.Expression.Match _
    | Syn.Cst.Expression.Try _
    | Syn.Cst.Expression.LetOperator _
    | Syn.Cst.Expression.Let _
    | Syn.Cst.Expression.Sequence _
    | Syn.Cst.Expression.Fun _
    | Syn.Cst.Expression.Function _ ->
        Doc.concat [ Doc.lparen; render_expression collection; Doc.rparen ]
    | _ ->
        render_expression collection
  in
  let punct =
    nontrivia_direct_tokens syntax_node
    |> List.map Syn.Ceibo.Red.SyntaxToken.text
  in
  let before_index, after_index =
    match List.rev punct with
    | [] ->
        (".(", ")")
    | right :: reversed_left ->
        (List.rev reversed_left |> String.concat "", right)
  in
  Doc.concat
    [
      collection_doc;
      Doc.text before_index;
      render_expression index;
      Doc.text after_index;
    ]

and render_record_expression = function
  | Syn.Cst.RecordExpression.Literal { fields; _ } ->
      Doc.group
        (Doc.concat
           [
             Doc.lbrace;
             Doc.indent 2
               (Doc.concat
                  [
                    Doc.break ~flat:"" ();
                    join_map
                      (Doc.concat [ Doc.semi; Doc.break () ])
                      render_record_field
                      fields;
                  ]);
             Doc.break ~flat:"" ();
             Doc.rbrace;
           ])
  | Syn.Cst.RecordExpression.Update { base; fields; _ } ->
      Doc.group
        (Doc.concat
           [
             Doc.lbrace;
             Doc.indent 2
               (Doc.concat
                  [
                    Doc.break ~flat:"" ();
                    render_expression base;
                    Doc.break ();
                    kw_with;
                    Doc.space;
                    join_map
                      (Doc.concat [ Doc.semi; Doc.break () ])
                      render_record_field
                      fields;
                  ]);
             Doc.break ~flat:"" ();
             Doc.rbrace;
           ])

and render_tuple_expression_bare elements =
  Doc.group
    (join_map (Doc.concat [ Doc.comma; Doc.break () ]) render_expression elements)

and render_preserved_parenthesized_apply_payload
    ({ opening_token; closing_token; inner; _ } : Syn.Cst.parenthesized_expression) =
  let rendered_inner =
    match inner with
    | Syn.Cst.Expression.Tuple { elements; _ } ->
        render_tuple_expression_bare elements
    | _ ->
        render_expression inner
  in
  Doc.concat
    [ doc_of_token opening_token; rendered_inner; doc_of_token closing_token ]

and render_local_open_expression
    ({ module_path; body; via_let_open; _ } : Syn.Cst.local_open_expression) =
  let module_doc = doc_of_ident module_path in
  let body_doc = render_expression body in
  if via_let_open then
    let head =
      Doc.concat
        [
          Doc.text "let";
          Doc.space;
          Doc.text "open";
          Doc.space;
          module_doc;
          Doc.space;
          Doc.text "in";
        ]
    in
    if
      Doc.is_multiline body_doc
      || expression_requires_break_after_equals body
    then
      Doc.concat [ head; Doc.line; Doc.indent 2 body_doc ]
    else
      Doc.concat [ head; Doc.space; body_doc ]
  else if expression_can_use_delimited_local_open_sugar body then
    if Doc.is_multiline body_doc then
      Doc.concat [ module_doc; Doc.text "."; Doc.line; Doc.indent 2 body_doc ]
    else
      Doc.concat [ module_doc; Doc.text "."; body_doc ]
  else if Doc.is_multiline body_doc then
    Doc.concat
      [
        module_doc;
        Doc.text ".(";
        Doc.line;
        Doc.indent 2 body_doc;
        Doc.line;
        Doc.rparen;
      ]
  else if expression_requires_spaced_delimited_local_open body then
    Doc.concat [ module_doc; Doc.text ".("; Doc.space; body_doc; Doc.space; Doc.rparen ]
  else
    Doc.concat [ module_doc; Doc.text ".("; body_doc; Doc.rparen ]

and render_multiline_list_expression elements =
  let body =
    join_map (Doc.concat [ Doc.semi; Doc.line ]) render_expression elements
  in
  Doc.concat
    [
      Doc.lbracket;
      Doc.line;
      Doc.indent 2 (Doc.concat [ body; Doc.semi ]);
      Doc.line;
      Doc.rbracket;
    ]

and render_apply_argument = function
  | Syn.Cst.Positional
      (Syn.Cst.Expression.Function _ as expression) ->
      Doc.concat
        [
          Doc.lparen;
          Doc.line;
          Doc.indent 2 (render_expression expression);
          Doc.line;
          Doc.rparen;
        ]
  | Syn.Cst.Positional
      (Syn.Cst.Expression.Parenthesized
        ({
           grouping = Syn.Cst.Parens;
           inner =
             ( Syn.Cst.Expression.Tuple _
             | Syn.Cst.Expression.PolyVariant { payload = Some _; _ } );
           _;
         } as expression)) ->
      render_preserved_parenthesized_apply_payload expression
  | Syn.Cst.Positional expression ->
      if expression_needs_parens_in_apply expression then
        Doc.concat [ Doc.lparen; render_expression expression; Doc.rparen ]
      else
        render_expression expression
  | Syn.Cst.Labeled { sigil_token; label_token; value; _ } ->
      (match value with
      | None ->
          Doc.concat [ doc_of_token sigil_token; doc_of_token label_token ]
      | Some value ->
          let value =
            match value with
            | Syn.Cst.Expression.Parenthesized
                ({
                   grouping = Syn.Cst.Parens;
                   inner =
                     ( Syn.Cst.Expression.Apply _
                     | Syn.Cst.Expression.Tuple _
                     | Syn.Cst.Expression.PolyVariant { payload = Some _; _ } );
                   _;
                 } as expression) ->
                render_preserved_parenthesized_apply_payload expression
            | _ when expression_needs_parens_in_labeled_argument value ->
                Doc.concat [ Doc.lparen; render_expression value; Doc.rparen ]
            | _ ->
                render_expression value
          in
          Doc.concat
            [
              doc_of_token sigil_token;
              doc_of_token label_token;
              Doc.text ":";
              value;
            ])
  | Syn.Cst.Optional { sigil_token; label_token; value; _ } ->
      (match value with
      | None ->
          Doc.concat [ doc_of_token sigil_token; doc_of_token label_token ]
      | Some value ->
          let value =
            match value with
            | Syn.Cst.Expression.Parenthesized
                ({
                   grouping = Syn.Cst.Parens;
                   inner =
                     ( Syn.Cst.Expression.Apply _
                     | Syn.Cst.Expression.Tuple _
                     | Syn.Cst.Expression.PolyVariant { payload = Some _; _ } );
                   _;
                 } as expression) ->
                render_preserved_parenthesized_apply_payload expression
            | _ when expression_needs_parens_in_labeled_argument value ->
                Doc.concat [ Doc.lparen; render_expression value; Doc.rparen ]
            | _ ->
                render_expression value
          in
          Doc.concat
            [
              doc_of_token sigil_token;
              doc_of_token label_token;
              Doc.text ":";
              value;
            ])

and syntax_node_of_apply_argument = function
  | Syn.Cst.Positional expression ->
      Syn.Cst.Expression.syntax_node expression
  | Syn.Cst.Labeled argument ->
      argument.syntax_node
  | Syn.Cst.Optional argument ->
      argument.syntax_node

and apply_argument_prefers_break = function
  | Syn.Cst.Positional
      (Syn.Cst.Expression.Parenthesized
        {
          inner =
            ( Syn.Cst.Expression.If _
            | Syn.Cst.Expression.Match _
            | Syn.Cst.Expression.Try _
            | Syn.Cst.Expression.LetOperator _
            | Syn.Cst.Expression.Let _
            | Syn.Cst.Expression.Sequence _ );
          _;
        }) ->
      true
  | Syn.Cst.Positional
      (Syn.Cst.Expression.Parenthesized
        { inner = Syn.Cst.Expression.Infix { operator_token; _ }; _ }) ->
      token_text operator_token = "|>"
  | Syn.Cst.Positional expression ->
      expression_prefers_multiline_layout expression
  | Syn.Cst.Labeled { value = Some value; _ }
  | Syn.Cst.Optional { value = Some value; _ } ->
      expression_prefers_multiline_layout value
  | _ ->
      false

and render_infix_expression ({ syntax_node; left; operator_token; right; _ } :
      Syn.Cst.infix_expression) =
  let operator = token_text operator_token in
  let parts =
    infix_chain operator
      (Syn.Cst.Expression.Infix { syntax_node; left; operator_token; right; attributes = [] })
  in
  Doc.group
    (join_map
       (Doc.concat [ Doc.break (); Doc.text operator; Doc.space ])
       render_expression parts)

and render_apply_expression ({ syntax_node; callee; argument; _ } : Syn.Cst.apply_expression) =
  let rec collect_arguments acc = function
    | Syn.Cst.Expression.Apply { callee; argument; _ } ->
        collect_arguments (argument :: acc) callee
    | expression ->
        (expression, acc)
  in
  let head, arguments = collect_arguments [ argument ] callee in
  let rendered_head =
    match head with
    | Syn.Cst.Expression.Parenthesized _ as expression ->
        render_parenthesized_expression expression
    | _ ->
        render_expression head
  in
  let rendered_arguments = arguments |> List.map render_apply_argument in
  let rendered_argument_pairs = List.combine arguments rendered_arguments in
  if
    List.exists
      (fun (argument, doc) -> apply_argument_prefers_break argument || Doc.is_multiline doc)
      rendered_argument_pairs
  then
    let rec split_inline_prefix acc = function
      | (argument, doc) :: rest
        when not (apply_argument_prefers_break argument) && not (Doc.is_multiline doc) ->
          split_inline_prefix (doc :: acc) rest
      | rest ->
          (List.rev acc, rest)
    in
    let inline_arguments, multiline_arguments = split_inline_prefix [] rendered_argument_pairs in
    let head_with_inline_arguments =
      Doc.concat
        (rendered_head
        :: List.map
             (fun argument -> Doc.concat [ Doc.space; argument ])
             inline_arguments)
    in
    (match multiline_arguments with
    | [] ->
        head_with_inline_arguments
    | multiline_arguments ->
        Doc.concat
          [
            head_with_inline_arguments;
            Doc.line;
            Doc.indent 2
              (multiline_arguments
              |> List.map snd
              |> Doc.join Doc.line);
          ])
  else
    Doc.group
      (Doc.concat
         (rendered_head
         :: List.map
              (fun argument ->
                Doc.concat [ Doc.break (); argument ])
              rendered_arguments))

and render_if_expression
    ({ syntax_node; keyword_token; then_token; else_token; condition; then_branch; else_branch; _ } :
      Syn.Cst.if_expression) =
  render_if_expression_block
    {
      syntax_node;
      keyword_token;
      then_token;
      else_token;
      condition;
      then_branch;
      else_branch;
      attributes = [];
    }

and render_case ?(force_multiline_body = false) ?(force_leading_bar = false)
    (case : Syn.Cst.match_case) =
  let body = render_expression case.body in
  let prefix =
    match case.bar_token with
    | Some token ->
        Doc.concat [ doc_of_token token; Doc.space ]
    | None when force_leading_bar ->
        Doc.concat [ Doc.bar; Doc.space ]
    | None ->
        Doc.empty
  in
  let rendered_pattern =
    match case.pattern with
    | Syn.Cst.Pattern.Tuple { elements; _ } ->
        join_map (Doc.concat [ Doc.comma; Doc.space ]) (fun (element : Syn.Cst.tuple_pattern_element) ->
            match element.label_token with
            | None ->
                render_pattern element.pattern
            | Some label_token ->
                Doc.concat [ doc_of_token label_token; render_pattern element.pattern ])
          elements
    | pattern ->
        render_pattern pattern
  in
  let render_branch pattern =
    let guard =
      match case.guard, case.when_token with
      | Some guard, Some when_token ->
          Doc.concat [ Doc.space; doc_of_token when_token; Doc.space; render_expression guard ]
      | Some guard, None ->
          Doc.concat [ Doc.space; kw_when; Doc.space; render_expression guard ]
      | None, _ ->
          Doc.empty
    in
    match case.body with
    | Syn.Cst.Expression.Parenthesized _ when Doc.is_multiline body ->
        Doc.concat
          [
            prefix;
            pattern;
            guard;
            Doc.space;
            doc_of_token case.arrow_token;
            Doc.space;
            Doc.indent 2 body;
          ]
    | _
      when
        force_multiline_body
        || Doc.is_multiline body
        || expression_prefers_multiline_layout case.body
        ||
        let case_source = text_of_syntax_node case.syntax_node in
        string_contains_substring case_source "->\n"
        || string_contains_substring case_source "->\r\n" ->
      Doc.concat
        [
          prefix;
          pattern;
          guard;
          Doc.space;
          doc_of_token case.arrow_token;
          Doc.line;
          Doc.indent 4 body;
        ]
    | _ ->
        Doc.concat [ prefix; pattern; guard; Doc.space; doc_of_token case.arrow_token; Doc.space; body ]
  in
  match case.pattern with
  | Syn.Cst.Pattern.Or { alternatives; _ } -> (
      match List.rev alternatives with
      | [] ->
          Doc.empty
      | last :: rest_reversed ->
          let leading =
            rest_reversed
            |> List.rev
            |> List.map (fun alternative ->
                   Doc.concat [ prefix; render_pattern alternative; Doc.line ])
          in
          Doc.concat (leading @ [ render_branch (render_pattern last) ]))
  | _ ->
      render_branch rendered_pattern

and render_match_expression ~keyword_token ~scrutinee ~with_token ~cases =
  let force_multiline_cases =
    List.length cases > 2 && List.exists case_body_prefers_multiline cases
  in
  let scrutinee_requires_parens =
    match scrutinee with
    | Syn.Cst.Expression.Coerce _ ->
        true
    | _ ->
        false
  in
  let scrutinee_doc =
    match scrutinee with
    | Syn.Cst.Expression.Tuple { elements; _ } ->
        join_map (Doc.concat [ Doc.comma; Doc.space ]) render_expression elements
    | _ when expression_prefers_multiline_layout scrutinee ->
        render_block_expression scrutinee
    | _ ->
        render_expression scrutinee
  in
  let scrutinee_doc =
    if scrutinee_requires_parens then
      Doc.concat [ Doc.lparen; scrutinee_doc; Doc.rparen ]
    else
      scrutinee_doc
  in
  let head =
    Doc.concat
      [
        doc_of_token keyword_token;
        Doc.space;
        scrutinee_doc;
      ]
  in
  if expression_prefers_multiline_layout scrutinee || Doc.is_multiline scrutinee_doc then
    Doc.concat
      [
        doc_of_token keyword_token;
        Doc.line;
        Doc.indent 2 scrutinee_doc;
        Doc.line;
        doc_of_token with_token;
        Doc.line;
        join_map Doc.line
          (render_case ~force_multiline_body:force_multiline_cases ~force_leading_bar:true)
          cases;
      ]
  else
    Doc.concat
      [
        head;
        Doc.space;
        doc_of_token with_token;
        Doc.line;
        join_map Doc.line
          (render_case ~force_multiline_body:force_multiline_cases ~force_leading_bar:true)
          cases;
      ]

and flatten_fun_expression ({ parameters; body; _ } : Syn.Cst.fun_expression) =
  let rec loop acc = function
    | Syn.Cst.Cases _ as body ->
        (List.rev acc, body)
    | Syn.Cst.Expression (Syn.Cst.Expression.Fun ({ parameters; body; _ } as inner)) ->
        loop (List.rev_append parameters acc) body
    | Syn.Cst.Expression expression ->
        (List.rev acc, Syn.Cst.Expression expression)
  in
  loop (List.rev parameters) body

and render_fun_expression
    ({ keyword_token; arrow_token; parameters = _; body = _; _ } as fun_ :
      Syn.Cst.fun_expression) =
  let parameters = fun_.parameters in
  let body = fun_.body in
  let parameters = parameters |> List.map render_parameter in
  let has_multiline_parameter = List.exists Doc.is_multiline parameters in
  let body = render_fun_body body in
  let body_start =
    match fun_.body with
    | Syn.Cst.Expression expression ->
        (nontrivia_bounds_span_of_syntax_node (Syn.Cst.Expression.syntax_node expression)).start
    | Syn.Cst.Cases cases ->
        (nontrivia_bounds_span_of_syntax_node cases.syntax_node).start
  in
  let body_trivia =
    (match fun_.body with
    | Syn.Cst.Expression expression ->
        render_leading_trivia_before_node ~after:(Syn.Cst.Token.span arrow_token).end_
          (Syn.Cst.Expression.syntax_node expression)
    | Syn.Cst.Cases cases ->
        render_leading_trivia_before_node ~after:(Syn.Cst.Token.span arrow_token).end_
          cases.syntax_node)
  in
  let body = doc_with_leading_trivia body_trivia body in
  let body_prefers_multiline =
    match body with
    | _ when Doc.is_multiline body ->
        true
    | _ ->
        (match fun_.body with
        | Syn.Cst.Expression expression ->
            function_body_prefers_multiline expression
        | Syn.Cst.Cases _ ->
            true)
  in
  if has_multiline_parameter then
    Doc.concat
      [
        doc_of_token keyword_token;
        Doc.line;
        Doc.indent 2
          (Doc.concat [ Doc.join Doc.space parameters; Doc.space; doc_of_token arrow_token ]);
        Doc.line;
        Doc.indent 2 body;
      ]
  else if body_prefers_multiline || List.length parameters = 0 then
    Doc.concat
      [
        doc_of_token keyword_token;
        (if List.length parameters = 0 then Doc.empty else Doc.concat [ Doc.space; Doc.join Doc.space parameters ]);
        Doc.space;
        doc_of_token arrow_token;
        Doc.line;
        Doc.indent 2 body;
      ]
  else
    Doc.concat
      [
        doc_of_token keyword_token;
        (if List.length parameters = 0 then Doc.empty else Doc.concat [ Doc.space; Doc.join Doc.space parameters ]);
        Doc.space;
        doc_of_token arrow_token;
        Doc.space;
        body;
      ]

and render_function_expression ({ keyword_token; cases; _ } : Syn.Cst.function_expression) =
  let force_multiline_cases =
    List.length cases > 2 && List.exists case_body_prefers_multiline cases
  in
  Doc.concat
    [
      doc_of_token keyword_token;
      Doc.line;
      join_map Doc.line
        (render_case ~force_multiline_body:force_multiline_cases ~force_leading_bar:true)
        cases;
    ]

and render_function_expression_inline
    ({ keyword_token; cases; _ } : Syn.Cst.function_expression) =
  let force_multiline_cases =
    List.length cases > 2 && List.exists case_body_prefers_multiline cases
  in
  Doc.concat
    [
      doc_of_token keyword_token;
      Doc.line;
      Doc.indent 2
        (join_map Doc.line
           (render_case ~force_multiline_body:force_multiline_cases ~force_leading_bar:true)
           cases);
    ]

and render_function_expression_unindented
    ({ keyword_token; cases; _ } : Syn.Cst.function_expression) =
  let force_multiline_cases =
    List.length cases > 2 && List.exists case_body_prefers_multiline cases
  in
  Doc.concat
    [
      doc_of_token keyword_token;
      Doc.line;
      join_map Doc.line
        (render_case ~force_multiline_body:force_multiline_cases ~force_leading_bar:true)
        cases;
    ]

and render_function_expression_indented
    ({ keyword_token; cases; _ } : Syn.Cst.function_expression) =
  let force_multiline_cases =
    List.length cases > 2 && List.exists case_body_prefers_multiline cases
  in
  Doc.concat
    [
      doc_of_token keyword_token;
      Doc.line;
      Doc.indent 2
        (join_map Doc.line
           (render_case ~force_multiline_body:force_multiline_cases ~force_leading_bar:true)
           cases);
    ]

and render_fun_body = function
  | Syn.Cst.Expression (Syn.Cst.Expression.Tuple { elements; _ }) ->
      render_tuple_expression_bare elements
  | Syn.Cst.Expression body ->
      if expression_prefers_multiline_layout body then
        render_block_expression body
      else
        render_expression body
  | Syn.Cst.Cases { cases; _ } ->
      let force_multiline_cases =
        List.length cases > 2 && List.exists case_body_prefers_multiline cases
      in
      Doc.concat
        [
          kw_function;
          Doc.line;
          join_map Doc.line
            (render_case ~force_multiline_body:force_multiline_cases ~force_leading_bar:true)
            cases;
        ]

and render_block_expression = function
  | Syn.Cst.Expression.If if_ ->
      render_if_expression_block if_
  | Syn.Cst.Expression.Match match_ ->
      render_match_expression ~keyword_token:match_.keyword_token
        ~scrutinee:match_.scrutinee ~with_token:match_.with_token
        ~cases:match_.cases
  | Syn.Cst.Expression.Try try_ ->
      render_match_expression ~keyword_token:try_.keyword_token
        ~scrutinee:try_.body ~with_token:try_.with_token ~cases:try_.cases
  | Syn.Cst.Expression.LetOperator let_operator ->
      render_let_operator_expression let_operator
  | Syn.Cst.Expression.Let let_ ->
      render_let_expression let_
  | Syn.Cst.Expression.Sequence sequence ->
      render_sequence_expression sequence
  | Syn.Cst.Expression.Function function_ ->
      render_function_expression_unindented function_
  | Syn.Cst.Expression.Fun fun_ ->
      render_fun_expression fun_
  | Syn.Cst.Expression.Parenthesized _ as expression ->
      render_parenthesized_expression expression
  | expression ->
      render_expression expression

and render_if_expression_block
    ({ keyword_token; then_token; else_token; condition; then_branch; else_branch; _ } :
      Syn.Cst.if_expression) =
  let render_condition_with_boolean_breaks condition =
    let syntax_node = Syn.Cst.Expression.syntax_node condition in
    let tokens = Syn.Ceibo.Red.SyntaxNode.tokens syntax_node in
    let has_boolean_operator =
      List.exists
        (fun token ->
          let text = Syn.Ceibo.Red.SyntaxToken.text token in
          text = "&&" || text = "||")
        tokens
    in
    let has_comment_like =
      List.exists
        (fun token ->
          Syn.Ceibo.Red.SyntaxToken.leading_trivia token
          |> List.exists (fun trivia ->
                 match Syn.Ceibo.Red.SyntaxTrivia.kind trivia with
                 | Syn.SyntaxKind.COMMENT
                 | Syn.SyntaxKind.DOCSTRING ->
                     true
                 | _ ->
                     false))
        tokens
    in
    if (not has_boolean_operator) || has_comment_like then
      render_expression condition
    else
      let rec loop acc = function
        | [] ->
            List.rev acc
        | token :: rest ->
            let text = Syn.Ceibo.Red.SyntaxToken.text token in
            if text = "&&" || text = "||" then
              loop
                (Doc.space :: Doc.text text :: Doc.break () :: acc)
                rest
            else
              let has_leading_layout =
                Syn.Ceibo.Red.SyntaxToken.leading_trivia token
                |> List.is_empty
                |> not
              in
              let acc =
                if has_leading_layout then
                  Doc.text text :: Doc.space :: acc
                else
                  Doc.text text :: acc
              in
              loop acc rest
      in
      match tokens with
      | [] ->
          render_expression condition
      | first :: rest ->
          Doc.concat (loop [ Doc.text (Syn.Ceibo.Red.SyntaxToken.text first) ] rest)
  in
  let condition_doc =
    render_condition_with_boolean_breaks condition
  in
  let then_doc =
    if branch_prefers_multiline_layout then_branch then
      render_block_expression then_branch
    else
      render_expression then_branch
  in
  let then_trivia =
    match else_token with
    | None ->
        None
    | Some else_token ->
        render_leading_trivia_before_token
          ~after:
            (nontrivia_bounds_span_of_syntax_node
               (Syn.Cst.Expression.syntax_node then_branch))
              .end_
          (Syn.Cst.Token.syntax_token else_token)
  in
  let then_doc = doc_with_trailing_trivia then_doc then_trivia in
  let head =
    Doc.group
      (Doc.concat
         [
           doc_of_token keyword_token;
           Doc.indent 2 (Doc.concat [ Doc.break (); condition_doc ]);
           Doc.break ();
           doc_of_token then_token;
         ])
  in
  match else_branch, else_token with
  | None, _ -> (
      match then_branch with
      | Syn.Cst.Expression.Sequence { expressions = first :: rest; separator_token; _ } when not (rest = []) ->
          let first_doc =
            Doc.concat [ head; Doc.line; Doc.indent 2 (render_expression first); doc_of_token separator_token ]
          in
          let tail_doc =
            rest
            |> List.mapi (fun index expression ->
                   let suffix =
                     if index < List.length rest - 1 then
                       doc_of_token separator_token
                     else
                       Doc.empty
                   in
                   Doc.concat [ render_expression expression; suffix ])
            |> Doc.join Doc.line
          in
          Doc.concat [ first_doc; Doc.line; tail_doc ]
      | _ ->
          Doc.concat [ head; Doc.line; Doc.indent 2 then_doc ]
    )
  | Some (Syn.Cst.Expression.If nested_if), Some else_token ->
      let else_trivia =
        render_leading_trivia_before_node
          ~after:(Syn.Cst.Token.span else_token).end_
          nested_if.syntax_node
      in
      Doc.concat
        [
          head;
          Doc.line;
          Doc.indent 2 then_doc;
          Doc.line;
          doc_of_token else_token;
          (match else_trivia with
          | None ->
              Doc.space
          | Some _ ->
              Doc.line);
          (match else_trivia with
          | None ->
              render_if_expression_block nested_if
          | Some trivia ->
              Doc.indent 2 (doc_with_leading_trivia (Some trivia) (render_if_expression_block nested_if)));
        ]
      | Some else_branch, Some else_token ->
      let else_doc =
        if branch_prefers_multiline_layout else_branch then
          render_block_expression else_branch
        else
          render_expression else_branch
      in
      let else_trivia =
        render_leading_trivia_before_node
          ~after:(Syn.Cst.Token.span else_token).end_
          (Syn.Cst.Expression.syntax_node else_branch)
      in
      let else_doc = doc_with_leading_trivia else_trivia else_doc in
      (match else_branch with
      | Syn.Cst.Expression.Parenthesized { inner = Syn.Cst.Expression.Sequence _; _ } ->
          Doc.concat
            [
              head;
              Doc.line;
              Doc.indent 2 then_doc;
              Doc.line;
              doc_of_token else_token;
              Doc.space;
              else_doc;
            ]
      | _ ->
          Doc.concat
            [
              head;
              Doc.line;
              Doc.indent 2 then_doc;
              Doc.line;
              doc_of_token else_token;
              Doc.line;
              Doc.indent 2 else_doc;
            ])
  | Some else_branch, None ->
      let else_doc = render_expression else_branch in
      Doc.concat [ head; Doc.line; Doc.indent 2 then_doc; Doc.line; kw_else; Doc.line; Doc.indent 2 else_doc ]

and render_parenthesized_expression = function
  | Syn.Cst.Expression.Parenthesized
      { opening_token; closing_token; grouping; inner; _ } ->
      let rendered_inner = render_expression inner in
      (match grouping with
      | Syn.Cst.BeginEnd ->
          Doc.concat
            [
              doc_of_token opening_token;
              Doc.line;
              Doc.indent 2 rendered_inner;
              Doc.line;
              doc_of_token closing_token;
            ]
      | Syn.Cst.Parens -> (
          match inner with
          | Syn.Cst.Expression.Tuple _
          | Syn.Cst.Expression.List _
          | Syn.Cst.Expression.Array _
          | Syn.Cst.Expression.Record _ ->
              render_expression inner
          | _ when expression_is_function_like inner ->
              Doc.concat
                [
                  doc_of_token opening_token;
                  Doc.line;
                  Doc.indent 2 rendered_inner;
                  Doc.line;
                  doc_of_token closing_token;
                ]
          | _ -> (
              match collapse_redundant_parenthesized_expression inner with
              | Some (`NegativeLiteral literal) ->
                  Doc.concat
                    [
                      doc_of_token opening_token;
                      Doc.text "-";
                      render_literal literal;
                      doc_of_token closing_token;
                    ]
              | Some (`Expression expression) ->
                  render_expression expression
              | None ->
                  if expression_prefers_multiline_layout inner || Doc.is_multiline rendered_inner then
                    (match inner with
                    | Syn.Cst.Expression.Function _
                    | Syn.Cst.Expression.Fun _ ->
                        Doc.concat
                          [
                            doc_of_token opening_token;
                            rendered_inner;
                            doc_of_token closing_token;
                          ]
                    | _ ->
                        Doc.concat
                          [
                            doc_of_token opening_token;
                            Doc.line;
                            Doc.indent 2 rendered_inner;
                            Doc.line;
                            doc_of_token closing_token;
                          ])
                  else
                    Doc.concat
                      [
                        doc_of_token opening_token;
                        rendered_inner;
                        doc_of_token closing_token;
                      ])))
  | expression ->
      render_expression expression

and render_let_module_expression
    ({ module_name_token; module_expression; body; _ } : Syn.Cst.let_module_expression) =
  let header =
    Doc.concat
      [
        kw_let;
        Doc.space;
        kw_module;
        Doc.space;
        doc_of_token module_name_token;
        equals;
        render_module_expression_doc module_expression;
        Doc.space;
        kw_in;
      ]
  in
  Doc.concat [ header; Doc.line; render_expression body ]

and render_exception_declaration (decl : Syn.Cst.exception_declaration) =
  let rhs_doc =
    match decl.rhs with
    | None ->
        Doc.empty
    | Some (Syn.Cst.Alias alias) ->
        Doc.concat [ equals; doc_of_ident alias ]
    | Some (Syn.Cst.Payload payload_type) ->
        Doc.concat [ Doc.space; kw_of; Doc.space; render_core_type payload_type ]
  in
  Doc.concat [ Doc.text "exception"; Doc.space; doc_of_token decl.name_token; rhs_doc ]

and render_let_exception_expression
    ({ exception_declaration; body; _ } : Syn.Cst.let_exception_expression) =
  let exception_doc = render_exception_declaration exception_declaration in
  Doc.concat
    [
      kw_let;
      Doc.space;
      exception_doc;
      Doc.space;
      kw_in;
      Doc.line;
      render_expression body;
    ]

and render_binding_operator_binding
    ({ keyword_token; operator_token; equals_token; binding_pattern; bound_value } :
      Syn.Cst.binding_operator_binding) =
  let header =
    Doc.concat
      [
        doc_of_token keyword_token;
        doc_of_token operator_token;
        Doc.space;
        render_pattern binding_pattern;
      ]
  in
  let leading_value_trivia =
    render_leading_trivia_before_node
      ~after:(Syn.Cst.Token.span equals_token).end_
      (Syn.Cst.Expression.syntax_node bound_value)
  in
  let rendered_value =
    render_expression bound_value
    |> doc_with_leading_trivia leading_value_trivia
  in
  let keep_value_after_equals =
    Option.is_none leading_value_trivia
    && not (expression_requires_break_after_equals bound_value)
    && (expression_is_simple_after_equals bound_value
       || expression_keeps_inline_binding_value bound_value)
    && not (Doc.is_multiline rendered_value)
  in
  if keep_value_after_equals then
    Doc.concat [ header; Doc.space; doc_of_token equals_token; Doc.space; rendered_value ]
  else
    Doc.concat [ header; Doc.space; doc_of_token equals_token; Doc.line; Doc.indent 2 rendered_value ]

and render_let_operator_expression
    ({ binding; and_bindings; in_token; body; _ } : Syn.Cst.let_operator_expression) =
  let rendered_bindings =
    render_binding_operator_binding binding
    :: List.map render_binding_operator_binding and_bindings
  in
  let bindings =
    match rendered_bindings with
    | first :: rest ->
        Doc.concat
          (first :: List.map (fun binding -> Doc.concat [ Doc.line; binding ]) rest)
    | [] ->
        Doc.empty
  in
  let last_bound_value =
    match List.rev and_bindings with
    | { bound_value; _ } :: _ ->
        bound_value
    | [] ->
        binding.bound_value
  in
  let body_trivia =
    render_leading_trivia_before_node
      ~after:(Syn.Cst.Token.span in_token).end_
      (Syn.Cst.Expression.syntax_node body)
  in
  let body_doc = render_expression body |> doc_with_leading_trivia body_trivia in
  if Doc.is_multiline bindings then
    Doc.concat [ bindings; Doc.line; doc_of_token in_token; Doc.line; body_doc ]
  else
    Doc.concat [ bindings; Doc.space; doc_of_token in_token; Doc.line; body_doc ]

and render_sequence_expression ({ separator_tokens; expressions; _ } : Syn.Cst.sequence_expression) =
  let expression_count = List.length expressions in
  let separator_token_at = fun index -> List.nth_opt separator_tokens index in
  let rec render_sequence_items previous_expression index = function
    | [] ->
        []
    | expression :: rest ->
        let leading_trivia =
          match previous_expression with
          | None ->
              None
          | Some previous_expression ->
              (match separator_token_at (index - 1) with
              | Some separator_token ->
                  render_leading_trivia_after_token_before_node
                    ~after:
                      (nontrivia_bounds_span_of_syntax_node
                         (Syn.Cst.Expression.syntax_node previous_expression))
                        .end_
                    (Syn.Cst.Token.syntax_token separator_token)
                    (Syn.Cst.Expression.syntax_node expression)
              | None ->
                  render_leading_trivia_before_node
                    ~after:
                      (nontrivia_bounds_span_of_syntax_node
                         (Syn.Cst.Expression.syntax_node previous_expression))
                        .end_
                    (Syn.Cst.Expression.syntax_node expression))
        in
        let suffix =
          match separator_token_at index with
          | Some separator_token ->
              doc_of_token separator_token
          | None ->
              Doc.empty
        in
        Doc.concat
          [ doc_with_leading_trivia leading_trivia (render_expression expression); suffix ]
        :: render_sequence_items (Some expression) (index + 1) rest
  in
  render_sequence_items None 0 expressions |> Doc.join Doc.line

and render_binding_header ~keyword_token ~rec_token pattern =
  let rec_part =
    match rec_token with
    | None ->
        []
    | Some token ->
        [ Doc.space; doc_of_token token ]
  in
  let pattern =
    match pattern with
    | Syn.Cst.Pattern.Tuple { elements; _ } ->
        join_map (Doc.concat [ Doc.comma; Doc.space ])
          (fun (element : Syn.Cst.tuple_pattern_element) ->
            match element.label_token with
            | None ->
                render_pattern element.pattern
            | Some label_token ->
                Doc.concat [ doc_of_token label_token; render_pattern element.pattern ])
          elements
    | _ ->
        render_pattern pattern
  in
  Doc.concat ([ doc_of_token keyword_token ] @ rec_part @ [ Doc.space; pattern ])

and split_typed_binding_value = function
  | Syn.Cst.Expression.Typed { expression; type_; _ } ->
      (expression, Some type_)
  | Syn.Cst.Expression.Polymorphic { expression; type_; _ } ->
      (expression, Some type_)
  | expression ->
      (expression, None)

and split_typed_binding_pattern = function
  | Syn.Cst.Pattern.Typed { pattern; type_; _ } ->
      (pattern, Some type_)
  | pattern ->
      (pattern, None)

and render_positional_parameter_pattern pattern =
  let pattern_doc = render_pattern pattern in
  if pattern_is_simple_function_parameter pattern then
    pattern_doc
  else
    Doc.concat [ Doc.lparen; pattern_doc; Doc.rparen ]

and render_named_parameter_binding_pattern pattern =
  let pattern, type_ = split_typed_binding_pattern pattern in
  let pattern_doc =
    if pattern_requires_parens_in_named_parameter pattern then
      Doc.concat [ Doc.lparen; render_pattern pattern; Doc.rparen ]
    else
      render_pattern pattern
  in
  match type_ with
  | Some type_ ->
      Doc.concat [ pattern_doc; annotation_colon; render_core_type type_ ]
  | None ->
      pattern_doc

and named_parameter_pattern_matches_label ~label_token pattern =
  let pattern, _ = split_typed_binding_pattern pattern in
  match pattern with
  | Syn.Cst.Pattern.Identifier { name_token; _ } ->
      String.equal (token_text name_token) (token_text label_token)
  | _ ->
      false

and render_named_parameter ~sigil_token ~label_token ~binding_pattern =
  match binding_pattern with
  | None ->
      Doc.concat [ doc_of_token sigil_token; doc_of_token label_token ]
  | Some pattern when named_parameter_pattern_matches_label ~label_token pattern ->
      let _, type_ = split_typed_binding_pattern pattern in
      (match type_ with
      | Some _ ->
          Doc.concat [
            doc_of_token sigil_token;
            Doc.lparen;
            render_named_parameter_binding_pattern pattern;
            Doc.rparen;
          ]
      | None ->
          Doc.concat [ doc_of_token sigil_token; doc_of_token label_token ])
  | Some pattern ->
      Doc.concat [
        doc_of_token sigil_token;
        doc_of_token label_token;
        Doc.colon;
        render_named_parameter_binding_pattern pattern;
      ]

and render_optional_parameter_with_default ~sigil_token ~label_token ~binding_pattern
    ~default_value =
  let binding_doc =
    match binding_pattern with
    | Some pattern ->
        render_named_parameter_binding_pattern pattern
    | None ->
        doc_of_token label_token
  in
  Doc.concat [
    doc_of_token sigil_token;
    Doc.lparen;
    binding_doc;
    equals;
    render_expression default_value;
    Doc.rparen;
  ]

and render_arrow_parameter_type_doc parameter_type =
  match parameter_type with
  | Syn.Cst.CoreType.Arrow _ ->
      Doc.concat [ Doc.lparen; render_core_type parameter_type; Doc.rparen ]
  | _ ->
      render_core_type parameter_type

and render_binding_annotation_parameter = function
  | Syn.Cst.Parameter.Positional { pattern; _ } -> (
      match split_typed_binding_pattern pattern with
      | _, Some type_ ->
          Some (render_arrow_parameter_type_doc type_)
      | _, None ->
          None)
  | Syn.Cst.Parameter.Labeled { sigil_token; label_token; binding_pattern; _ } -> (
      match binding_pattern with
      | Some pattern -> (
          match split_typed_binding_pattern pattern with
          | _, Some type_ ->
              Some
                (Doc.concat
                   [
                     doc_of_token label_token;
                     Doc.colon;
                     render_arrow_parameter_type_doc type_;
                   ])
          | _, None ->
              None)
      | None ->
          None)
  | Syn.Cst.Parameter.Optional { sigil_token; label_token; binding_pattern; _ } -> (
      match binding_pattern with
      | Some pattern -> (
          match split_typed_binding_pattern pattern with
          | _, Some type_ ->
              Some
                (Doc.concat
                   [
                     doc_of_token sigil_token;
                     doc_of_token label_token;
                     Doc.colon;
                     render_arrow_parameter_type_doc type_;
                   ])
          | _, None ->
              None)
      | None ->
          None)
  | Syn.Cst.Parameter.LocallyAbstract _ ->
      None

and synthesize_binding_type_annotation parameters result_type =
  let rec collect binders remaining_parameters parameter_docs = function
    | [] ->
        Some (binders, List.rev remaining_parameters, List.rev parameter_docs)
    | Syn.Cst.Parameter.LocallyAbstract { binders = new_binders; _ } :: rest ->
        collect (binders @ new_binders) remaining_parameters parameter_docs rest
    | parameter :: rest -> (
        match render_binding_annotation_parameter parameter with
        | Some parameter_doc ->
            collect binders (parameter :: remaining_parameters)
              (parameter_doc :: parameter_docs) rest
        | None ->
            None)
  in
  match collect [] [] [] parameters with
  | None ->
      None
  | Some (binders, remaining_parameters, parameter_docs) ->
      let result_doc = render_core_type result_type in
      let type_doc =
        Doc.group
          (join_map
             (Doc.concat [ Doc.space; Doc.arrow; Doc.break () ])
             (fun doc -> doc)
             (parameter_docs @ [ result_doc ]))
      in
      let type_doc =
        match binders with
        | [] ->
            type_doc
        | binders ->
            Doc.concat
              [
                kw_type;
                Doc.space;
                join_map (Doc.concat [ Doc.space ]) render_type_binder binders;
                Doc.text ".";
                Doc.space;
                type_doc;
              ]
      in
      Some (type_doc, remaining_parameters)

and render_unsugared_binding_parameter = function
  | Syn.Cst.Parameter.Positional { pattern; _ } ->
      let pattern, _ = split_typed_binding_pattern pattern in
      render_positional_parameter_pattern pattern
  | Syn.Cst.Parameter.Labeled { sigil_token; label_token; binding_pattern; _ } ->
      render_named_parameter ~sigil_token ~label_token ~binding_pattern
  | Syn.Cst.Parameter.Optional
      { sigil_token; label_token; has_default; binding_pattern; default_value; _ } ->
      if has_default then
        match default_value with
        | Some default_value ->
            render_optional_parameter_with_default ~sigil_token ~label_token ~binding_pattern
              ~default_value
        | None ->
            unsupported
              ~context:[ "parameter"; "optional"; "default" ]
              "optional parameter default value missing from CST"
      else
        render_named_parameter ~sigil_token ~label_token ~binding_pattern
  | Syn.Cst.Parameter.LocallyAbstract parameter ->
      render_parameter (Syn.Cst.Parameter.LocallyAbstract parameter)

and render_parameter = function
  | Syn.Cst.Parameter.Positional { pattern; _ } ->
      render_positional_parameter_pattern pattern
  | Syn.Cst.Parameter.Labeled { sigil_token; label_token; binding_pattern; _ } ->
      render_named_parameter ~sigil_token ~label_token ~binding_pattern
  | Syn.Cst.Parameter.Optional
      { sigil_token; label_token; has_default; binding_pattern; default_value; _ } ->
      if has_default then
        match default_value with
        | Some default_value ->
            render_optional_parameter_with_default ~sigil_token ~label_token ~binding_pattern
              ~default_value
        | None ->
            unsupported
              ~context:[ "parameter"; "optional"; "default" ]
              "optional parameter default value missing from CST"
      else
        render_named_parameter ~sigil_token ~label_token ~binding_pattern
  | Syn.Cst.Parameter.LocallyAbstract { binders; _ } ->
      Doc.concat [
        Doc.lparen;
        kw_type;
        Doc.space;
        join_map (Doc.concat [ Doc.space ]) render_type_binder binders;
        Doc.rparen;
      ]

and render_binding_value ~leading_body_trivia ~force_multiline_body ~parameters ~value =
  match parameters with
  | [] ->
      (match value with
      | Syn.Cst.Expression.Fun ({ keyword_token; arrow_token; _ } as fun_)
        when force_multiline_body ->
          let parameters, body = flatten_fun_expression fun_ in
          let parameters = parameters |> List.map render_parameter |> Doc.join Doc.space in
          let body = render_fun_body body in
          Doc.concat
            [
              doc_of_token keyword_token;
              (if parameters = Doc.empty then Doc.empty else Doc.concat [ Doc.space; parameters ]);
              Doc.space;
              doc_of_token arrow_token;
              Doc.line;
              Doc.indent 2 body;
            ]
      | _ ->
          if expression_requires_break_after_equals value then
            render_block_expression value
          else
            render_expression value)
  | parameters ->
      let force_multiline_body =
        force_multiline_body
        || parameters_mix_complex_positional_and_named parameters
      in
      let parameters = parameters |> List.map render_parameter |> Doc.join Doc.space in
      let has_multiline_parameters = Doc.is_multiline parameters in
      let body =
        match value with
        | Syn.Cst.Expression.Tuple { elements; _ } ->
            render_tuple_expression_bare elements
        | _ ->
            render_expression value
      in
      let body = doc_with_leading_trivia leading_body_trivia body in
      if has_multiline_parameters then
        Doc.concat
          [
            kw_fun;
            Doc.line;
            Doc.indent 2 (Doc.concat [ parameters; Doc.space; Doc.arrow ]);
            Doc.line;
            Doc.indent 2 body;
          ]
      else if force_multiline_body || function_body_prefers_multiline value || Doc.is_multiline body then
        Doc.concat
          [
            kw_fun;
            Doc.space;
            parameters;
            Doc.space;
            Doc.arrow;
            Doc.line;
            Doc.indent 2 body;
          ]
      else
        Doc.group
      (Doc.concat
             [
               kw_fun;
               Doc.space;
               parameters;
               Doc.space;
               Doc.arrow;
               Doc.indent 2 (Doc.concat [ Doc.break (); body ]);
             ])

and render_binding_value_with_parameter_doc ~leading_body_trivia ~force_multiline_body
    ~parameter_doc ~value =
  let body =
    match value with
    | Syn.Cst.Expression.Tuple { elements; _ } ->
        render_tuple_expression_bare elements
    | _ ->
        render_expression value
  in
  let body = doc_with_leading_trivia leading_body_trivia body in
  if force_multiline_body || function_body_prefers_multiline value || Doc.is_multiline body then
    Doc.concat
      [
        kw_fun;
        Doc.space;
        parameter_doc;
        Doc.space;
        Doc.arrow;
        Doc.line;
        Doc.indent 2 body;
      ]
  else
    Doc.group
      (Doc.concat
         [
           kw_fun;
           Doc.space;
           parameter_doc;
           Doc.space;
           Doc.arrow;
           Doc.indent 2 (Doc.concat [ Doc.break (); body ]);
         ])

and render_local_binding
    ~local_context
    ~source_has_explicit_fun
    ~keyword_token ~rec_token ~equals_token ~pattern ~parameters ~value =
  let pattern, type_annotation_from_pattern = split_typed_binding_pattern pattern in
  let value, type_annotation = split_typed_binding_value value in
  let type_annotation =
    match type_annotation with
    | Some _ ->
        type_annotation
    | None ->
        type_annotation_from_pattern
  in
  let lifted_parameters, lifted_body_expression =
    match parameters, value, type_annotation with
    | _, _, Some _ ->
        ([], None)
    | [], Syn.Cst.Expression.Fun fun_, None when not source_has_explicit_fun ->
        let fun_parameters, fun_body = flatten_fun_expression fun_ in
        (match fun_body with
        | Syn.Cst.Expression body_expression ->
            (fun_parameters, Some body_expression)
        | Syn.Cst.Cases _ ->
            ([], None))
    | _ ->
        ([], None)
  in
  let parameters = parameters @ lifted_parameters in
  let value =
    match lifted_body_expression with
    | Some body_expression ->
        body_expression
    | None ->
        value
  in
  let rendered_type_annotation, parameters, synthesized_type_annotation =
    match type_annotation with
    | None ->
        (None, parameters, false)
    | Some type_ when parameters = [] ->
        (Some (render_core_type type_), parameters, false)
    | Some type_ -> (
        match synthesize_binding_type_annotation parameters type_ with
        | Some (type_doc, remaining_parameters) ->
            (Some type_doc, remaining_parameters, true)
        | None ->
            (Some (render_core_type type_), parameters, false))
  in
  let header = render_binding_header ~keyword_token ~rec_token pattern in
  let parameter_doc =
    parameters
    |> List.map
         (if synthesized_type_annotation then
            render_unsugared_binding_parameter
          else
            render_parameter)
    |> Doc.join Doc.space
  in
  let keep_header_parameters =
    not (parameters = [])
    && Option.is_some rendered_type_annotation
    && not synthesized_type_annotation
  in
  let header =
    if keep_header_parameters then
      Doc.concat [ header; Doc.space; parameter_doc ]
    else
      header
  in
  let header =
    match rendered_type_annotation with
    | None ->
        header
    | Some type_doc ->
        Doc.concat [ header; colon; type_doc ]
  in
  let force_multiline_body =
    local_context
    &&
    Option.is_some rec_token
    && (expression_prefers_multiline_layout value
       ||
       match value with
       | Syn.Cst.Expression.Fun _ ->
           true
       | _ ->
           false)
  in
  let leading_value_trivia =
    render_leading_trivia_before_node
      ~after:(Syn.Cst.Token.span equals_token).end_
      (Syn.Cst.Expression.syntax_node value)
  in
  let keep_value_after_equals =
    let has_fun_rhs =
      match value with
      | Syn.Cst.Expression.Fun _ ->
          true
      | _ ->
          false
    in
    if Option.is_some leading_value_trivia && (parameters = [] || keep_header_parameters) then
      false
    else if not (parameters = []) && not keep_header_parameters then
      true
    else
      match value with
      | _ when has_fun_rhs ->
          true
      | _ when expression_is_boolean_infix value ->
          false
      | _ when expression_requires_break_after_equals value ->
          false
      | _ ->
          expression_is_simple_after_equals value || expression_keeps_inline_binding_value value
  in
  let rendered_value =
    match value with
    | Syn.Cst.Expression.Function function_
      when parameters = []
           && keep_value_after_equals
           && syntax_node_has_internal_newline function_.syntax_node ->
        render_function_expression_indented function_
    | Syn.Cst.Expression.Function function_
      when parameters = [] && not keep_value_after_equals ->
        render_function_expression_unindented function_
    | _ when not (parameters = []) ->
        if keep_header_parameters then
          if expression_requires_break_after_equals value then
            render_block_expression value
          else
            render_expression value
        else if synthesized_type_annotation then
          render_binding_value_with_parameter_doc ~leading_body_trivia:leading_value_trivia
            ~force_multiline_body ~parameter_doc ~value
        else
          render_binding_value ~leading_body_trivia:leading_value_trivia
            ~force_multiline_body ~parameters ~value
    | _ ->
        render_binding_value ~leading_body_trivia:None ~force_multiline_body ~parameters:[] ~value
  in
  let rendered_value =
    if parameters = [] || keep_header_parameters then
      doc_with_leading_trivia leading_value_trivia rendered_value
    else
      rendered_value
  in
  let keep_value_after_equals =
    match value with
    | Syn.Cst.Expression.Fun _ ->
        keep_value_after_equals
    | _ when expression_is_pipeline value && Doc.is_multiline rendered_value ->
        false
    | _ ->
        keep_value_after_equals
  in
  if keep_value_after_equals then
    Doc.concat
      [
        header;
        Doc.space;
        doc_of_token equals_token;
        Doc.space;
        rendered_value;
      ]
  else if
    syntax_node_has_internal_newline (Syn.Cst.Expression.syntax_node value)
    || not (expression_is_simple_after_equals value)
    || expression_prefers_multiline_layout value
    || Doc.is_multiline rendered_value
  then
    let rendered_value =
      match value with
      | Syn.Cst.Expression.Infix ({ operator_token; _ } as infix)
        when parameters = [] || keep_header_parameters ->
          let operator = token_text operator_token in
          let parts = infix_chain operator (Syn.Cst.Expression.Infix infix) in
          join_map
            (Doc.concat [ Doc.line; Doc.text operator; Doc.space ])
            render_expression
            parts
      | _ ->
          rendered_value
    in
    Doc.concat
      [
        header;
        Doc.space;
        doc_of_token equals_token;
        Doc.line;
        Doc.indent 2 rendered_value;
      ]
  else
    Doc.group
      (Doc.concat
         [
           header;
           Doc.space;
           doc_of_token equals_token;
           Doc.indent 2 (Doc.concat [ Doc.break (); rendered_value ]);
         ])

and render_let_expression
    ({ keyword_token; rec_token; equals_token; binding_pattern; parameters; bound_value; and_bindings; body; in_token; _ } :
      Syn.Cst.let_expression) =
  let first_binding =
    render_local_binding ~local_context:true ~source_has_explicit_fun:false
      ~keyword_token ~rec_token ~equals_token
      ~pattern:binding_pattern
      ~parameters
      ~value:bound_value
  in
  let and_bindings =
    and_bindings
    |> List.map (fun (binding : Syn.Cst.let_binding) ->
           render_local_binding ~local_context:true ~source_has_explicit_fun:false
             ~keyword_token:binding.keyword_token
             ~rec_token:binding.rec_token ~equals_token:binding.equals_token
             ~pattern:binding.binding_pattern ~parameters:binding.parameters
             ~value:binding.value)
  in
  let bindings =
    Doc.concat
      (first_binding :: List.map (fun binding -> Doc.concat [ Doc.line; binding ]) and_bindings)
  in
  let body_trivia =
    render_leading_trivia_before_node
      ~after:(Syn.Cst.Token.span in_token).end_
      (Syn.Cst.Expression.syntax_node body)
  in
  let body_doc = render_expression body |> doc_with_leading_trivia body_trivia in
  if Doc.is_multiline first_binding then
    Doc.concat
      [
        bindings;
        Doc.line;
        doc_of_token in_token;
        Doc.line;
        body_doc;
      ]
  else
    Doc.concat
      [
        bindings;
        Doc.space;
        doc_of_token in_token;
        Doc.line;
        body_doc;
      ]

and render_let_binding_group_item (binding : Syn.Cst.let_binding) =
  let source_has_explicit_fun =
    binding_has_explicit_fun_rhs binding
  in
  render_local_binding ~local_context:false ~keyword_token:binding.keyword_token
    ~source_has_explicit_fun ~rec_token:binding.rec_token
    ~equals_token:binding.equals_token ~pattern:binding.binding_pattern
    ~parameters:binding.parameters ~value:binding.value

and render_let_binding (binding : Syn.Cst.let_binding) =
  let first = render_let_binding_group_item binding in
  let trailing =
    binding.and_bindings
    |> List.map (fun and_binding ->
           Doc.concat [ Doc.line; render_let_binding_group_item and_binding ])
  in
  Doc.concat (first :: trailing)

and nested_structure_items_from_module_expression module_expression =
  match Syn.CstBuilder.structure_items_of_module_expression module_expression with
  | Ok (Some items) ->
      items
  | Ok None ->
      unsupported_syntax ~context:[ "module_expression" ]
        ~syntax_node:(Syn.Cst.ModuleExpression.syntax_node module_expression)
        "nested structure module expressions do not have a structural item stream"
  | Error error ->
      unsupported
        ~context:([ "module_expression"; Syn.SyntaxKind.to_string error.syntax_kind ] @ error.context)
        error.message

and nested_signature_items_from_module_type module_type =
  match Syn.CstBuilder.signature_items_of_module_type module_type with
  | Ok (Some items) ->
      items
  | Ok None ->
      unsupported_syntax ~context:[ "module_type" ]
        ~syntax_node:(Syn.Cst.ModuleType.syntax_node module_type)
        "nested signature module types do not have a structural item stream"
  | Error error ->
      unsupported
        ~context:([ "module_type"; Syn.SyntaxKind.to_string error.syntax_kind ] @ error.context)
        error.message

and render_module_type_constraint ~keyword (constraint_ : Syn.Cst.module_type_constraint) =
  let separator =
    if constraint_.is_destructive then
      Doc.concat [ Doc.space; Doc.text ":="; Doc.space ]
    else
      equals
  in
  Doc.concat
    [
      keyword;
      Doc.space;
      kw_type;
      Doc.space;
      render_core_type constraint_.constrained_type;
      separator;
      render_core_type constraint_.replacement_type;
    ]

and render_functor_parameter ({ name_token; module_type; _ } : Syn.Cst.functor_parameter) =
  Doc.concat
    [
      Doc.lparen;
      doc_of_token name_token;
      colon;
      render_module_type_doc module_type;
      Doc.rparen;
    ]

and render_module_type_doc = function
  | Syn.Cst.ModuleType.Path path ->
      doc_of_ident path
  | Syn.Cst.ModuleType.TypeOf { module_path; _ } ->
      Doc.concat [ kw_module; Doc.space; kw_type; Doc.space; kw_of; Doc.space; doc_of_ident module_path ]
  | (Syn.Cst.ModuleType.Signature { syntax_node; _ } as module_type) ->
      let body =
        render_signature_items ~source_node:syntax_node
          (nested_signature_items_from_module_type module_type)
      in
      Doc.concat
        [
          Doc.text "sig";
          Doc.line;
          Doc.indent 2 body;
          Doc.line;
          Doc.text "end";
        ]
  | Syn.Cst.ModuleType.Functor { parameters; result; _ } ->
      Doc.concat
        [
          Doc.text "functor";
          Doc.space;
          Doc.join Doc.space (List.map render_functor_parameter parameters);
          Doc.space;
          Doc.arrow;
          Doc.space;
          render_module_type_doc result;
        ]
  | Syn.Cst.ModuleType.With { base; constraints; _ } ->
      let first, rest =
        match constraints with
        | [] ->
            (Doc.empty, [])
        | first :: rest ->
            (render_module_type_constraint ~keyword:kw_with first, rest)
      in
      Doc.concat
        (render_module_type_doc base
        :: Doc.space
        :: first
        :: List.map (fun constraint_ ->
               Doc.concat
                 [
                   Doc.space;
                   render_module_type_constraint ~keyword:kw_and constraint_;
                 ])
             rest)
  | Syn.Cst.ModuleType.Parenthesized { inner; _ } ->
      Doc.concat [ Doc.lparen; render_module_type_doc inner; Doc.rparen ]
  | Syn.Cst.ModuleType.Attribute { module_type; attribute; _ } ->
      Doc.concat [ render_module_type_doc module_type; Doc.space; render_attribute attribute ]
  | Syn.Cst.ModuleType.Extension extension ->
      unsupported_syntax ~context:[ "module_type" ] ~syntax_node:extension.syntax_node
        "module-type extensions do not have a structural formatter yet"

and render_module_application_argument = function
  | Syn.Cst.ModuleExpression.Parenthesized { inner; _ } ->
      Doc.concat [ Doc.lparen; render_module_expression_doc inner; Doc.rparen ]
  | argument ->
      Doc.concat [ Doc.lparen; render_module_expression_doc argument; Doc.rparen ]

and render_module_expression_doc = function
  | Syn.Cst.ModuleExpression.Path path ->
      doc_of_ident path
  | (Syn.Cst.ModuleExpression.Structure { syntax_node; item_syntax_nodes = _ } as module_expression) ->
      let body =
        render_structure_items ~source_node:syntax_node
          (nested_structure_items_from_module_expression module_expression)
      in
      Doc.concat
        [
          Doc.text "struct";
          Doc.line;
          Doc.indent 2 body;
          Doc.line;
          Doc.text "end";
        ]
  | Syn.Cst.ModuleExpression.Functor { parameters; body; _ } ->
      Doc.concat
        [
          Doc.text "functor";
          Doc.space;
          Doc.join Doc.space (List.map render_functor_parameter parameters);
          Doc.space;
          Doc.arrow;
          Doc.space;
          render_module_expression_doc body;
        ]
  | Syn.Cst.ModuleExpression.Apply { callee; argument; _ } ->
      Doc.concat
        [
          render_module_expression_doc callee;
          Doc.space;
          render_module_application_argument argument;
        ]
  | Syn.Cst.ModuleExpression.ApplyUnit { callee; _ } ->
      Doc.concat [ render_module_expression_doc callee; Doc.space; Doc.lparen; Doc.rparen ]
  | Syn.Cst.ModuleExpression.Constraint { module_expression; module_type; _ } ->
      Doc.concat
        [
          render_module_expression_doc module_expression;
          colon;
          render_module_type_doc module_type;
        ]
  | Syn.Cst.ModuleExpression.ModuleUnpack { expression; module_type; _ } ->
      let constraint_doc =
        match module_type with
        | None ->
            Doc.empty
        | Some module_type ->
            Doc.concat [ colon; render_module_type_doc module_type ]
      in
      Doc.concat
        [
          Doc.lparen;
          kw_val;
          Doc.space;
          render_expression expression;
          constraint_doc;
          Doc.rparen;
        ]
  | Syn.Cst.ModuleExpression.Parenthesized { inner; _ } ->
      Doc.concat [ Doc.lparen; render_module_expression_doc inner; Doc.rparen ]
  | Syn.Cst.ModuleExpression.Attribute { module_expression; attribute; _ } ->
      Doc.concat [ render_module_expression_doc module_expression; Doc.space; render_attribute attribute ]
  | Syn.Cst.ModuleExpression.Extension extension ->
      unsupported_syntax ~context:[ "module_expression" ] ~syntax_node:extension.syntax_node
        "module-expression extensions do not have a structural formatter yet"

and render_module_declaration_with_keyword keyword_doc
    ({ module_name; functor_parameters; module_type; module_expression; is_destructive_substitution; _ } :
      Syn.Cst.ModuleDeclaration.t) =
  let header =
    Doc.concat
      [
        keyword_doc;
        Doc.space;
        doc_of_token module_name;
        (if functor_parameters = [] then
           Doc.empty
         else
           Doc.concat
             [
               Doc.space;
               Doc.join Doc.space (List.map render_functor_parameter functor_parameters);
             ]);
      ]
  in
  let header =
    match module_type with
    | None ->
        header
    | Some module_type ->
        Doc.concat [ header; colon; render_module_type_doc module_type ]
  in
  match module_expression with
  | None ->
      header
  | Some (Syn.Cst.ModuleExpression.Constraint { module_expression; _ })
    when Option.is_some module_type ->
      let separator =
        if is_destructive_substitution then
          Doc.concat [ Doc.space; Doc.text ":="; Doc.space ]
        else
          equals
      in
      Doc.concat [ header; separator; render_module_expression_doc module_expression ]
  | Some module_expression ->
      let separator =
        if is_destructive_substitution then
          Doc.concat [ Doc.space; Doc.text ":="; Doc.space ]
        else
          equals
      in
      Doc.concat [ header; separator; render_module_expression_doc module_expression ]

and render_recursive_module_declaration (decl : Syn.Cst.RecursiveModuleDeclaration.t) =
  match Syn.Cst.RecursiveModuleDeclaration.declarations decl with
  | [] ->
      Doc.empty
  | first :: rest ->
      Doc.join blank_line
        (render_module_declaration_with_keyword
           (Doc.concat [ kw_module; Doc.space; kw_rec ])
           first
        :: List.map (render_module_declaration_with_keyword kw_and) rest)

and render_module_type_declaration ({ module_type_name; module_type; is_destructive_substitution; _ } :
      Syn.Cst.ModuleTypeDeclaration.t) =
  let header =
    Doc.concat [ kw_module; Doc.space; kw_type; Doc.space; doc_of_token module_type_name ]
  in
  match module_type with
  | None ->
      header
  | Some module_type ->
      let separator =
        if is_destructive_substitution then
          Doc.concat [ Doc.space; Doc.text ":="; Doc.space ]
        else
          equals
      in
      Doc.concat [ header; separator; render_module_type_doc module_type ]

and render_open_target = function
  | Syn.Cst.OpenStatement.Path path ->
      doc_of_ident path
  | Syn.Cst.OpenStatement.ModuleExpression expression ->
      render_module_expression_doc expression

and render_include_statement ({ target; _ } : Syn.Cst.include_statement) =
  let target =
    match target with
    | Syn.Cst.ModuleExpression expression ->
        render_module_expression_doc expression
    | Syn.Cst.ModuleType module_type ->
        render_module_type_doc module_type
  in
  Doc.concat [ Doc.text "include"; Doc.space; target ]

and is_module_alias_structure_item = function
  | Syn.Cst.StructureItem.ModuleDeclaration
      { functor_parameters = []; module_type = None; module_expression = Some (Syn.Cst.ModuleExpression.Path _); _ } ->
      true
  | _ ->
      false

and is_open_structure_item = function
  | item when is_module_alias_structure_item item ->
      true
  | Syn.Cst.StructureItem.OpenStatement _ ->
      true
  | _ ->
      false

and is_open_signature_item = function
  | Syn.Cst.SignatureItem.OpenStatement _ ->
      true
  | _ ->
      false

and trailing_inline_comment_suffix ~source ~source_offset (span : Syn.Ceibo.Span.t) =
  let source_length = String.length source in
  let rec skip_horizontal index =
    if index >= source_length then
      index
    else
      match source.[index] with
      | ' '
      | '\t' ->
          skip_horizontal (index + 1)
      | _ ->
          index
  in
  let rec scan_comment index depth =
    if index >= source_length then
      None
    else if index + 1 < source_length && source.[index] = '(' && source.[index + 1] = '*' then
      scan_comment (index + 2) (depth + 1)
    else if index + 1 < source_length && source.[index] = '*' && source.[index + 1] = ')' then
      if depth = 1 then
        Some (index + 2)
      else
        scan_comment (index + 2) (depth - 1)
    else
      scan_comment (index + 1) depth
  in
  let start = span.end_ - source_offset in
  if start < 0 || start >= source_length then
    None
  else
    let comment_start = skip_horizontal start in
    if
      comment_start + 1 >= source_length
      || not (Char.equal source.[comment_start] '(')
      || not (Char.equal source.[comment_start + 1] '*')
    then
      None
    else
      match scan_comment (comment_start + 2) 1 with
      | None ->
          None
      | Some after_comment ->
          let suffix_end = skip_horizontal after_comment in
          Some
            ( Source.source_between source ~start:start ~end_:suffix_end,
              source_offset + suffix_end )

and leading_inline_comment_between_offsets source ~start ~end_ =
  let source_length = String.length source in
  let start = Int.max 0 (Int.min start source_length) in
  let end_ = Int.max start (Int.min end_ source_length) in
  match
    trailing_inline_comment_suffix ~source ~source_offset:0
      { Syn.Ceibo.Span.start; end_ = start }
  with
  | Some (suffix_text, suffix_end) when suffix_end <= end_ ->
      Some (Doc.text suffix_text, suffix_end)
  | _ ->
      None

and split_leading_inline_comment_source gap_source =
  let source_length = String.length gap_source in
  let rec skip_horizontal index =
    if index >= source_length then
      index
    else
      match gap_source.[index] with
      | ' '
      | '\t' ->
          skip_horizontal (index + 1)
      | _ ->
          index
  in
  let rec scan_comment index depth =
    if index >= source_length || index + 1 >= source_length then
      None
    else if gap_source.[index] = '(' && gap_source.[index + 1] = '*' then
      scan_comment (index + 2) (depth + 1)
    else if gap_source.[index] = '*' && gap_source.[index + 1] = ')' then
      if depth = 1 then
        Some (index + 2)
      else
        scan_comment (index + 2) (depth - 1)
    else
      scan_comment (index + 1) depth
  in
  let comment_start = skip_horizontal 0 in
  if
    comment_start >= source_length
    || comment_start + 1 >= source_length
    || not (Char.equal gap_source.[comment_start] '(')
    || not (Char.equal gap_source.[comment_start + 1] '*')
  then
    (None, gap_source)
  else
    match scan_comment (comment_start + 2) 1 with
    | None ->
        (None, gap_source)
    | Some comment_end ->
        let comment =
          String.sub gap_source comment_start (comment_end - comment_start)
        in
        let remaining =
          if comment_end >= source_length then
            ""
          else
            String.sub gap_source comment_end (source_length - comment_end)
        in
        (Some (Doc.text comment), remaining)

and flatten_top_level_expression_item = function
  | Syn.Cst.StructureItem.Expression (Syn.Cst.Expression.Sequence { expressions; _ }) ->
      expressions
  | Syn.Cst.StructureItem.Expression expression ->
      [ expression ]
  | _ ->
      []

and render_structure_expression_run ~has_trailing_separator items =
  let expressions =
    items
    |> List.concat_map flatten_top_level_expression_item
  in
  let expression_count = List.length expressions in
  expressions
  |> List.mapi (fun index expression ->
         let suffix =
           if index < expression_count - 1 || (has_trailing_separator && index = expression_count - 1)
           then
             Doc.semi
           else
             Doc.empty
         in
         Doc.concat [ render_expression expression; suffix ])
  |> Doc.join Doc.line

and render_structure_item_owned_trivia =
  function
  | Syn.Cst.StructureItem.TypeDeclaration decl ->
      Some (Syn.Cst.TypeDeclaration.owned_trivia decl)
  | Syn.Cst.StructureItem.TypeExtension decl ->
      Some (Syn.Cst.TypeExtension.owned_trivia decl)
  | Syn.Cst.StructureItem.ClassDeclaration decl ->
      Some decl.owned_trivia
  | Syn.Cst.StructureItem.ClassTypeDeclaration decl ->
      Some decl.owned_trivia
  | Syn.Cst.StructureItem.ModuleDeclaration decl ->
      Some (Syn.Cst.ModuleDeclaration.owned_trivia decl)
  | Syn.Cst.StructureItem.RecursiveModuleDeclaration decl ->
      Some (Syn.Cst.RecursiveModuleDeclaration.owned_trivia decl)
  | Syn.Cst.StructureItem.ModuleTypeDeclaration decl ->
      Some (Syn.Cst.ModuleTypeDeclaration.owned_trivia decl)
  | Syn.Cst.StructureItem.OpenStatement stmt ->
      Some (Syn.Cst.OpenStatement.owned_trivia stmt)
  | Syn.Cst.StructureItem.ValueDeclaration decl ->
      Some (Syn.Cst.ValueDeclaration.owned_trivia decl)
  | Syn.Cst.StructureItem.ExternalDeclaration decl ->
      Some decl.owned_trivia
  | Syn.Cst.StructureItem.IncludeStatement stmt ->
      Some stmt.owned_trivia
  | Syn.Cst.StructureItem.ExceptionDeclaration decl ->
      Some decl.owned_trivia
  | Syn.Cst.StructureItem.LetBinding _
  | Syn.Cst.StructureItem.Expression _
  | Syn.Cst.StructureItem.Attribute _
  | Syn.Cst.StructureItem.Extension _
  | Syn.Cst.StructureItem.Docstring _
  | Syn.Cst.StructureItem.Comment _ ->
      None

and render_signature_item_owned_trivia =
  function
  | Syn.Cst.SignatureItem.TypeDeclaration decl ->
      Some (Syn.Cst.TypeDeclaration.owned_trivia decl)
  | Syn.Cst.SignatureItem.TypeExtension decl ->
      Some (Syn.Cst.TypeExtension.owned_trivia decl)
  | Syn.Cst.SignatureItem.ClassDeclaration decl ->
      Some decl.owned_trivia
  | Syn.Cst.SignatureItem.ClassTypeDeclaration decl ->
      Some decl.owned_trivia
  | Syn.Cst.SignatureItem.ModuleDeclaration decl ->
      Some (Syn.Cst.ModuleDeclaration.owned_trivia decl)
  | Syn.Cst.SignatureItem.RecursiveModuleDeclaration decl ->
      Some (Syn.Cst.RecursiveModuleDeclaration.owned_trivia decl)
  | Syn.Cst.SignatureItem.ModuleTypeDeclaration decl ->
      Some (Syn.Cst.ModuleTypeDeclaration.owned_trivia decl)
  | Syn.Cst.SignatureItem.OpenStatement stmt ->
      Some (Syn.Cst.OpenStatement.owned_trivia stmt)
  | Syn.Cst.SignatureItem.ValueDeclaration decl ->
      Some (Syn.Cst.ValueDeclaration.owned_trivia decl)
  | Syn.Cst.SignatureItem.ExternalDeclaration decl ->
      Some decl.owned_trivia
  | Syn.Cst.SignatureItem.IncludeStatement stmt ->
      Some stmt.owned_trivia
  | Syn.Cst.SignatureItem.ExceptionDeclaration decl ->
      Some decl.owned_trivia
  | Syn.Cst.SignatureItem.Attribute _
  | Syn.Cst.SignatureItem.Extension _
  | Syn.Cst.SignatureItem.Docstring _
  | Syn.Cst.SignatureItem.Comment _ ->
      None

and render_structure_entry ~source ~trailing_suffix item =
  let doc =
    let base_doc =
      match item with
      | Syn.Cst.StructureItem.TypeDeclaration decl ->
          render_type_declaration_with_keyword kw_type decl
      | _ ->
          render_structure_item item
    in
    let base_doc =
      match item with
      | Syn.Cst.StructureItem.TypeDeclaration _ ->
          base_doc
      | _ -> (
          match render_structure_item_owned_trivia item with
          | Some owned ->
              let leading =
                doc_of_owned_trivia ~source (Syn.Cst.OwnedTrivia.leading owned)
              in
              let trailing =
                doc_of_owned_trivia ~source (Syn.Cst.OwnedTrivia.trailing owned)
              in
              base_doc
              |> doc_with_leading_trivia leading
              |> fun doc -> doc_with_trailing_trivia doc trailing
          | None ->
              base_doc)
    in
    match trailing_suffix with
    | None ->
        base_doc
    | Some suffix ->
        Doc.concat [ base_doc; suffix ]
  in
  let is_trivia =
    match item with
    | Syn.Cst.StructureItem.Docstring docstring ->
        not (Syn.Cst.Docstring.is_section docstring)
    | Syn.Cst.StructureItem.Comment _ ->
        false
    | _ ->
        false
  in
  let tight_after = false in
  let is_docstring =
    match item with
    | Syn.Cst.StructureItem.Docstring docstring ->
        not (Syn.Cst.Docstring.is_section docstring)
    | _ ->
        false
  in
  (doc, is_open_structure_item item, is_trivia, tight_after, false, is_docstring)

and render_signature_entry ~source ~source_offset ~span ~trailing_suffix item =
  let _span = (span : Syn.Ceibo.Span.t) in
  let _source_offset = source_offset in
  let doc =
    let base_doc =
      match item with
      | Syn.Cst.SignatureItem.TypeDeclaration decl ->
          render_type_declaration_with_keyword kw_type decl
      | _ ->
          render_signature_item item
    in
    let base_doc =
      match item with
      | Syn.Cst.SignatureItem.TypeDeclaration _ ->
          base_doc
      | _ -> (
          match render_signature_item_owned_trivia item with
          | Some owned ->
              let leading =
                doc_of_owned_trivia ~source (Syn.Cst.OwnedTrivia.leading owned)
              in
              let trailing =
                doc_of_owned_trivia ~source (Syn.Cst.OwnedTrivia.trailing owned)
              in
              base_doc
              |> doc_with_leading_trivia leading
              |> fun doc -> doc_with_trailing_trivia doc trailing
          | None ->
              base_doc)
    in
    match trailing_suffix with
    | None ->
        base_doc
    | Some suffix ->
        Doc.concat [ base_doc; suffix ]
  in
  let is_trivia =
    match item with
    | Syn.Cst.SignatureItem.Docstring docstring ->
        not (Syn.Cst.Docstring.is_section docstring)
    | Syn.Cst.SignatureItem.Comment _ ->
        false
    | _ ->
        false
  in
  let tight_after =
    match item with
    | Syn.Cst.SignatureItem.TypeDeclaration _ ->
        true
    | _ ->
        false
  in
  let compact_after =
    false
  in
  let is_docstring =
    match item with
    | Syn.Cst.SignatureItem.Docstring docstring ->
        not (Syn.Cst.Docstring.is_section docstring)
    | _ ->
        false
  in
  (doc, is_open_signature_item item, is_trivia, tight_after, false, compact_after, is_docstring)

and render_structure_item = function
  | Syn.Cst.StructureItem.LetBinding binding ->
      render_let_binding binding
  | Syn.Cst.StructureItem.TypeDeclaration decl ->
      render_type_declaration_with_keyword kw_type decl
  | Syn.Cst.StructureItem.TypeExtension decl ->
      render_type_extension ctx decl
  | Syn.Cst.StructureItem.ExternalDeclaration decl ->
      render_external_declaration decl
  | Syn.Cst.StructureItem.ModuleDeclaration decl ->
      render_module_declaration_with_keyword kw_module decl
  | Syn.Cst.StructureItem.RecursiveModuleDeclaration decl ->
      render_recursive_module_declaration decl
  | Syn.Cst.StructureItem.ModuleTypeDeclaration decl ->
      render_module_type_declaration decl
  | Syn.Cst.StructureItem.IncludeStatement stmt ->
      render_include_statement stmt
  | Syn.Cst.StructureItem.OpenStatement open_ ->
      Doc.concat
        [
          kw_open;
          (if open_.bang_token = None then Doc.empty else Doc.text "!");
          Doc.space;
          render_open_target open_.target;
        ]
  | Syn.Cst.StructureItem.Attribute attribute ->
      render_floating_attribute attribute
  | Syn.Cst.StructureItem.Docstring docstring ->
      doc_of_token (Syn.Cst.Docstring.token docstring)
  | Syn.Cst.StructureItem.Comment comment ->
      doc_of_token (Syn.Cst.Comment.token comment)
  | Syn.Cst.StructureItem.ExceptionDeclaration decl ->
      render_exception_declaration decl
  | Syn.Cst.StructureItem.Expression expression ->
      render_expression expression
  | Syn.Cst.StructureItem.Extension extension ->
      unsupported_syntax ~context:[ "structure_item" ] ~syntax_node:extension.syntax_node
        "floating extension items do not have a structural formatter yet"
  | Syn.Cst.StructureItem.ClassDeclaration decl ->
      unsupported_syntax ~context:[ "structure_item" ] ~syntax_node:decl.syntax_node
        "class declaration items do not have a structural formatter yet"
  | Syn.Cst.StructureItem.ClassTypeDeclaration decl ->
      unsupported_syntax ~context:[ "structure_item" ] ~syntax_node:decl.syntax_node
        "class type declaration items do not have a structural formatter yet"
  | Syn.Cst.StructureItem.ValueDeclaration decl ->
      unsupported_syntax ~context:[ "structure_item" ] ~syntax_node:decl.syntax_node
        "implementation val declaration items do not have a structural formatter yet"

and render_signature_item item =
  match item with
  | Syn.Cst.SignatureItem.TypeDeclaration decl ->
      render_type_declaration_with_keyword kw_type decl
  | Syn.Cst.SignatureItem.TypeExtension decl ->
      render_type_extension ctx decl
  | Syn.Cst.SignatureItem.ModuleDeclaration decl ->
      render_module_declaration_with_keyword kw_module decl
  | Syn.Cst.SignatureItem.RecursiveModuleDeclaration decl ->
      render_recursive_module_declaration decl
  | Syn.Cst.SignatureItem.ModuleTypeDeclaration decl ->
      render_module_type_declaration decl
  | Syn.Cst.SignatureItem.IncludeStatement stmt ->
      render_include_statement stmt
  | Syn.Cst.SignatureItem.OpenStatement open_ ->
      Doc.concat
        [
          kw_open;
          (if open_.bang_token = None then Doc.empty else Doc.text "!");
          Doc.space;
          render_open_target open_.target;
        ]
  | Syn.Cst.SignatureItem.Attribute attribute ->
      render_floating_attribute attribute
  | Syn.Cst.SignatureItem.Docstring docstring ->
      doc_of_token (Syn.Cst.Docstring.token docstring)
  | Syn.Cst.SignatureItem.Comment comment ->
      doc_of_token (Syn.Cst.Comment.token comment)
  | Syn.Cst.SignatureItem.ValueDeclaration decl ->
      Doc.concat
        [
          kw_val;
          Doc.space;
          render_value_declaration_name decl;
          colon;
          render_core_type decl.type_;
        ]
  | Syn.Cst.SignatureItem.ExternalDeclaration decl ->
      render_external_declaration decl
  | Syn.Cst.SignatureItem.ExceptionDeclaration decl ->
      render_exception_declaration decl
  | Syn.Cst.SignatureItem.Extension extension ->
      unsupported_syntax ~context:[ "signature_item" ] ~syntax_node:extension.syntax_node
        "floating extension items do not have a structural formatter yet"
  | Syn.Cst.SignatureItem.ClassDeclaration decl ->
      unsupported_syntax ~context:[ "signature_item" ] ~syntax_node:decl.syntax_node
        "class declaration items do not have a structural formatter yet"
  | Syn.Cst.SignatureItem.ClassTypeDeclaration decl ->
      unsupported_syntax ~context:[ "signature_item" ] ~syntax_node:decl.syntax_node
        "class type declaration items do not have a structural formatter yet"

and span_of_syntax_node_nontrivia_bounds ?(preserve_leading_trivia = false) syntax_node =
  let full_span = Syn.Ceibo.Red.SyntaxNode.span syntax_node in
  match Syn.Ceibo.Red.SyntaxNode.tokens syntax_node with
  | [] ->
      full_span
  | first :: rest ->
      let last = List.fold_left (fun _ token -> token) first rest in
      {
        Syn.Ceibo.Span.start =
          (if preserve_leading_trivia then
             full_span.start
           else
             (Syn.Ceibo.Red.SyntaxToken.span first).start);
        end_ = (Syn.Ceibo.Red.SyntaxToken.span last).end_;
      }

and span_of_syntax_node_nonwhitespace_bounds ?(preserve_leading_trivia = false) syntax_node =
  span_of_syntax_node_nontrivia_bounds ~preserve_leading_trivia syntax_node

and span_of_syntax_node_trim_leading_trivia_keep_trailing_comments syntax_node =
  let start_span = span_of_syntax_node_nontrivia_bounds syntax_node in
  let end_span =
    span_of_syntax_node_nonwhitespace_bounds ~preserve_leading_trivia:true syntax_node
  in
  { Syn.Ceibo.Span.start = start_span.start; end_ = end_span.end_ }

and trivia_span = fun trivia -> Syn.Cst.Token.span (Syn.Cst.Trivia.token trivia)

and owned_trivia_end = fun owned -> Syn.Cst.OwnedTrivia.leading owned
@ Syn.Cst.OwnedTrivia.inner owned
@ Syn.Cst.OwnedTrivia.trailing owned
|> List.fold_left
     (fun acc trivia ->
       Int.max acc (trivia_span trivia).end_)
     0

and record_field_owned_trivia_end = fun field ->
  owned_trivia_end (Syn.Cst.RecordField.owned_trivia field)

and variant_constructor_owned_trivia_end = fun constructor ->
  let arguments_end =
    match Syn.Cst.VariantConstructor.arguments constructor with
    | Some (Syn.Cst.ConstructorArguments.Record fields) ->
        fields
        |> List.fold_left
             (fun acc field -> Int.max acc (record_field_owned_trivia_end field))
             0
    | Some (Syn.Cst.ConstructorArguments.Tuple _)
    | None ->
        0
  in
  Int.max arguments_end
    (owned_trivia_end (Syn.Cst.VariantConstructor.owned_trivia constructor))

and type_definition_owned_trivia_end =
  function
  | Syn.Cst.TypeDefinition.Record { fields; _ } ->
      fields
      |> List.fold_left
           (fun acc field -> Int.max acc (record_field_owned_trivia_end field))
           0
  | Syn.Cst.TypeDefinition.Variant { constructors; _ } ->
      constructors
      |> List.fold_left
           (fun acc constructor ->
             Int.max acc (variant_constructor_owned_trivia_end constructor))
           0
  | Syn.Cst.TypeDefinition.Abstract
  | Syn.Cst.TypeDefinition.Alias _
  | Syn.Cst.TypeDefinition.Extensible _
  | Syn.Cst.TypeDefinition.FirstClassModule _
  | Syn.Cst.TypeDefinition.Object _
  | Syn.Cst.TypeDefinition.PolyVariant _ ->
      0

and type_declaration_owned_trivia_end = fun decl ->
  let current =
    Int.max
      (owned_trivia_end (Syn.Cst.TypeDeclaration.owned_trivia decl))
      (type_definition_owned_trivia_end (Syn.Cst.TypeDeclaration.type_definition decl))
  in
  Syn.Cst.TypeDeclaration.and_declarations decl
  |> List.fold_left
       (fun acc declaration ->
         Int.max acc (type_declaration_owned_trivia_end declaration))
       current

and render_structure_top_level_items ~source ~source_offset:_source_offset ~source_node ~items =
  let rec join_entries = function
    | [] ->
        Doc.empty
    | (doc, _, _, _, _, _) :: [] ->
        doc
    | (doc, is_open, is_trivia, tight_after, has_trailing_break, is_docstring)
      :: ((_, next_is_open, _, _, _, next_is_docstring) :: _ as rest) ->
        let separator =
          if has_trailing_break then
            Doc.empty
          else if is_docstring && next_is_docstring then
            blank_line
          else if tight_after || is_trivia then
            Doc.line
          else if is_open && next_is_open then
            Doc.line
          else
            blank_line
        in
        Doc.concat [ doc; separator; join_entries rest ]
  in
  let structure_item_span item =
    match item with
    | Syn.Cst.StructureItem.Comment comment ->
        Syn.Cst.Token.span (Syn.Cst.Comment.token comment)
    | Syn.Cst.StructureItem.Docstring docstring ->
        Syn.Cst.Token.span (Syn.Cst.Docstring.token docstring)
    | _ ->
        let syntax_node = Syn.Cst.StructureItem.syntax_node item in
        (match item with
        | Syn.Cst.StructureItem.TypeDeclaration decl ->
            let span =
              span_of_syntax_node_trim_leading_trivia_keep_trailing_comments syntax_node
            in
            { span with end_ = Int.max span.end_ (type_declaration_owned_trivia_end decl) }
        | _ ->
            span_of_syntax_node_nontrivia_bounds syntax_node)
  in
  let compare_structure_items_by_span left right =
    let left_span = structure_item_span left in
    let right_span = structure_item_span right in
    if not (Int.equal left_span.start right_span.start) then
      Int.compare left_span.start right_span.start
    else
      Int.compare left_span.end_ right_span.end_
  in
  let source_length = String.length source in
  let source_end = (Syn.Ceibo.Red.SyntaxNode.span source_node).start + source_length in
  let top_level_tokens = direct_tokens_in_source_order source_node in
  let items =
    items
    |> List.map (fun item -> (item, structure_item_span item))
    |> List.sort (fun (left, left_span) (right, right_span) ->
           let left_span : Syn.Ceibo.Span.t = left_span in
           let right_span : Syn.Ceibo.Span.t = right_span in
           let order =
             if not (Int.equal left_span.start right_span.start) then
               Int.compare left_span.start right_span.start
             else
               Int.compare left_span.end_ right_span.end_
           in
           if not (Int.equal order 0) then
             order
           else
             compare_structure_items_by_span left right)
  in
  let rec loop acc items =
    yield ();
    match items with
    | [] ->
        join_entries (List.rev acc)
    | (item, (base_span : Syn.Ceibo.Span.t)) :: rest ->
        let next_boundary =
          match rest with
          | (_, (next_span : Syn.Ceibo.Span.t)) :: _ ->
              next_span.start
          | [] ->
              source_end
        in
        let trailing_suffix =
          phrase_separator_doc_between_tokens top_level_tokens ~start:base_span.end_
            ~end_:next_boundary
        in
        let entry = render_structure_entry ~source ~trailing_suffix item in
        loop (entry :: acc) rest
  in
  loop [] items

and render_structure_items ?source ~source_node items =
  let source_opt = source in
  let source =
    match source_opt with
    | Some source ->
        source
    | None ->
        (match ctx.source with
        | Some full_source ->
            Source.source_of_span full_source (Syn.Ceibo.Red.SyntaxNode.span source_node)
        | None ->
            Source.source_of_syntax_node source_node)
  in
  let source_offset =
    match source_opt with
    | Some _ ->
        0
    | None ->
        (Syn.Ceibo.Red.SyntaxNode.span source_node).start
  in
  render_structure_top_level_items ~source ~source_offset ~source_node ~items

and render_signature_top_level_items
    ~source ~source_offset ~source_node ~items =
  let rec join_entries = function
    | [] ->
        Doc.empty
    | (doc, _, _, _, _, _, _) :: [] ->
        doc
    | (doc, is_open, is_trivia, tight_after, has_trailing_break, compact_after, is_docstring)
      :: ((_, next_is_open, _, _, _, _, next_is_docstring) :: _ as rest) ->
        let separator =
          if has_trailing_break then
            Doc.empty
          else if compact_after then
            Doc.empty
          else if is_docstring && next_is_docstring then
            blank_line
          else if tight_after || is_trivia then
            Doc.line
          else if is_open && next_is_open then
            Doc.line
          else
            blank_line
        in
        Doc.concat [ doc; separator; join_entries rest ]
  in
  let signature_item_span item =
    match item with
    | Syn.Cst.SignatureItem.Comment comment ->
        Syn.Cst.Token.span (Syn.Cst.Comment.token comment)
    | Syn.Cst.SignatureItem.Docstring docstring ->
        Syn.Cst.Token.span (Syn.Cst.Docstring.token docstring)
    | Syn.Cst.SignatureItem.TypeDeclaration decl ->
        let syntax_node = Syn.Cst.SignatureItem.syntax_node item in
        let span =
          let span =
            span_of_syntax_node_trim_leading_trivia_keep_trailing_comments syntax_node
          in
          { span with end_ = Int.max span.end_ (type_declaration_owned_trivia_end decl) }
        in
        span
    | _ ->
        Syn.Cst.SignatureItem.syntax_node item
        |> span_of_syntax_node_nontrivia_bounds
  in
  let compare_signature_items_by_span left right =
    let left_span = signature_item_span left in
    let right_span = signature_item_span right in
    if not (Int.equal left_span.start right_span.start) then
      Int.compare left_span.start right_span.start
    else
      Int.compare left_span.end_ right_span.end_
  in
  let items =
    items
    |> List.map (fun item -> (item, signature_item_span item))
    |> List.sort (fun (left, left_span) (right, right_span) ->
           let left_span : Syn.Ceibo.Span.t = left_span in
           let right_span : Syn.Ceibo.Span.t = right_span in
           let order =
             if not (Int.equal left_span.start right_span.start) then
               Int.compare left_span.start right_span.start
             else
               Int.compare left_span.end_ right_span.end_
           in
           if not (Int.equal order 0) then
             order
           else
             compare_signature_items_by_span left right)
  in
  let rec loop acc items =
    yield ();
    match items with
    | [] ->
        join_entries (List.rev acc)
    | (item, (base_span : Syn.Ceibo.Span.t)) :: rest ->
        let entry =
          render_signature_entry ~source ~source_offset ~span:base_span
            ~trailing_suffix:None item
        in
        loop (entry :: acc) rest
  in
  loop [] items

and render_signature_items ?source ~source_node items =
  let source_opt = source in
  let source =
    match source_opt with
    | Some source ->
        source
    | None ->
        (match ctx.source with
        | Some full_source ->
            Source.source_of_span full_source (Syn.Ceibo.Red.SyntaxNode.span source_node)
        | None ->
            Source.source_of_syntax_node source_node)
  in
  let source_offset =
    match source_opt with
    | Some _ ->
        0
    | None ->
        (Syn.Ceibo.Red.SyntaxNode.span source_node).start
  in
  render_signature_top_level_items ~source ~source_offset ~source_node ~items
  in
  { render_structure_items; render_signature_items }

let source_file = fun ~source source_file ->
  try
    let lowerer = make_lowerer {source = Some source} in
    Ok
      (match source_file with
      | Syn.Cst.Implementation implementation ->
          lowerer.render_structure_items ~source ~source_node:(Syn.Cst.SourceFile.syntax_node
          (Syn.Cst.Implementation implementation)) implementation.items
      | Syn.Cst.Interface interface ->
          lowerer.render_signature_items ~source ~source_node:(Syn.Cst.SourceFile.syntax_node
          (Syn.Cst.Interface interface)) interface.items)
  with
  | Unsupported err ->
      Error err
