open Std
open Std.Collections
  open Std.IO

(** Compile Ceibo green tree to HTML *)
let rec compile_element source elem =
  match elem with
  | Ceibo.Green.Token tok -> (
      (* For escaped characters, strip the leading backslash *)
      match tok.kind with
      | Syntax_kind.ESCAPED_CHAR ->
          let text = tok.text in
          if String.length text > 1 && text.[0] = '\\' then
            Html.text (String.sub text 1 (String.length text - 1))
          else Html.text text
      | _ -> Html.text tok.text)
  | Ceibo.Green.Node node -> compile_node source node

(** Compile element but keep backslashes (for code spans) *)
and compile_element_literal source elem =
  match elem with
  | Ceibo.Green.Token tok -> Html.text tok.text
  | Ceibo.Green.Node node -> compile_node source node

and compile_node source node =
  let kind = node.kind in
  let children = node.children in
  let children_html =
    Array.to_list children |> List.map (compile_element source)
  in

  (* Helper to strip up to 3 leading spaces from children *)
  let strip_leading_spaces children =
    (* Strip spaces from the raw token stream *)
    let rec strip_spaces_from_children remaining_strips = function
      | [] -> []
      | Ceibo.Green.Token tok :: rest
        when tok.kind = Syntax_kind.SPACE && remaining_strips > 0 ->
          strip_spaces_from_children (remaining_strips - 1) rest
      | children -> children
    in
    let stripped_children =
      strip_spaces_from_children 3 (Array.to_list children)
    in
    List.map (compile_element source) stripped_children
  in

  (* Helper to process paragraph content: strip leading spaces, handle line breaks, strip trailing *)
  let process_paragraph_content children =
    (* First strip up to 3 leading spaces *)
    let rec strip_leading remaining = function
      | [] -> []
      | Ceibo.Green.Token tok :: rest
        when tok.kind = Syntax_kind.SPACE && remaining > 0 ->
          strip_leading (remaining - 1) rest
      | lst -> lst
    in
    let after_leading = strip_leading 3 (Array.to_list children) in

    (* Process spaces before newlines and trailing spaces *)
    let rec process_line_breaks acc = function
      | [] -> List.rev acc
      | Ceibo.Green.Token backslash :: Ceibo.Green.Token newline :: rest
        when backslash.kind = Syntax_kind.BACKSLASH
             && newline.kind = Syntax_kind.NEWLINE ->
          (* Backslash before newline - hard line break *)
          process_line_breaks
            (Ceibo.Green.Node
               (Ceibo.Green.make_node Syntax_kind.HARD_BREAK [||])
            :: acc)
            rest
      | Ceibo.Green.Token space :: Ceibo.Green.Token newline :: rest
        when space.kind = Syntax_kind.SPACE
             && newline.kind = Syntax_kind.NEWLINE ->
          (* Single space before newline - strip it (soft line break) *)
          process_line_breaks (Ceibo.Green.Token newline :: acc) rest
      | Ceibo.Green.Token space1 :: Ceibo.Green.Token space2 :: rest
        when space1.kind = Syntax_kind.SPACE && space2.kind = Syntax_kind.SPACE
        ->
          (* Two spaces - consume any additional spaces and check for newline *)
          let rec consume_spaces_before_newline collected remaining =
            match remaining with
            | Ceibo.Green.Token tok :: rest when tok.kind = Syntax_kind.SPACE ->
                consume_spaces_before_newline (collected + 1) rest
            | Ceibo.Green.Token tok :: rest when tok.kind = Syntax_kind.NEWLINE
              ->
                (* Found newline after spaces - this is a hard break *)
                (true, collected, rest)
            | _ ->
                (* No newline found, not a hard break *)
                (false, collected, remaining)
          in
          let is_hard_break, total_spaces, after =
            consume_spaces_before_newline 2 rest
          in
          if is_hard_break && total_spaces >= 2 then
            (* Hard line break - don't include any of the spaces *)
            process_line_breaks
              (Ceibo.Green.Node
                 (Ceibo.Green.make_node Syntax_kind.HARD_BREAK [||])
              :: acc)
              after
          else
            (* Not a hard break, keep the first space and continue *)
            process_line_breaks
              (Ceibo.Green.Token space1 :: acc)
              (Ceibo.Green.Token space2 :: rest)
      | item :: rest -> process_line_breaks (item :: acc) rest
    in
    let after_breaks = process_line_breaks [] after_leading in

    (* Strip trailing spaces, tabs, and newlines *)
    let rec strip_trailing = function
      | [] -> []
      | lst -> (
          match List.rev lst with
          | Ceibo.Green.Token tok :: rest
            when tok.kind = Syntax_kind.SPACE
                 || tok.kind = Syntax_kind.TAB
                 || tok.kind = Syntax_kind.NEWLINE ->
              strip_trailing (List.rev rest)
          | _ -> lst)
    in
    let final_children = strip_trailing after_breaks in
    List.map (compile_element source) final_children
  in

  (* Helper to strip trailing spaces from children *)
  let strip_trailing_spaces children =
    let rec strip_from_end = function
      | [] -> []
      | lst -> (
          match List.rev lst with
          | Ceibo.Green.Token tok :: rest
            when tok.kind = Syntax_kind.SPACE || tok.kind = Syntax_kind.TAB ->
              strip_from_end (List.rev rest)
          | _ -> lst)
    in
    let stripped_children = strip_from_end (Array.to_list children) in
    List.map (compile_element source) stripped_children
  in

  (* Helper to strip both leading and trailing spaces *)
  let strip_heading_spaces children =
    let lst = Array.to_list children in
    (* Strip leading *)
    let rec strip_leading remaining = function
      | [] -> []
      | Ceibo.Green.Token tok :: rest
        when tok.kind = Syntax_kind.SPACE && remaining > 0 ->
          strip_leading (remaining - 1) rest
      | lst -> lst
    in
    (* Strip trailing *)
    let rec strip_trailing = function
      | [] -> []
      | lst -> (
          match List.rev lst with
          | Ceibo.Green.Token tok :: rest
            when tok.kind = Syntax_kind.SPACE || tok.kind = Syntax_kind.TAB ->
              strip_trailing (List.rev rest)
          | _ -> lst)
    in
    let result = lst |> strip_leading 3 |> strip_trailing in
    List.map (compile_element source) result
  in

  match kind with
  | Syntax_kind.DOCUMENT -> Html.fragment children_html
  | Syntax_kind.PARAGRAPH ->
      Html.element "p" (process_paragraph_content children)
  | Syntax_kind.HEADING1 -> Html.element "h1" (strip_heading_spaces children)
  | Syntax_kind.HEADING2 -> Html.element "h2" (strip_heading_spaces children)
  | Syntax_kind.HEADING3 -> Html.element "h3" (strip_heading_spaces children)
  | Syntax_kind.HEADING4 -> Html.element "h4" (strip_heading_spaces children)
  | Syntax_kind.HEADING5 -> Html.element "h5" (strip_heading_spaces children)
  | Syntax_kind.HEADING6 -> Html.element "h6" (strip_heading_spaces children)
  | Syntax_kind.TEXT -> Html.fragment children_html
  | Syntax_kind.CODE_BLOCK ->
      (* Indented code blocks - compile literally (keep backslashes) *)
      let literal_html =
        Array.to_list children |> List.map (compile_element_literal source)
      in
      let code = Html.element "code" (literal_html @ [ Html.text "\n" ]) in
      Html.element "pre" [ code ]
  | Syntax_kind.FENCED_CODE_BLOCK ->
      (* Extract info string if present (first child is INFO_STRING node) *)
      let info_string, content_children =
        match Array.to_list children with
        | Ceibo.Green.Node info_node :: rest
          when info_node.kind = Syntax_kind.INFO_STRING ->
            (* Get raw text from info string *)
            let rec get_text_tokens acc = function
              | [] -> String.concat "" (List.rev acc)
              | Ceibo.Green.Token tok :: rest ->
                  get_text_tokens (tok.text :: acc) rest
              | Ceibo.Green.Node n :: rest ->
                  let inner_text =
                    get_text_tokens [] (Array.to_list n.children)
                  in
                  get_text_tokens (inner_text :: acc) rest
            in
            let info_text =
              get_text_tokens [] (Array.to_list info_node.children)
            in
            (* Extract first word as language *)
            let lang =
              match String.split_on_char ' ' info_text with
              | first :: _ when String.length first > 0 -> Some first
              | _ -> None
            in
            (lang, rest)
        | lst -> (None, lst)
      in

      (* Compile content children literally (keep backslashes) *)
      let content_html =
        List.map (compile_element_literal source) content_children
      in

      (* Check if we need to add trailing newline *)
      let needs_newline =
        match List.rev content_children with
        | Ceibo.Green.Token tok :: _ -> tok.kind != Syntax_kind.NEWLINE
        | _ :: _ -> true
        | [] -> false
      in

      (* Build code element with optional class attribute *)
      let code_attrs =
        match info_string with
        | Some lang -> [ ("class", "language-" ^ lang) ]
        | None -> []
      in
      let code =
        if List.length content_html = 0 then
          Html.element "code" ~attrs:code_attrs []
        else if needs_newline then
          Html.element "code" ~attrs:code_attrs
            (content_html @ [ Html.text "\n" ])
        else Html.element "code" ~attrs:code_attrs content_html
      in
      Html.element "pre" [ code ]
  | Syntax_kind.BLOCKQUOTE -> Html.element "blockquote" children_html
  | Syntax_kind.LIST ->
      (* Generic LIST - shouldn't be used, but fallback to ul *)
      Html.element "ul" children_html
  | Syntax_kind.UNORDERED_LIST -> Html.element "ul" children_html
  | Syntax_kind.ORDERED_LIST -> Html.element "ol" children_html
  | Syntax_kind.LIST_ITEM -> Html.element "li" children_html
  | Syntax_kind.EMPHASIS -> Html.element "em" children_html
  | Syntax_kind.STRONG -> Html.element "strong" children_html
  | Syntax_kind.INLINE_CODE ->
      (* Compile children literally (keep backslashes) *)
      let literal_html =
        Array.to_list children |> List.map (compile_element_literal source)
      in
      (* Convert line endings to spaces *)
      let text = Html.to_string (Html.fragment literal_html) in
      let with_spaces =
        String.map (fun c -> if c = '\n' || c = '\r' then ' ' else c) text
      in
      (* Strip single leading/trailing space if both present AND there's content between *)
      let len = String.length with_spaces in
      let stripped =
        if len > 2 && with_spaces.[0] = ' ' && with_spaces.[len - 1] = ' ' then
          String.sub with_spaces 1 (len - 2)
        else with_spaces
      in
      Html.element "code" [ Html.text stripped ]
  | Syntax_kind.LINK ->
      (* For autolinks, get raw token text for href and display *)
      let rec get_raw_text elems =
        List.fold_left
          (fun acc elem ->
            match elem with
            | Ceibo.Green.Token tok -> acc ^ tok.text
            | Ceibo.Green.Node n ->
                acc ^ get_raw_text (Array.to_list n.children))
          "" elems
      in
      let href = get_raw_text (Array.to_list node.children) in

      (* Percent-encode special characters in href *)
      let percent_encode str =
        let buf = Buffer.create (String.length str * 2) in
        String.iter
          (fun c ->
            match c with
            | '\\' -> Buffer.add_string buf "%5C"
            | '[' -> Buffer.add_string buf "%5B"
            | ']' -> Buffer.add_string buf "%5D"
            | '<' -> Buffer.add_string buf "%3C"
            | '>' -> Buffer.add_string buf "%3E"
            | ' ' -> Buffer.add_string buf "%20"
            | c -> Buffer.add_char buf c)
          str;
        Buffer.contents buf
      in

      let encoded_href = percent_encode href in

      (* Check if this is an email autolink - if so, add mailto: prefix *)
      let is_email =
        String.contains href "@" && not (String.contains href ":")
      in
      let full_href =
        if is_email then "mailto:" ^ encoded_href else encoded_href
      in
      (* Compile children literally to preserve backslashes in display text *)
      let display_html =
        Array.to_list children |> List.map (compile_element_literal source)
      in
      Html.element "a" ~attrs:[ ("href", full_href) ] display_html
  | Syntax_kind.IMAGE ->
      (* TODO: Extract src/alt from somewhere *)
      Html.element "img" ~attrs:[ ("src", ""); ("alt", "") ] []
  | Syntax_kind.HARD_BREAK -> Html.raw "<br />\n"
  | Syntax_kind.SOFT_BREAK -> Html.text "\n"
  | Syntax_kind.THEMATIC_BREAK -> Html.element "hr" []
  | Syntax_kind.INFO_STRING ->
      (* Info strings are handled by their parent FENCED_CODE_BLOCK *)
      Html.fragment []
  | _ ->
      (* For tokens/unknown, just render children *)
      Html.fragment children_html

let compile source tree = compile_node source tree
