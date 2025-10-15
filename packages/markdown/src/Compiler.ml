open Std

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
  | Syntax_kind.PARAGRAPH -> Html.element "p" (strip_leading_spaces children)
  | Syntax_kind.HEADING1 -> Html.element "h1" (strip_heading_spaces children)
  | Syntax_kind.HEADING2 -> Html.element "h2" (strip_heading_spaces children)
  | Syntax_kind.HEADING3 -> Html.element "h3" (strip_heading_spaces children)
  | Syntax_kind.HEADING4 -> Html.element "h4" (strip_heading_spaces children)
  | Syntax_kind.HEADING5 -> Html.element "h5" (strip_heading_spaces children)
  | Syntax_kind.HEADING6 -> Html.element "h6" (strip_heading_spaces children)
  | Syntax_kind.TEXT -> Html.fragment children_html
  | Syntax_kind.CODE_BLOCK ->
      (* Indented code blocks need trailing newline *)
      let code = Html.element "code" (children_html @ [ Html.text "\n" ]) in
      Html.element "pre" [ code ]
  | Syntax_kind.FENCED_CODE_BLOCK ->
      (* Fenced code blocks need trailing newline if non-empty *)
      let code =
        if List.length children_html = 0 then Html.element "code" []
        else Html.element "code" (children_html @ [ Html.text "\n" ])
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
      (* Convert line endings to spaces *)
      let text = Html.to_string (Html.fragment children_html) in
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
      (* For autolinks, get raw token text for href (unescape) *)
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
      (* Check if this is an email autolink - if so, add mailto: prefix *)
      let is_email =
        String.contains href '@' && not (String.contains href ':')
      in
      let full_href = if is_email then "mailto:" ^ href else href in
      Html.element "a" ~attrs:[ ("href", full_href) ] children_html
  | Syntax_kind.IMAGE ->
      (* TODO: Extract src/alt from somewhere *)
      Html.element "img" ~attrs:[ ("src", ""); ("alt", "") ] []
  | Syntax_kind.HARD_BREAK -> Html.raw "<br />\n"
  | Syntax_kind.SOFT_BREAK -> Html.text "\n"
  | Syntax_kind.THEMATIC_BREAK -> Html.element "hr" []
  | _ ->
      (* For tokens/unknown, just render children *)
      Html.fragment children_html

let compile source tree = compile_node source tree
