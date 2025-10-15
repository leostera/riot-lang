open Std

(** Compile Ceibo green tree to HTML *)
let rec compile_element source elem =
  match elem with
  | Ceibo.Green.Token tok ->
      (* For escaped characters, strip the leading backslash *)
      (match tok.kind with
      | Syntax_kind.ESCAPED_CHAR ->
          let text = tok.text in
          if String.length text > 1 && text.[0] = '\\' then
            Html.text (String.sub text 1 (String.length text - 1))
          else
            Html.text text
      | _ ->
          Html.text tok.text)
  | Ceibo.Green.Node node ->
      compile_node source node

and compile_node source node =
  let kind = node.kind in
  let children = node.children in
  let children_html = Array.to_list children |> List.map (compile_element source) in
  
  match kind with
  | Syntax_kind.DOCUMENT ->
      Html.fragment children_html
  | Syntax_kind.PARAGRAPH ->
      Html.element "p" children_html
  | Syntax_kind.HEADING1 -> Html.element "h1" children_html
  | Syntax_kind.HEADING2 -> Html.element "h2" children_html
  | Syntax_kind.HEADING3 -> Html.element "h3" children_html
  | Syntax_kind.HEADING4 -> Html.element "h4" children_html
  | Syntax_kind.HEADING5 -> Html.element "h5" children_html
  | Syntax_kind.HEADING6 -> Html.element "h6" children_html
  | Syntax_kind.TEXT ->
      Html.fragment children_html
  | Syntax_kind.CODE_BLOCK ->
      (* Add trailing newline after code content *)
      let code = Html.element "code" (children_html @ [Html.text "\n"]) in
      Html.element "pre" [ code ]
  | Syntax_kind.BLOCKQUOTE ->
      Html.element "blockquote" children_html
  | Syntax_kind.LIST ->
      (* TODO: Determine ordered vs unordered *)
      Html.element "ul" children_html
  | Syntax_kind.LIST_ITEM ->
      Html.element "li" children_html
  | Syntax_kind.EMPHASIS ->
      Html.element "em" children_html
  | Syntax_kind.STRONG ->
      Html.element "strong" children_html
  | Syntax_kind.INLINE_CODE ->
      (* Convert line endings to spaces *)
      let text = Html.to_string (Html.fragment children_html) in
      let with_spaces = String.map (fun c -> if c = '\n' || c = '\r' then ' ' else c) text in
      (* Strip single leading/trailing space if both present AND there's content between *)
      let len = String.length with_spaces in
      let stripped = 
        if len > 2 && with_spaces.[0] = ' ' && with_spaces.[len - 1] = ' ' then
          String.sub with_spaces 1 (len - 2)
        else
          with_spaces
      in
      Html.element "code" [Html.text stripped]
  | Syntax_kind.LINK ->
      (* For autolinks, the href is the text content *)
      let href = Html.to_string (Html.fragment children_html) in
      Html.element "a" ~attrs:[("href", href)] children_html
  | Syntax_kind.IMAGE ->
      (* TODO: Extract src/alt from somewhere *)
      Html.element "img" ~attrs:[("src", ""); ("alt", "")] []
  | Syntax_kind.HARD_BREAK ->
      Html.raw "<br />\n"
  | Syntax_kind.SOFT_BREAK ->
      Html.text "\n"
  | Syntax_kind.THEMATIC_BREAK ->
      Html.element "hr" []
  | _ ->
      (* For tokens/unknown, just render children *)
      Html.fragment children_html

let compile source tree =
  compile_node source tree
