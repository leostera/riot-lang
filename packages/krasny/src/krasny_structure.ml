open Std
open Std.Collections

module Doc = Krasny_doc
module Source = Krasny_source
module Expression = Krasny_expression

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
            let rendered_text = Doc.to_string rendered in
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
