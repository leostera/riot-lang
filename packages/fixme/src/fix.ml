open Std

type target =
  | Node of Syn.Cst.syntax_node
  | Token of Syn.Cst.syntax_token

type replacement =
  | SourceOfNode of Syn.Cst.syntax_node
  | SourceOfToken of Syn.Cst.syntax_token
  | Text of string

type operation =
  | Delete of { target: target }
  | Replace of { target: target; replacement: replacement }
  | InsertBefore of { anchor: target; content: replacement }
  | InsertAfter of { anchor: target; content: replacement }
  | Swap of { left: target; right: target }

type fix = {
  title: string;
  operations: operation list;
}

type text_edit = {
  span: Syn.Ceibo.Span.t;
  new_text: string;
}

let source_of_node = fun node -> SourceOfNode node

let source_of_token = fun token -> SourceOfToken token

let text = fun value -> Text value

let delete = fun ~target -> Delete { target }

let delete_node = fun target -> delete ~target:(Node target)

let replace = fun ~target ~replacement -> Replace { target; replacement }

let replace_node = fun ~target ~replacement ->
  replace ~target:(Node target) ~replacement:(source_of_node replacement)

let replace_node_with_text = fun ~target ~text:value ->
  replace ~target:(Node target) ~replacement:(text value)

let replace_token_with_text = fun ~target ~text:value ->
  replace ~target:(Token target) ~replacement:(text value)

let insert_before = fun ~anchor ~content -> InsertBefore { anchor; content }

let insert_after = fun ~anchor ~content -> InsertAfter { anchor; content }

let swap = fun ~left ~right -> Swap { left; right }

let make = fun ~title ~operations -> { title; operations }

let title = fun fix -> fix.title

let operations = fun fix -> fix.operations

let target_span = fun target ->
  match target with
  | Node node -> Syn.Ceibo.Red.SyntaxNode.span node
  | Token token -> Syn.Ceibo.Red.SyntaxToken.span token

let source_slice = fun ~source span ->
  let len = Syn.Ceibo.Span.(span.end_ - span.start) in
  String.sub source ~offset:span.start ~len

let replacement_text = fun ~source ->
  function
  | SourceOfNode node -> source_slice ~source (Syn.Ceibo.Red.SyntaxNode.span node)
  | SourceOfToken token -> source_slice ~source (Syn.Ceibo.Red.SyntaxToken.span token)
  | Text text -> text

let make_insert_at = fun pos ~new_text ->
  let span = Syn.Ceibo.Span.make ~start:pos ~end_:pos in
  { span; new_text }

let lower_operation = fun ~source ->
  function
  | Delete { target } ->
      Ok [ { span = target_span target; new_text = "" } ]
  | Replace { target; replacement } ->
      Ok [ { span = target_span target; new_text = replacement_text ~source replacement } ]
  | InsertBefore { anchor; content } ->
      let anchor_span = target_span anchor in
      Ok [ make_insert_at anchor_span.start ~new_text:(replacement_text ~source content); ]
  | InsertAfter { anchor; content } ->
      let anchor_span = target_span anchor in
      Ok [ make_insert_at anchor_span.end_ ~new_text:(replacement_text ~source content); ]
  | Swap { left; right } ->
      let left_span = target_span left in
      let right_span = target_span right in
      if Syn.Ceibo.Span.overlaps left_span right_span then
        Error "Swap operations require non-overlapping syntax objects"
      else
        Ok [
          { span = left_span; new_text = source_slice ~source right_span };
          { span = right_span; new_text = source_slice ~source left_span };
        ]

let compare_text_edit = fun a b ->
  match Int.compare a.span.start b.span.start with
  | Order.EQ -> Int.compare a.span.end_ b.span.end_
  | order -> order

let same_text_edit = fun a b ->
  a.span.start = b.span.start && a.span.end_ = b.span.end_ && String.equal a.new_text b.new_text

let dedupe_text_edits = fun edits ->
  let sorted = List.sort edits ~compare:compare_text_edit in
  let rec loop = fun acc ->
    function
    | [] -> List.reverse acc
    | [ edit ] -> List.reverse (edit :: acc)
    | edit :: (next :: rest) ->
        if same_text_edit edit next then
          loop acc (next :: rest)
        else
          loop (edit :: acc) (next :: rest)
  in
  loop [] sorted

let validate_text_edit = fun ~source edit ->
  let source_len = String.length source in
  if edit.span.start < 0 || edit.span.end_ < 0 then
    Error "Fix operation span cannot be negative"
  else if edit.span.start > edit.span.end_ then
    Error "Fix operation span start cannot be greater than end"
  else if edit.span.end_ > source_len then
    Error "Fix operation span is out of bounds for the source"
  else
    Ok ()

let validate_edits = fun ~source edits ->
  let edits = dedupe_text_edits edits in
  let rec loop = fun previous ->
    function
    | [] -> Ok edits
    | edit :: rest -> (
        match validate_text_edit ~source edit with
        | Error _ as err -> err
        | Ok () -> (
            match previous with
            | Some prev when Syn.Ceibo.Span.overlaps prev.span edit.span -> Error "Fix operations overlap and cannot be applied safely"
            | _ -> loop (Some edit) rest
          )
      )
  in
  loop None edits

let apply_text_edit_unchecked = fun ~source edit ->
  let prefix = String.sub source ~offset:0 ~len:edit.span.start in
  let suffix_start = edit.span.end_ in
  let suffix_len = String.length source - suffix_start in
  let suffix = String.sub source ~offset:suffix_start ~len:suffix_len in
  prefix ^ edit.new_text ^ suffix

let apply_operation = fun ~source operation ->
  match lower_operation ~source operation with
  | Error _ as err -> err
  | Ok edits -> (
      match validate_edits ~source edits with
      | Error _ as err -> err
      | Ok edits ->
          let edits_desc =
            List.sort edits
              ~compare:(fun a b ->
                Int.compare b.span.start a.span.start)
          in
          Ok (List.fold_left
            edits_desc
            ~init:source
            ~fn:(fun acc edit -> apply_text_edit_unchecked ~source:acc edit))
    )

let lower_fix = fun ~source fix ->
  match List.map fix.operations ~fn:(lower_operation ~source) |> Result.all with
  | Error _ as err -> err
  | Ok edits -> validate_edits ~source (List.concat edits)

let lower_fixes = fun ~source fixes ->
  let lowered = fixes
  |> List.map ~fn:(fun fix -> List.map fix.operations ~fn:(lower_operation ~source))
  |> List.concat in
  match Result.all lowered with
  | Error _ as err -> err
  | Ok edits -> validate_edits ~source (List.concat edits)

let validate_fix = fun ~source fix -> lower_fix ~source fix |> Result.map ~fn:(fun _ -> ())

let apply_fix = fun ~source fix ->
  match lower_fix ~source fix with
  | Error _ as err -> err
  | Ok edits ->
      let edits_desc =
        List.sort edits
          ~compare:(fun a b ->
            Int.compare b.span.start a.span.start)
      in
      Ok (List.fold_left
        edits_desc
        ~init:source
        ~fn:(fun acc edit -> apply_text_edit_unchecked ~source:acc edit))

let apply_fixes = fun ~source fixes ->
  match lower_fixes ~source fixes with
  | Error _ as err -> err
  | Ok edits ->
      let edits_desc =
        List.sort edits
          ~compare:(fun a b ->
            Int.compare b.span.start a.span.start)
      in
      Ok (List.fold_left
        edits_desc
        ~init:source
        ~fn:(fun acc edit -> apply_text_edit_unchecked ~source:acc edit))
