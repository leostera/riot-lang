open Std

type text_edit = { span : Syn.Ceibo.Span.t; new_text : string }
type fix = { title : string; edits : text_edit list }

let make_text_edit ~span ~new_text = { span; new_text }
let make ~title ~edits = { title; edits }
let title fix = fix.title
let edits fix = fix.edits

let compare_text_edit a b =
  match Int.compare a.span.start b.span.start with
  | 0 -> Int.compare a.span.end_ b.span.end_
  | n -> n

let same_text_edit a b =
  a.span.start = b.span.start
  && a.span.end_ = b.span.end_
  && String.equal a.new_text b.new_text

let dedupe_text_edits edits =
  let sorted = List.sort compare_text_edit edits in
  let rec loop acc = function
    | [] -> List.rev acc
    | [ edit ] -> List.rev (edit :: acc)
    | edit :: (next :: rest) ->
        if same_text_edit edit next then
          loop acc (next :: rest)
        else loop (edit :: acc) (next :: rest)
  in
  loop [] sorted

let validate_text_edit ~source edit =
  let source_len = String.length source in
  if edit.span.start < 0 || edit.span.end_ < 0 then
    Error "Fix edit span cannot be negative"
  else if edit.span.start > edit.span.end_ then
    Error "Fix edit span start cannot be greater than end"
  else if edit.span.end_ > source_len then
    Error "Fix edit span is out of bounds for the source"
  else Ok ()

let validate_edits ~source edits =
  let edits = dedupe_text_edits edits in
  let rec loop previous = function
    | [] -> Ok edits
    | edit :: rest -> (
        match validate_text_edit ~source edit with
        | Error _ as err -> err
        | Ok () -> (
            match previous with
            | Some prev when Syn.Ceibo.Span.overlaps prev.span edit.span ->
                Error "Fix edits overlap and cannot be applied safely"
            | _ -> loop (Some edit) rest))
  in
  loop None edits

let apply_text_edit_unchecked ~source edit =
  let prefix = String.sub source 0 edit.span.start in
  let suffix_start = edit.span.end_ in
  let suffix_len = String.length source - suffix_start in
  let suffix = String.sub source suffix_start suffix_len in
  prefix ^ edit.new_text ^ suffix

let apply_edit ~source edit =
  match validate_text_edit ~source edit with
  | Error _ as err -> err
  | Ok () -> Ok (apply_text_edit_unchecked ~source edit)

let validate_fix ~source fix =
  validate_edits ~source fix.edits |> Result.map (fun _ -> ())

let apply_fix ~source fix =
  match validate_edits ~source fix.edits with
  | Error _ as err -> err
  | Ok edits ->
      let edits_desc =
        List.sort (fun a b -> Int.compare b.span.start a.span.start) edits
      in
      Ok
        (List.fold_left
           (fun acc edit -> apply_text_edit_unchecked ~source:acc edit)
           source edits_desc)

let apply_fixes ~source fixes =
  let edits = List.concat_map (fun fix -> fix.edits) fixes in
  match validate_edits ~source edits with
  | Error _ as err -> err
  | Ok edits ->
      let edits_desc =
        List.sort (fun a b -> Int.compare b.span.start a.span.start) edits
      in
      Ok
        (List.fold_left
           (fun acc edit -> apply_text_edit_unchecked ~source:acc edit)
           source edits_desc)
