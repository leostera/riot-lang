open Std

type constraints = {
  available_width: float option;
  available_height: float option;
}

type text_measurement = {
  size: Viewport.t;
  lines: string list;
}

type text_measurer = constraints:constraints -> string -> Style.t -> text_measurement

type t = {
  viewport: Viewport.t;
  text_measurer: text_measurer;
}

let constraints = fun ?available_width ?available_height () -> { available_width; available_height }

let clamp_non_negative = fun value ->
  if (
    match Float.compare value 0.0 with
    | Order.LT -> true
    | Order.EQ
    | Order.GT -> false
  ) then
    0.0
  else
    value

let wrap_characters = fun ~width text ->
  if width <= 0 then
    [ "" ]
  else
    let graphemes =
      String.into_grapheme_iter text
      |> Std.Iter.Iterator.to_list
    in
    let rec flush current acc =
      match current with
      | [] -> acc
      | _ ->
          let line =
            current
            |> List.rev
            |> List.map ~fn:Std.Unicode.Grapheme.to_string
            |> String.concat ""
          in
          line :: acc
    in
    let rec loop current current_width acc = fun __tmp1 ->
      match __tmp1 with
      | [] ->
          flush current acc
          |> List.rev
      | grapheme :: rest ->
          let grapheme_width = Std.Unicode.Grapheme.width grapheme in
          let next_width = current_width + grapheme_width in
          if current = [] || next_width <= width then
            loop (grapheme :: current) next_width acc rest
          else
            let acc = flush current acc in
            loop [ grapheme ] grapheme_width acc rest
    in
    loop [] 0 [] graphemes

let wrap_paragraph = fun (style: Style.t) max_width paragraph ->
  match (style.Style.text_wrap, max_width) with
  | (Style.NoWrap, _) -> [ paragraph ]
  | (_, None) -> [ paragraph ]
  | (_, Some width) when width <= 0 -> [ "" ]
  | (Style.Words, Some width) ->
      let lines = String.wrap_words ~width paragraph in
      if lines = [] then
        [ "" ]
      else
        lines
  | (Style.Character, Some width) -> wrap_characters ~width paragraph

let default_text_measurer = fun ~constraints text style ->
  let width_hint =
    match constraints.available_width with
    | Some width -> Some (Float.to_int (Float.floor (clamp_non_negative width)))
    | None -> None
  in
  let paragraphs = String.split_on_char '\n' text in
  let lines_rev =
    List.fold_left
      paragraphs
      ~init:[]
      ~fn:(fun acc paragraph ->
        let wrapped = wrap_paragraph style width_hint paragraph in
        List.fold_left wrapped ~init:acc ~fn:(fun acc line -> line :: acc))
  in
  let lines =
    match List.rev lines_rev with
    | [] -> [ "" ]
    | lines -> lines
  in
  let width =
    List.fold_left lines ~init:0 ~fn:(fun acc line -> Int.max acc (String.width line))
    |> Float.from_int
  in
  let height =
    Int.max 1 (List.length lines)
    |> Float.from_int
  in
  { size = Viewport.make ~width ~height; lines }

let make = fun ~viewport ~text_measurer () -> { viewport; text_measurer }
