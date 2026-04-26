open Std
open Std.Sync

type style =
  | Dots
  | Numerals

type t = {
  style: style;
  page: int;
  per_page: int;
  total_pages: int;
  active_dot: string;
  inactive_dot: string;
  numerals_format: int -> int -> string;
  text_style: Style.t;
}

let set_total_pages = fun t ~total:items ->
  if items < 1 then
    (t, t.total_pages)
  else
    let n = items / t.per_page in
    let n =
      if items mod t.per_page > 0 then
        n + 1
      else
        n
    in
    let new_t = { t with total_pages = n } in
    (new_t, n)

let get_slice_bounds = fun t length ->
  let start = t.page * t.per_page in
  let end_pos = min ((t.page * t.per_page) + t.per_page) length in
  (start, end_pos)

let items_on_page = fun t total_items ->
  if total_items < 1 then
    0
  else
    let (start, end_pos) = get_slice_bounds t total_items in
    end_pos - start

let on_last_page = fun t -> t.page = t.total_pages - 1

let on_first_page = fun t -> t.page = 0

let prev_page = fun t -> { t with page = max (t.page - 1) 0 }

let next_page = fun t ->
  if on_last_page t then
    t
  else
    { t with page = t.page + 1 }

let make = fun ?(style = Numerals) ?(page = 0) ?(per_page = 1) ?(total_pages = 1) ?(active_dot = "•") ?(inactive_dot = "○") ?(numerals_format = fun page total -> Int.to_string page ^ "/" ^ Int.to_string total) ?(text_style = Style.default) () ->
  {
    style;
    page;
    per_page;
    total_pages;
    active_dot;
    inactive_dot;
    numerals_format;
    text_style;
  }

let update = fun t (e: Event.t) ->
  match e with
  | KeyDown ((Key "h"
  | Left), _) -> prev_page t
  | KeyDown ((Key "l"
  | Right), _) -> next_page t
  | _ -> t

let dots_view = fun t text_style ->
  let result = Cell.create "" in
  for i = 0 to t.total_pages - 1 do
    let dot =
      if i = t.page then
        Style.render text_style t.active_dot
      else
        Style.render text_style t.inactive_dot
    in
    Cell.set result (Cell.get result ^ dot)
  done;
  Cell.get result

let numerals_view = fun t text_style ->
  let txt = t.numerals_format (t.page + 1) t.total_pages in
  Style.render text_style txt

let view = fun t ->
  match t.style with
  | Dots -> dots_view t t.text_style
  | Numerals -> numerals_view t t.text_style
