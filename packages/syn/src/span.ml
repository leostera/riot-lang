open Std

type t = { start: int; end_: int }

let make = fun ~start ~end_ -> { start; end_ }

let width = fun t -> t.end_ - t.start

let length = width

let compare = fun left right -> Int.compare (width left) (width right)

let contains = fun outer inner ->
  if Int.equal inner.start inner.end_ then
    inner.start >= outer.start && inner.start <= outer.end_
  else
    inner.start >= outer.start && inner.end_ <= outer.end_

let contains_offset = fun t offset -> offset >= t.start && offset < t.end_

let overlaps = fun t1 t2 -> t1.start < t2.end_ && t2.start < t1.end_

let starts_before = fun left right -> left.start < right.start

let ends_before = fun left right -> left.end_ < right.end_

let starts_after = fun left right -> left.start > right.start

let ends_after = fun left right -> left.end_ > right.end_

let union = fun t1 t2 -> { start = min t1.start t2.start; end_ = max t1.end_ t2.end_ }

let to_string = fun t -> Int.to_string t.start ^ ".." ^ Int.to_string t.end_

let to_json = fun t -> Data.Json.(Object [ ("start", Int t.start); ("end", Int t.end_) ])
