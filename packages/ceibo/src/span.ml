open Std

type t = { start: int; end_: int }

let make = fun ~start ~end_ -> { start; end_ }

let length = fun t -> t.end_ - t.start

let contains = fun t offset -> offset >= t.start && offset < t.end_

let overlaps = fun t1 t2 -> t1.start < t2.end_ && t2.start < t1.end_

let union = fun t1 t2 -> { start = min t1.start t2.start; end_ = max t1.end_ t2.end_ }

let to_string = fun t -> Int.to_string t.start ^ ".." ^ Int.to_string t.end_

let to_json = fun t -> Data.Json.(Object [
  "start", Int t.start;
  "end", Int t.end_;
])
