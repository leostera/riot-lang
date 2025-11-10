open Std

type t = { start : int; end_ : int }

let make ~start ~end_ = { start; end_ }
let length t = t.end_ - t.start
let contains t offset = offset >= t.start && offset < t.end_
let overlaps t1 t2 = t1.start < t2.end_ && t2.start < t1.end_
let union t1 t2 = { start = min t1.start t2.start; end_ = max t1.end_ t2.end_ }
let to_string t = Int.to_string t.start ^ ".." ^ Int.to_string t.end_

let to_json t =
  Data.Json.(Object [ ("start", Int t.start); ("end", Int t.end_) ])
