open Kernel

type t = bytes

type error = Kernel.Bytes.error =
  | OutOfBoundSet of { bytes: bytes; lenght: int; at: int; char: char }

let create = fun ~size -> Kernel.Bytes.create ~size

let length = Kernel.Bytes.length

let get = fun value ~at -> Kernel.Bytes.get value ~at

let get_unchecked = fun value ~at -> Kernel.Bytes.get_unchecked value ~at

let set = fun value ~at ~char -> Kernel.Bytes.set value ~at ~char

let set_unchecked = fun value ~at ~char -> Kernel.Bytes.set_unchecked value ~at ~char

let blit = fun src ~src_offset ~dst ~dst_offset ~len ->
  Kernel.Bytes.blit src ~src_offset ~dst ~dst_offset ~len

let blit_unchecked = fun src ~src_offset ~dst ~dst_offset ~len ->
  Kernel.Bytes.blit_unchecked src ~src_offset ~dst ~dst_offset ~len

let blit_string = fun src ~src_offset ~dst ~dst_offset ~len ->
  let slice = Kernel.String.sub src ~offset:src_offset ~len in
  let src_bytes = Kernel.String.to_bytes slice in
  Kernel.Bytes.blit_unchecked src_bytes ~src_offset:0 ~dst ~dst_offset ~len

let fill = fun value ~offset ~len ~char -> Kernel.Bytes.fill value ~offset ~len ~char

let from_string = Kernel.Bytes.from_string

let to_string = Kernel.Bytes.to_string

let unsafe_to_string = Kernel.Bytes.unsafe_to_string

let sub = fun value ~offset ~len -> Kernel.Bytes.sub value ~offset ~len

let sub_unchecked = fun value ~offset ~len -> Kernel.Bytes.sub_unchecked value ~offset ~len
