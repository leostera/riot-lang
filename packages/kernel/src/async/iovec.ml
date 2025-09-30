type iov = { ba : bytes; off : int; len : int }
type t = iov array

(** creates an iovector array with [size] equally distributed in [count]s *)
let create ?(count = 1) ~size () =
  assert (count > 0);
  assert (size > 0);
  let size = size / count in
  Array.init count (fun _id ->
      { ba = Bytes.create size; off = 0; len = size })

let with_capacity size = create ~size ()

let sub ?(pos = 0) ~len t =
  let curr = ref 0 in
  t |> Array.to_list
  |> List.filter_map (fun iov ->
      if !curr + iov.len < pos then (
        curr := !curr + iov.len;
        None)
      else
        let next_curr = iov.len + !curr in
        let diff = len - !curr in
        if next_curr < len then (
          curr := next_curr;
          Some iov)
        else if diff > 0 then (
          curr := len;
          Some { iov with len = diff })
        else None)
  |> Array.of_list

let length t = Array.fold_left (fun acc iov -> acc + (iov.len - iov.off)) 0 t
let iter (t : t) fn = Array.iter fn t
let of_bytes ba = [| { ba; off = 0; len = Bytes.length ba } |]
let from_string str = of_bytes (Bytes.of_string str)
let from_buffer buf = of_bytes (Buffer.to_bytes buf)

let into_string t =
  let buf = Buffer.create (length t) in
  iter t (fun iov -> Buffer.add_bytes buf (Bytes.sub iov.ba iov.off iov.len));
  Buffer.contents buf