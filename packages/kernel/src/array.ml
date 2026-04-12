open Prelude

type 'value t = 'value array

let make = fun ~count ~value ->
  Caml_runtime.array_make count value

let init = fun ~count ~fn ->
  if count = 0 then
    [||]
  else
    let out = make ~count ~value:(fn 0) in
    let rec fill index =
      if index >= count then
        out
      else (
        Caml_runtime.array_set out index (fn index);
        fill (index + 1)
      )
    in
    let _ = fill 1 in
    out

let length = Caml_runtime.array_length

let get = fun values ~at ->
  if at < 0 || at >= length values then
    None
  else
    Some (Caml_runtime.array_get values at)

let get_unchecked = fun values ~at ->
  Caml_runtime.array_unsafe_get values at

let set = fun values ~at ~value ->
  if at < 0 || at >= length values then
    System_error.panic ("Array.set received an out-of-bounds index: " ^ Int.to_string at)
  else
    Caml_runtime.array_set values at value

let set_unchecked = fun values ~at ~value ->
  Caml_runtime.array_unsafe_set values at value

let clone = fun values ->
  let count = length values in
  if count = 0 then
    [||]
  else
    init ~count ~fn:(fun index -> get_unchecked values ~at:index)

let blit = fun source ~src_offset ~dst ~dst_offset ~len ->
  if len <= 0 then
    ()
  else if Ptr.equal source dst && dst_offset > src_offset then
    let rec loop index =
      if index < 0 then
        ()
      else (
        set_unchecked
          dst
          ~at:(dst_offset + index)
          ~value:(get_unchecked source ~at:(src_offset + index));
        loop (index - 1)
      )
    in
    loop (len - 1)
  else
    let rec loop index =
      if index >= len then
        ()
      else (
        set_unchecked
          dst
          ~at:(dst_offset + index)
          ~value:(get_unchecked source ~at:(src_offset + index));
        loop (index + 1)
      )
    in
    loop 0

let sub = fun values ~offset ~len ->
  if len = 0 then
    [||]
  else
    init ~count:len ~fn:(fun index -> get_unchecked values ~at:(offset + index))

let for_each = fun array ~fn ->
  let rec loop index =
    if index >= length array then
      ()
    else (
      fn (get_unchecked array ~at:index);
      loop (index + 1)
    )
  in
  loop 0

let map = fun array ~fn ->
  let count = length array in
  if count = 0 then
    [||]
  else
    let out = make ~count ~value:(fn (get_unchecked array ~at:0)) in
    let rec fill index =
      if index >= count then
        out
      else (
        set_unchecked out ~at:index ~value:(fn (get_unchecked array ~at:index));
        fill (index + 1)
      )
    in
    let _ = fill 1 in
    out

let fold_left = fun array ~fn ~acc ->
  let rec loop index acc =
    if index >= length array then
      acc
    else
      loop (index + 1) (fn acc (get_unchecked array ~at:index))
  in
  loop 0 acc

let fold_right = fun array ~fn ~acc ->
  let rec loop index acc =
    if index < 0 then
      acc
    else
      loop (index - 1) (fn (get_unchecked array ~at:index) acc)
  in
  loop (length array - 1) acc

let from_list = fun value ->
  match value with
  | [] -> [||]
  | head :: tail ->
      let rec list_length acc = function
        | [] -> acc
        | _ :: rest -> list_length (acc + 1) rest
      in
      let out = make ~count:(list_length 1 tail) ~value:head in
      let rec fill index = function
        | [] -> out
        | value :: rest ->
            set_unchecked out ~at:index ~value;
            fill (index + 1) rest
      in
      fill 1 tail
