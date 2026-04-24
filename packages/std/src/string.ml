open Collections
open Iter
open Prelude
module Rune = Kernel.Unicode.Rune

external bytes_unsafe_of_string: string -> bytes = "%bytes_of_string"

include Kernel.String

let from_char = fun value -> make ~len:1 ~char:value

module MutIter = struct
  type state = {
    source: string;
    mutable current_pos: int;
  }

  type item = Unicode.Rune.t

  let next = fun state ->
    if state.current_pos < length state.source then
      (
        let utf_decoded =
          match get_utf_8_rune state.source ~at:state.current_pos with
          | Some utf_decoded -> utf_decoded
          | None -> Kernel.SystemError.panic "Std.String: invalid utf-8 input"
        in
        let char_size = Rune.utf_decode_length utf_decoded in
        let item = Rune.utf_decode_rune utf_decoded in
        state.current_pos <- state.current_pos + char_size;
        Some item
      )
    else
      None

  let size = fun { current_pos; source } -> length source - current_pos

  let clone = fun { source; current_pos } -> { source; current_pos }
end

let into_mut_iter = fun source ->
  MutIterator.make (module MutIter) { source; current_pos = 0 }

module Iter = struct
  type state = {
    source: string;
    current_pos: int;
  }

  type item = Unicode.Rune.t

  let next = fun ({ source; current_pos } as state) ->
    if current_pos < length source then
      let utf_decoded =
        match get_utf_8_rune source ~at:current_pos with
        | Some utf_decoded -> utf_decoded
        | None -> Kernel.SystemError.panic "Std.String: invalid utf-8 input"
      in
      let char_size = Rune.utf_decode_length utf_decoded in
      let item = Rune.utf_decode_rune utf_decoded in
      (Some item, { state with current_pos = current_pos + char_size })
    else
      (None, state)

  let size = fun { current_pos; source } -> length source - current_pos
end

let into_iter = fun source ->
  Iterator.make (module Iter) { source; current_pos = 0 }

let iter = fun fn value -> for_each ~fn value

let fold_left = fun ~fn ~init value -> Kernel.String.fold_left ~fn:fn ~acc:init value

let split_on_char = fun separator value -> split ~by:(from_char separator) value

(* Unicode-aware operations *)

let width = fun s ->
  (* Calculate display width by summing rune widths *)
  into_iter s
  |> Iterator.to_list
  |> List.fold_left ~init:0 ~fn:(fun acc rune -> acc + Unicode.Rune.width rune)

let rune_count = fun s -> into_iter s |> Iterator.to_list |> List.length

let grapheme_count = fun s ->
  (* Count grapheme clusters *)
  let rec count pos acc =
    if pos >= length s then
      acc
    else
      match Unicode.Grapheme.first (sub s ~offset:pos ~len:(length s - pos)) with
      | None -> acc
      | Some (_, rest) ->
          let consumed = length s - pos - length rest in
          count (pos + consumed) (acc + 1)
  in
  count 0 0

let truncate_width = fun ~width:target_width ?(tail = "…") s ->
  let s_width = width s in
  if s_width <= target_width then
    s
  else
    let tail_width = length tail in
    (* Simplified: assume ASCII tail *)
    let max_width = target_width - tail_width in
    if max_width <= 0 then
      tail
    else
      (* Find position where width exceeds target *)
      let rec find_cut pos acc_width =
        if pos >= length s then
          s
        else
          let decode =
            match get_utf_8_rune s ~at:pos with
            | Some decode -> decode
            | None -> Kernel.SystemError.panic "Std.String: invalid utf-8 input"
          in
          if Rune.utf_decode_is_valid decode then
            let rune = Rune.utf_decode_rune decode in
            let rune_w = Unicode.Rune.width rune in
            if acc_width + rune_w > max_width then
              sub s ~offset:0 ~len:pos ^ tail
            else
              let len = Rune.utf_decode_length decode in
              find_cut (pos + len) (acc_width + rune_w)
          else
            sub s ~offset:0 ~len:pos ^ tail
      in
      find_cut 0 0

let pad_left = fun ~width:target_width pad_char s ->
  let s_width = width s in
  if s_width >= target_width then
    s
  else
    let padding = make ~len:(target_width - s_width) ~char:pad_char in
    padding ^ s

let pad_right = fun ~width:target_width pad_char s ->
  let s_width = width s in
  if s_width >= target_width then
    s
  else
    let padding = make ~len:(target_width - s_width) ~char:pad_char in
    s ^ padding

let pad_center = fun ~width:target_width pad_char s ->
  let s_width = width s in
  if s_width >= target_width then
    s
  else
    let total_padding = target_width - s_width in
    let left_padding = total_padding / 2 in
    let right_padding = total_padding - left_padding in
    make ~len:left_padding ~char:pad_char ^ s ^ make ~len:right_padding ~char:pad_char

(* Grapheme iterators *)

module GraphemeMutIter = struct
  type state = {
    source: string;
    mutable current_pos: int;
  }

  type item = Unicode.Grapheme.t

  let next = fun state ->
    if state.current_pos < length state.source then
      let remaining = sub
        state.source
        ~offset:state.current_pos
        ~len:(length state.source - state.current_pos) in
      match Unicode.Grapheme.first remaining with
      | None -> None
      | Some (grapheme, rest) ->
          let consumed = length remaining - length rest in
          state.current_pos <- state.current_pos + consumed;
          Some grapheme
    else
      None

  let size = fun { current_pos; source } -> length source - current_pos

  let clone = fun { source; current_pos } -> { source; current_pos }
end

let into_grapheme_mut_iter = fun source ->
  MutIterator.make (module GraphemeMutIter) { source; current_pos = 0 }

module GraphemeIter = struct
  type state = {
    source: string;
    current_pos: int;
  }

  type item = Unicode.Grapheme.t

  let next = fun ({ source; current_pos } as state) ->
    if current_pos < length source then
      let remaining = sub source ~offset:current_pos ~len:(length source - current_pos) in
      match Unicode.Grapheme.first remaining with
      | None -> (None, state)
      | Some (grapheme, rest) ->
          let consumed = length remaining - length rest in
          (Some grapheme, { state with current_pos = current_pos + consumed })
    else
      (None, state)

  let size = fun { current_pos; source } -> length source - current_pos
end

let into_grapheme_iter = fun source ->
  Iterator.make (module GraphemeIter) { source; current_pos = 0 }

(* Text segmentation *)

let word_boundaries = fun s -> Unicode.Segmentation.find_word_boundaries s

let split_words = fun s ->
  let boundaries = word_boundaries s in
  let rec split = fun start ->
    function
    | [] ->
        if start < length s then
          [ sub s ~offset:start ~len:(length s - start) ]
        else
          []
    | pos :: rest ->
        let word = trim (sub s ~offset:start ~len:(pos - start)) in
        if word = "" then
          split pos rest
        else
          word :: split pos rest
  in
  split 0 boundaries

let line_breaks = fun s -> Unicode.Segmentation.find_line_breaks s

let wrap = fun ~width:_ s ->
  (* Simplified: split on whitespace *)
  split ~by:" " s |> List.filter ~fn:(fun w -> w != "")

let wrap_words = fun ~width:target_width s ->
  let words = split_words s in
  let rec build_lines = fun current_line current_width ->
    function
    | [] ->
        if current_line = "" then
          []
        else
          [ trim current_line ]
    | word :: rest ->
        let word_width = width word in
        let space_width =
          if current_line = "" then
            0
          else
            1
        in
        if current_width + space_width + word_width <= target_width then
          let new_line =
            if current_line = "" then
              word
            else
              current_line ^ " " ^ word
          in
          build_lines new_line (current_width + space_width + word_width) rest
        else if current_line = "" then
          word :: build_lines "" 0 rest
        else
          (* Start new line *)
          trim current_line :: build_lines word word_width rest
  in
  build_lines "" 0 words

(** Check if a string contains a substring *)
let contains = fun haystack needle ->
  let needle_len = length needle in
  let haystack_len = length haystack in
  if needle_len = 0 then
    true
  else if needle_len > haystack_len then
    false
  else
    let rec check pos =
      if pos > haystack_len - needle_len then
        false
      else if sub haystack ~offset:pos ~len:needle_len = needle then
        true
      else
        check (pos + 1)
    in
    check 0

module Read = struct
  type t = {
    mutable offset: int;
    chunk_size: int;
    source: string;
    source_length: int;
    source_bytes: bytes;
  }

  type progress = {
    mutable total: int;
    mutable continue_: bool;
  }

  let panic_buffer_error = fun fn error ->
    Kernel.SystemError.panic ("Std.String.to_reader." ^ fn ^ ": " ^ Kernel.IO.Error.message error)

  let read = fun state ~into ->
    let remaining = state.source_length - state.offset in
    if Int.equal remaining 0 then
      Ok 0
    else
      let available =
        if IO.Buffer.writable_bytes into = 0 then
          (
            match IO.Buffer.ensure_free into state.chunk_size with
            | Ok () -> IO.Buffer.writable_bytes into
            | Error error -> panic_buffer_error "ensure_free" error
          )
        else
          IO.Buffer.writable_bytes into
      in
      let to_read = min state.chunk_size (min remaining available) in
      begin
        match IO.Buffer.append_subbytes into state.source_bytes ~off:state.offset ~len:to_read with
        | Ok () -> ()
        | Error error -> panic_buffer_error "append_subbytes" error
      end;
      state.offset <- state.offset + to_read;
      Ok to_read

  let read_vectored = fun state ~into:bufs ->
    let progress = { total = 0; continue_ = true } in
    IO.IoVec.for_each
      ~fn:(fun segment ->
        if progress.continue_ then
          (
            let remaining = state.source_length - state.offset in
            if Int.equal remaining 0 then
              progress.continue_ <- false
            else
              let segment_length = IO.IoVec.IoSlice.length segment in
              let to_read = min state.chunk_size (min remaining segment_length) in
              IO.IoVec.IoSlice.blit_from_bytes_unchecked
                state.source_bytes
                ~src_off:state.offset
                segment
                ~dst_off:0
                ~len:to_read;
              state.offset <- state.offset + to_read;
              progress.total <- progress.total + to_read;
              if to_read < segment_length then
                progress.continue_ <- false
              else
                ()
          ))
      bufs;
    Ok progress.total

  let is_read_vectored = fun _ -> true
end

let to_reader = fun ?chunk_size value ->
  let chunk_size =
    match chunk_size with
    | None -> max 1 (length value)
    | Some chunk_size ->
        if chunk_size <= 0 then
          raise (Invalid_argument "Std.String.to_reader: chunk_size must be positive");
        chunk_size
  in
  let state =
    Read.{
      chunk_size;
      offset = 0;
      source = value;
      source_length = length value;
      source_bytes = bytes_unsafe_of_string value;
    }
  in
  IO.Reader.from_source (module Read) state

module Syntax = struct
  let set_unchecked = fun value ~at ~char ->
    let bytes = bytes_unsafe_of_string value in
    Kernel.Bytes.set_unchecked bytes ~at ~char

  let get = fun value at -> get_unchecked value ~at

  let set = fun value at char -> set_unchecked value ~at ~char
end
