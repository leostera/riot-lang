open Std

type reader_state = {
  reader: IO.BufReader.t;
  mutable view: IO.IoSlice.t;
  mutable base: int;
  mutable pos: int;
  mutable eof: bool;
}

type t =
  | String_input of { input: string; mutable pos: int }
  | Reader_input of reader_state

type scan_result =
[
  `Stop of int * char
  | `Boundary of int
  | `Eof of int
]

let default_capacity = 131_072

let of_string = fun input -> String_input { input; pos = 0 }

let of_reader = fun reader ->
  Reader_input {
    reader = IO.BufReader.from_reader ~size:default_capacity reader;
    view = IO.IoSlice.empty;
    base = 0;
    pos = 0;
    eof = false;
  }

let position = function
  | String_input state -> state.pos
  | Reader_input state -> state.base + state.pos

let local_index = fun state absolute -> absolute - state.base

let compact = fun state ->
  if state.pos > 0 then
    (
      ignore (IO.BufReader.consume state.reader ~len:state.pos);
      state.base <- state.base + state.pos;
      state.pos <- 0
    )

let refill = fun state ->
  if state.eof then
    false
  else (
    compact state;
    match IO.BufReader.buffered state.reader with
    | Ok slice ->
        state.view <- slice;
        true
    | Error IO.End_of_file ->
          state.eof <- true;
          state.view <- IO.IoSlice.empty;
          false
    | Error err ->
        raise (Serde.Decode_error (`Io_error err))
  )

let ensure = fun input needed ->
  match input with
  | String_input state -> state.pos + needed <= String.length state.input
  | Reader_input state ->
      let rec loop () =
        if state.pos + needed <= IO.IoSlice.length state.view then
          true
        else if state.eof then
          false
        else if refill state then
          loop ()
        else
          state.pos + needed <= IO.IoSlice.length state.view
      in
      loop ()

let peek_char = fun input ~offset ->
  if ensure input (offset + 1) then
    match input with
    | String_input state -> Some (String.unsafe_get state.input (state.pos + offset))
    | Reader_input state -> Some (IO.IoSlice.get_unchecked state.view ~at:(state.pos + offset))
  else
    None

let current_char = fun input -> peek_char input ~offset:0

let advance = function
  | String_input state -> state.pos <- state.pos + 1
  | Reader_input state -> state.pos <- state.pos + 1

let advance_by = fun input count ->
  match input with
  | String_input state -> state.pos <- state.pos + count
  | Reader_input state -> state.pos <- state.pos + count

let set_position = fun input absolute ->
  match input with
  | String_input state -> state.pos <- absolute
  | Reader_input state -> state.pos <- local_index state absolute

let remaining = function
  | String_input state -> String.length state.input - state.pos
  | Reader_input state -> IO.IoSlice.length state.view - state.pos

let slice_to_string = fun input ~start ~stop ->
  match input with
  | String_input state -> String.sub state.input ~offset:start ~len:(stop - start)
  | Reader_input state ->
      let local_start = local_index state start in
      IO.IoSlice.sub_unchecked state.view ~off:local_start ~len:(stop - start)
      |> IO.IoSlice.to_string

let copy_range_to_buffer = fun buffer input ~start ~stop ->
  let length = stop - start in
  if length > 0 then
    match input with
    | String_input state -> IO.Buffer.add_substring buffer state.input start length
    | Reader_input state ->
        IO.Buffer.append_slice
          buffer
          (IO.IoSlice.sub_unchecked state.view ~off:(local_index state start) ~len:length)
        |> Result.expect ~msg:"serde-json buffered input should append borrowed slices"

let match_field_range = fun fields input ~start ~stop ->
  let length = stop - start in
  match input with
  | String_input state -> Serde.De.Fields.match_slice fields state.input ~offset:start ~length
  | Reader_input state -> Serde.De.Fields.match_ioslice
    fields
    state.view
    ~offset:(local_index state start)
    ~length

let skip_whitespace = function
  | String_input state ->
      let input = state.input in
      let length = String.length input in
      let rec loop pos =
        if pos >= length then
          pos
        else
          match String.unsafe_get input pos with
          | ' '
          | '\t'
          | '\n'
          | '\r' -> loop (pos + 1)
          | _ -> pos
      in
      state.pos <- loop state.pos
  | Reader_input state ->
      let rec loop () =
        let rec advance_local pos =
          if pos >= IO.IoSlice.length state.view then
            pos
          else
            match IO.IoSlice.get_unchecked state.view ~at:pos with
            | ' '
            | '\t'
            | '\n'
            | '\r' -> advance_local (pos + 1)
            | _ -> pos
        in
        state.pos <- advance_local state.pos;
        if state.pos >= IO.IoSlice.length state.view && not state.eof && refill state then
          loop ()
      in
      loop ()

let scan_while = fun input ~continue ->
  match input with
  | String_input state ->
      let length = String.length state.input in
      let rec loop pos =
        if pos >= length then
          `Eof pos
        else
          let current = String.unsafe_get state.input pos in
          if continue current then
            loop (pos + 1)
          else
            `Stop (pos, current)
      in
      loop state.pos
  | Reader_input state ->
      let rec loop () =
        if state.pos >= IO.IoSlice.length state.view then
          if refill state then
            loop ()
          else
            `Eof (state.base + state.pos)
        else
          let rec scan local =
            if local >= IO.IoSlice.length state.view then
              `Boundary (state.base + local)
            else
              let current = IO.IoSlice.get_unchecked state.view ~at:local in
              if continue current then
                scan (local + 1)
              else
                `Stop (state.base + local, current)
          in
          scan state.pos
      in
      loop ()
