open Std

type reader_state = {
  reader: IO.Reader.t;
  buffer: IO.Buffer.t;
  mutable base: int;
  mutable pos: int;
  mutable eof: bool;
}

type t =
  | String_input of {
      input: string;
      mutable pos: int;
    }
  | Reader_input of reader_state

type scan_result = [`Stop of int * char | `Boundary of int | `Eof of int]

let default_capacity = 131_072

let from_string = fun input -> String_input { input; pos = 0 }

let from_reader = fun reader ->
  Reader_input {
    reader;
    buffer = IO.Buffer.create ~size:default_capacity;
    base = 0;
    pos = 0;
    eof = false;
  }

let position = fun __tmp1 ->
  match __tmp1 with
  | String_input state -> state.pos
  | Reader_input state -> state.base + state.pos

let local_index = fun state absolute -> absolute - state.base

let compact = fun state ->
  if Int.(state.pos > 0) then (
    ignore (IO.Buffer.consume state.buffer ~len:state.pos);
    state.base <- state.base + state.pos;
    state.pos <- 0
  )

let reader_length = fun state -> IO.Buffer.readable_bytes state.buffer

let reader_get_unchecked = fun state ~at -> IO.Buffer.get_unchecked state.buffer ~at

let reader_slice = fun state -> IO.Buffer.readable state.buffer

let reader_subslice = fun state ~off ~len -> IO.IoSlice.sub_unchecked (reader_slice state) ~off ~len

let reader_substring = fun state ~off ~len ->
  reader_subslice state ~off ~len
  |> IO.IoSlice.to_string

let reader_append_range = fun dst state ~start ~stop ->
  let len = stop - start in
  if Int.(len > 0) then
    IO.Buffer.append_subslice dst (reader_slice state) ~off:start ~len
    |> Result.expect ~msg:"serde-json buffered input should append borrowed slices"

let reader_match_field_range = fun fields state ~offset ~length ->
  Serde.De.Fields.match_buffer_range
    fields
    state.buffer
    ~offset
    ~length

let refill = fun state ->
  if state.eof then
    false
  else (
    compact state;
    match IO.Reader.read state.reader ~into:state.buffer with
    | Ok 0 ->
        state.eof <- true;
        false
    | Ok _ -> true
    | Error err -> raise (Serde.Decode_error (`Io_error err))
  )

let ensure = fun input needed ->
  match input with
  | String_input state -> Int.(state.pos + needed <= String.length state.input)
  | Reader_input state ->
      let rec loop () =
        if Int.(state.pos + needed <= reader_length state) then
          true
        else if state.eof then
          false
        else if refill state then
          loop ()
        else
          Int.(state.pos + needed <= reader_length state)
      in
      loop ()

let peek_char = fun input ~offset ->
  if ensure input (offset + 1) then
    match input with
    | String_input state -> Some (String.unsafe_get state.input (state.pos + offset))
    | Reader_input state -> Some (reader_get_unchecked state ~at:(state.pos + offset))
  else
    None

let current_char = fun input -> peek_char input ~offset:0

let advance = fun __tmp1 ->
  match __tmp1 with
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

let remaining = fun __tmp1 ->
  match __tmp1 with
  | String_input state -> String.length state.input - state.pos
  | Reader_input state -> reader_length state - state.pos

let slice_to_string = fun input ~start ~stop ->
  match input with
  | String_input state -> String.sub state.input ~offset:start ~len:(stop - start)
  | Reader_input state ->
      let local_start = local_index state start in
      reader_substring state ~off:local_start ~len:(stop - start)

let copy_range_to_buffer = fun buffer input ~start ~stop ->
  let length = stop - start in
  if Int.(length > 0) then
    match input with
    | String_input state -> IO.Buffer.add_substring buffer state.input start length
    | Reader_input state ->
        reader_append_range
          buffer
          state
          ~start:(local_index state start)
          ~stop:(local_index state stop)

let match_field_range = fun fields input ~start ~stop ->
  let length = stop - start in
  match input with
  | String_input state -> Serde.De.Fields.match_slice fields state.input ~offset:start ~length
  | Reader_input state ->
      reader_match_field_range
        fields
        state
        ~offset:(local_index state start)
        ~length

let skip_whitespace = fun __tmp1 ->
  match __tmp1 with
  | String_input state ->
      let input = state.input in
      let length = String.length input in
      let rec loop pos =
        if Int.(pos >= length) then
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
          if Int.(pos >= reader_length state) then
            pos
          else
            match reader_get_unchecked state ~at:pos with
            | ' '
            | '\t'
            | '\n'
            | '\r' -> advance_local (pos + 1)
            | _ -> pos
        in
        state.pos <- advance_local state.pos;
        if Int.(state.pos >= reader_length state) && not state.eof && refill state then
          loop ()
      in
      loop ()

let scan_while = fun input ~continue ->
  match input with
  | String_input state ->
      let length = String.length state.input in
      let rec loop pos =
        if Int.(pos >= length) then
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
        if Int.(state.pos >= reader_length state) then
          if refill state then
            loop ()
          else
            `Eof (state.base + state.pos)
        else
          let rec scan local =
            if Int.(local >= reader_length state) then
              `Boundary (state.base + local)
            else
              let current = reader_get_unchecked state ~at:local in
              if continue current then
                scan (local + 1)
              else
                `Stop (state.base + local, current)
          in
          scan state.pos
      in
      loop ()
