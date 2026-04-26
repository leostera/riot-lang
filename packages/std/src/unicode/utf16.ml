open Prelude

module String = Kernel.String

type position = { line: int; character: int }

let code_units_of_rune = fun rune ->
  if Rune.to_int rune > 0xffff then
    2
  else
    1

type step =
  | Newline of int
  | Rune of Rune.t * int

let next_step = fun text byte_offset ->
  if byte_offset >= String.length text then
    None
  else
    match String.get text ~at:byte_offset with
    | Some '\r' ->
        if byte_offset + 1 < String.length text then
          match String.get text ~at:(byte_offset + 1) with
          | Some '\n' -> Some (Newline 2)
          | _ -> Some (Newline 1)
        else
          Some (Newline 1)
    | Some '\n' -> Some (Newline 1)
    | Some _ -> (
        match Utf8.decode_rune text byte_offset with
        | Some (rune, next_byte_offset) -> Some (Rune (rune, next_byte_offset - byte_offset))
        | None -> Some (Rune (Rune.replacement, 1))
      )
    | None -> None

let position_of_offset = fun text ~offset ->
  let target = Int.min (String.length text) (Int.max 0 offset) in
  let rec loop byte_offset line character =
    if byte_offset >= target then
      { line; character }
    else
      match next_step text byte_offset with
      | None -> { line; character }
      | Some (Newline width) -> loop (byte_offset + width) (line + 1) 0
      | Some (Rune (rune, width)) ->
          loop (byte_offset + width) line (character + code_units_of_rune rune)
  in
  loop 0 0 0

let offset_of_position = fun text ({ line; character } as position) ->
  if line < 0 || character < 0 then
    Error "UTF-16 positions must be non-negative"
  else
    let rec find_line byte_offset current_line =
      if current_line = line then
        find_character byte_offset 0
      else if byte_offset >= String.length text then
        Error ("line " ^ Int.to_string line ^ " is beyond the end of the document")
      else
        match next_step text byte_offset with
        | None -> Error ("line " ^ Int.to_string line ^ " is beyond the end of the document")
        | Some (Newline width) -> find_line (byte_offset + width) (current_line + 1)
        | Some (Rune (_, width)) -> find_line (byte_offset + width) current_line
    and find_character byte_offset current_character =
      if current_character = character then
        Ok byte_offset
      else if byte_offset >= String.length text then
        Error ("character "
        ^ Int.to_string character
        ^ " is beyond the end of line "
        ^ Int.to_string position.line)
      else
        match next_step text byte_offset with
        | None ->
            Error ("character "
            ^ Int.to_string character
            ^ " is beyond the end of line "
            ^ Int.to_string position.line)
        | Some (Newline _) ->
            Error ("character "
            ^ Int.to_string character
            ^ " is beyond the end of line "
            ^ Int.to_string position.line)
        | Some (Rune (rune, width)) ->
            let next_character = current_character + code_units_of_rune rune in
            if next_character = character then
              Ok (byte_offset + width)
            else if next_character > character then
              Error "position splits a UTF-16 surrogate pair"
            else
              find_character (byte_offset + width) next_character
    in
    find_line 0 0
