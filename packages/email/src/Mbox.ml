open Std
open Std.Iter

type entry = {
  envelope_from : string Option.t;
  envelope_date : string Option.t;
  message : Message.t;
}

type t = { file : Fs.File.t }

let of_file file = Ok { file }

let is_separator_line line =
  String.length line >= 5 && String.sub line 0 5 = "From "

let parse_separator line =
  if not (is_separator_line line) then Error "Not a valid separator line"
  else
    let rest = String.sub line 5 (String.length line - 5) in
    match String.index_opt rest ' ' with
    | None -> Ok (String.trim rest, "")
    | Some idx ->
        let addr = String.sub rest 0 idx in
        let date_start = idx + 1 in
        let date =
          if date_start < String.length rest then
            String.sub rest date_start (String.length rest - date_start)
          else ""
        in
        Ok (String.trim addr, String.trim date)

module MboxIter = struct
  type state = {
    file : Fs.File.t;
    finished : bool Cell.t;
    next_separator : string Option.t Cell.t;
  }

  type item = entry

  let rec next state =
    if Cell.get state.finished then None
    else
      let separator_line =
        match Cell.get state.next_separator with
        | Some sep ->
            Cell.set state.next_separator None;
            sep
        | None ->
            let rec find_separator () =
              match Fs.File.read_line state.file with
              | Error _ ->
                  Cell.set state.finished true;
                  ""
              | Ok line ->
                  if line = "" then (
                    Cell.set state.finished true;
                    "")
                  else if is_separator_line line then line
                  else find_separator ()
            in
            find_separator ()
      in

      if separator_line = "" then None
      else
        let lines = Cell.create [] in
        let rec read_lines () =
          match Fs.File.read_line state.file with
          | Error _ -> ()
          | Ok line ->
              if line = "" then ()
              else if is_separator_line line then (
                Cell.set state.next_separator (Some line);
                ())
              else (
                Cell.set lines (line :: Cell.get lines);
                read_lines ())
        in
        read_lines ();

        let content_lines = List.rev (Cell.get lines) in
        let content = String.concat "\n" content_lines in

        let envelope_from, envelope_date =
          match parse_separator separator_line with
          | Ok (addr, date) ->
              let from = if addr = "" then None else Some addr in
              let dt = if date = "" then None else Some date in
              (from, dt)
          | Error _ -> (None, None)
        in

        match Message.of_string content with
        | Error _ -> next state
        | Ok message -> Some { envelope_from; envelope_date; message }

  let size _state = 0

  let clone state =
    {
      state with
      finished = Cell.create (Cell.get state.finished);
      next_separator = Cell.create (Cell.get state.next_separator);
    }
end

let into_mut_iter t =
  MutIterator.make
    (module MboxIter)
    {
      MboxIter.file = t.file;
      finished = Cell.create false;
      next_separator = Cell.create None;
    }
