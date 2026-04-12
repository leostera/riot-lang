open Prelude

type t = exn = ..

type raw_backtrace_entry = private int

type raw_backtrace = raw_backtrace_entry array

type raw_backtrace_slot

type backtrace_slot =
  | Known_location of {
      is_raise: bool;
      filename: string;
      start_lnum: int;
      start_char: int;
      end_offset: int;
      end_lnum: int;
      end_char: int;
      is_inline: bool;
      defname: string
    }
  | Unknown_location of { is_raise: bool }

external to_string: exn -> string = "kernel_new_exception_to_string"

external get_raw_backtrace: unit -> raw_backtrace = "caml_get_exception_raw_backtrace"

external convert_raw_backtrace_slot: raw_backtrace_slot -> backtrace_slot = "caml_convert_raw_backtrace_slot"

external convert_raw_backtrace: raw_backtrace -> backtrace_slot array = "caml_convert_raw_backtrace"

external record_backtrace: bool -> unit = "caml_record_backtrace"

external backtrace_status: unit -> bool = "caml_backtrace_status"

external get_callstack: int -> raw_backtrace = "caml_get_current_callstack"

let convert_raw_backtrace = fun backtrace ->
  try Some (convert_raw_backtrace backtrace) with
  | Failure _ -> None

let format_backtrace_slot = fun position slot ->
  let info is_raise =
    if is_raise then
      if position = 0 then
        "Raised at"
      else
        "Re-raised at"
    else if position = 0 then
      "Raised by primitive operation at"
    else
      "Called from"
  in
  match slot with
  | Unknown_location { is_raise } ->
      if is_raise then
        None
      else
        Some (String.concat "" [ info false; " unknown location" ])
  | Known_location location ->
      let lines =
        if location.start_lnum = location.end_lnum then
          String.concat "" [ " "; Int.to_string location.start_lnum ]
        else
          String.concat
            ""
            [ "s "; Int.to_string location.start_lnum; "-"; Int.to_string location.end_lnum ]
      in
      Some (
        String.concat ""
          [
            info location.is_raise;
            " ";
            location.defname;
            " in file \"";
            location.filename;
            "\"";
            (
              if location.is_inline then
                " (inlined)"
              else
                ""
            );
            ", line";
            lines;
            ", characters ";
            Int.to_string location.start_char;
            "-";
            Int.to_string location.end_char
          ]
      )

let raw_backtrace_to_string = fun backtrace ->
  match convert_raw_backtrace backtrace with
  | None -> "(Program not linked with -g, cannot print stack backtrace)\n"
  | Some slots ->
      let rec loop index acc =
        if index >= Array.length slots then
          acc
        else
          let acc =
            match format_backtrace_slot index (Array.get slots index) with
            | None -> acc
            | Some line -> String.concat "" [ acc; line; "\n" ]
          in
          loop (index + 1) acc
      in
      loop 0 ""
