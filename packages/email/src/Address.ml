open Std
open Std.IO

type t = {
  display_name : string Option.t;
  local_part : string;
  domain : string;
  local_was_quoted : bool;
}

let make ?display_name ~local_part ~domain =
  { display_name; local_part; domain; local_was_quoted = false }

let display_name t = t.display_name
let local_part t = t.local_part
let domain t = t.domain

let address t =
  let needs_quoting =
    t.local_was_quoted
    || String.contains t.local_part ' '
    || String.contains t.local_part '@'
    || String.contains t.local_part '"'
  in
  if needs_quoting then (
    (* Re-escape special characters for output *)
    let buf = Buffer.create (String.length t.local_part + 10) in
    String.iter
      (fun c ->
        match c with
        | '"' | '\\' ->
            Buffer.add_char buf '\\';
            Buffer.add_char buf c
        | _ -> Buffer.add_char buf c)
      t.local_part;
    "\"" ^ Buffer.contents buf ^ "\"@" ^ t.domain)
  else t.local_part ^ "@" ^ t.domain

let to_string t =
  match t.display_name with
  | None -> address t
  | Some name ->
      let needs_quoting =
        String.contains name "," || String.contains name "."
      in
      if needs_quoting then "\"" ^ name ^ "\" <" ^ address t ^ ">"
      else name ^ " <" ^ address t ^ ">"

let remove_comments s =
  let buf = Buffer.create (String.length s) in
  let rec loop i depth =
    if i >= String.length s then ()
    else
      let c = String.get s i in
      match c with
      | '(' -> loop (i + 1) (depth + 1)
      | ')' -> loop (i + 1) (depth - 1)
      | _ ->
          if depth = 0 then Buffer.add_char buf c;
          loop (i + 1) depth
  in
  loop 0 0;
  Buffer.contents buf

let normalize_whitespace s =
  let buf = Buffer.create (String.length s) in
  let rec loop i prev_was_space =
    if i >= String.length s then ()
    else
      let c = String.get s i in
      match c with
      | ' ' | '\n' | '\r' | '\t' ->
          if not prev_was_space then Buffer.add_char buf ' ';
          loop (i + 1) true
      | _ ->
          Buffer.add_char buf c;
          loop (i + 1) false
  in
  loop 0 true;
  String.trim (Buffer.contents buf)

let of_string s =
  let s = String.trim s in

  (* Find the last < to handle <<user@example.com>> *)
  let rec find_last_angle s pos last =
    match String.index_from_opt s pos '<' with
    | Some p -> find_last_angle s (p + 1) (Some p)
    | None -> last
  in

  (* Try to parse "Display Name <local@domain>" format *)
  match find_last_angle s 0 None with
  | Some angle_start -> (
      match String.index_opt s '>' with
      | Some angle_end -> (
          let display_part =
            String.sub s 0 angle_start |> remove_comments
            |> normalize_whitespace |> String.trim
          in
          let addr_part =
            String.sub s (angle_start + 1) (angle_end - angle_start - 1)
            |> String.trim
          in

          (* Extract display name, removing quotes if present and ignoring lone < *)
          let display_name =
            if display_part = "" || display_part = "<" then None
            else
              let cleaned =
                if
                  String.starts_with ~prefix:"\"" display_part
                  && String.ends_with ~suffix:"\"" display_part
                then String.sub display_part 1 (String.length display_part - 2)
                else display_part
              in
              Some cleaned
          in

          (* Parse the address part *)
          match String.index_opt addr_part '@' with
          | Some at_pos ->
              let local = String.sub addr_part 0 at_pos in
              let domain =
                String.sub addr_part (at_pos + 1)
                  (String.length addr_part - at_pos - 1)
              in
              Ok
                {
                  display_name;
                  local_part = local;
                  domain;
                  local_was_quoted = false;
                }
          | None -> Error "Missing @ in address")
      | None -> Error "Missing closing >")
  | None ->
      (* Simple format: local@domain *)
      (* Handle quoted local part with escaped characters *)
      let addr =
        if String.starts_with ~prefix:"\"" s then
          let rec find_closing_quote pos =
            if pos >= String.length s then None
            else
              match String.get s pos with
              | '\\' -> find_closing_quote (pos + 2) (* Skip escaped char *)
              | '"' -> Some pos
              | _ -> find_closing_quote (pos + 1)
          in
          match find_closing_quote 1 with
          | Some quote_end ->
              let local_quoted_raw = String.sub s 1 (quote_end - 1) in
              (* Unescape backslashes *)
              let buf = Buffer.create (String.length local_quoted_raw) in
              let rec unescape i =
                if i >= String.length local_quoted_raw then ()
                else if
                  String.get local_quoted_raw i = '\\'
                  && i + 1 < String.length local_quoted_raw
                then (
                  Buffer.add_char buf (String.get local_quoted_raw (i + 1));
                  unescape (i + 2))
                else (
                  Buffer.add_char buf (String.get local_quoted_raw i);
                  unescape (i + 1))
              in
              unescape 0;
              let local_quoted = Buffer.contents buf in
              let rest =
                String.sub s (quote_end + 1) (String.length s - quote_end - 1)
                |> String.trim
              in
              if String.starts_with ~prefix:"@" rest then
                let domain = String.sub rest 1 (String.length rest - 1) in
                Ok
                  {
                    display_name = None;
                    local_part = local_quoted;
                    domain;
                    local_was_quoted = true;
                  }
              else Error "Expected @ after quoted local part"
          | None -> Error "Unclosed quoted string"
        else
          match String.index_opt s '@' with
          | Some at_pos ->
              let local = String.sub s 0 at_pos in
              let domain =
                String.sub s (at_pos + 1) (String.length s - at_pos - 1)
              in
              Ok
                {
                  display_name = None;
                  local_part = local;
                  domain;
                  local_was_quoted = false;
                }
          | None -> Error "Missing @ in address"
      in
      addr
