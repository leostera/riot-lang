open Std

type t = { headers : (string * string) List.t; body : string }

let make ~headers ~body = { headers; body }
let headers t = t.headers
let body t = t.body

let parse_header line =
  match String.index_opt line ':' with
  | None -> Error "Invalid header: missing colon"
  | Some idx ->
      let name = String.sub line 0 idx in
      let value_start = idx + 1 in
      let value =
        if value_start < String.length line then
          String.sub line value_start (String.length line - value_start)
        else ""
      in
      let trimmed_value = String.trim value in
      Ok (name, trimmed_value)

let of_string s =
  match String.index_opt s '\n' with
  | None -> Error "No newline found in message"
  | Some _ -> (
      let lines = String.split_on_char '\n' s in

      let rec collect_headers acc current_header lines =
        match lines with
        | [] -> Error "No blank line found separating headers from body"
        | line :: rest -> (
            let trimmed = String.trim line in
            if trimmed = "" then
              let final_headers =
                match current_header with Some h -> h :: acc | None -> acc
              in
              let body = String.concat "\n" rest in
              let body = if body = "" then body else body ^ "\n" in
              Ok (List.rev final_headers, body)
            else if String.length line > 0 && (line.[0] = ' ' || line.[0] = '\t')
            then
              match current_header with
              | None -> Error "Header continuation without initial header"
              | Some (name, value) ->
                  let continuation = String.trim line in
                  collect_headers acc
                    (Some (name, value ^ " " ^ continuation))
                    rest
            else
              match parse_header line with
              | Error e -> Error e
              | Ok (name, value) ->
                  let acc' =
                    match current_header with Some h -> h :: acc | None -> acc
                  in
                  collect_headers acc' (Some (name, value)) rest)
      in

      match collect_headers [] None lines with
      | Error e -> Error e
      | Ok (headers, body) -> Ok { headers; body })

let to_string t =
  let header_lines =
    List.map (fun (name, value) -> format "%s: %s" name value) t.headers
  in
  String.concat "\n" (header_lines @ [ ""; t.body ])

let to_json t =
  let header_obj =
    List.map (fun (name, value) -> (name, Data.Json.String value)) t.headers
  in
  Data.Json.Object
    [
      ("headers", Data.Json.Object header_obj); ("body", Data.Json.String t.body);
    ]
