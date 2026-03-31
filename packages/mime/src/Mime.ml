open Std
open Std.Sync
open Std.IO

type content_type = {
  media_type: string;
  subtype: string;
  parameters: (string * string) List.t;
}

type content_disposition =
  | Inline of {
      filename: string Option.t;
    }
  | Attachment of {
      filename: string Option.t;
    }

type encoding =
  | SevenBit
  | EightBit
  | Binary
  | QuotedPrintable
  | Base64
  | Other of string

type header =
  | ContentType of content_type
  | ContentDisposition of content_disposition
  | ContentTransferEncoding of encoding
  | ContentId of string
  | ContentDescription of string
  | Other of string * string

type part = {
  headers: header List.t;
  content: string;
}

type t =
  | SinglePart of part
  | MultiPart of { boundary: string; parts: t List.t; }

let find_header = fun name headers ->
    let name_lower = String.lowercase_ascii name in
    List.find_opt
      (fun h ->
        match h with
        | ContentType _ -> name_lower = "content-type"
        | ContentDisposition _ -> name_lower = "content-disposition"
        | ContentTransferEncoding _ -> name_lower = "content-transfer-encoding"
        | ContentId _ -> name_lower = "content-id"
        | ContentDescription _ -> name_lower = "content-description"
        | Other (n, _) -> String.lowercase_ascii n = name_lower)
      headers

let percent_decode = fun s ->
    let len = String.length s in
    let buf = Buffer.create len in
    let rec decode i =
      if i >= len then
        Buffer.contents buf
      else if s.[i] = '%' && i + 2 < len then
        (
          let hex = String.sub s (i + 1) 2 in
          match int_of_string_opt ("0x" ^ hex) with
          | Some code ->
              Buffer.add_char buf (Char.chr code);
              decode (i + 3)
          | None ->
              Buffer.add_char buf s.[i];
              decode (i + 1)
        )
      else (
        Buffer.add_char buf s.[i];
        decode (i + 1)
      )
    in
    decode 0

let parse_rfc2231_value = fun value ->
    match String.split_on_char '\'' value with
    | [charset;_lang;encoded] -> (Some charset, percent_decode encoded)
    | [ encoded ] -> (None, percent_decode encoded)
    | _ -> (None, value)

let parse_encoding_string = fun value ->
    match String.lowercase_ascii (String.trim value) with
    | "7bit" -> SevenBit
    | "8bit" -> EightBit
    | "binary" -> Binary
    | "quoted-printable" -> QuotedPrintable
    | "base64" -> Base64
    | other -> Other other

let rec parse_content_type_string = fun value ->
    let rec parse_params str acc =
      let str = String.trim str in
      let str =
        if String.starts_with ~prefix:";" str then
          String.trim (String.sub str 1 (String.length str - 1))
        else
          str
      in
      match String.index_opt str '=' with
      | None -> List.rev acc
      | Some eq_idx ->
          let key = String.trim (String.sub str 0 eq_idx) |> String.lowercase_ascii in
          let rest = String.sub str (eq_idx + 1) (String.length str - eq_idx - 1) in
          let value_end =
            if String.length rest > 0 && rest.[0] = '"' then
              match String.index_from_opt rest 1 '"' with
              | Some close_idx ->
                  let value = String.sub rest 1 (close_idx - 1) in
                  let remaining =
                    if close_idx + 1 < String.length rest then
                      String.sub rest (close_idx + 1) (String.length rest - close_idx - 1)
                    else
                      ""
                  in
                  (value, remaining)
              | None -> (String.trim rest, "")
            else
              match String.index_opt rest ';' with
              | Some semi_idx ->
                  let value = String.trim (String.sub rest 0 semi_idx) in
                  let remaining = String.trim
                    (String.sub rest (semi_idx + 1) (String.length rest - semi_idx - 1)) in
                  (value, remaining)
              | None -> (String.trim rest, "")
          in
          let value, remaining = value_end in
          parse_params remaining ((key, value) :: acc)
    in
    match String.index_opt value ';' with
    | None -> (String.trim value, [])
    | Some idx ->
        let main_type = String.trim (String.sub value 0 idx) in
        let params_str = String.trim (String.sub value (idx + 1) (String.length value - idx - 1)) in
        let raw_params = parse_params params_str [] in
        let params = combine_rfc2231_params raw_params in
        (main_type, params)

and combine_rfc2231_params = fun raw_params ->
    let continuations = Collections.HashMap.create () in
    let regular = Cell.create [] in
    let add_continuation ~name ~num ~decoded =
      let parts =
        match Collections.HashMap.get continuations name with
        | Some p -> p
        | None ->
            let p = Cell.create [] in
            let _ = Collections.HashMap.insert continuations name p in
            p
      in
      Cell.set parts ((num, decoded) :: Cell.get parts)
    in
    let parse_key key value =
      match String.rindex_opt key '*' with
      | None -> `Regular (key, value)
      | Some idx ->
          let name = String.sub key 0 idx in
          let suffix = String.sub key (idx + 1) (String.length key - idx - 1) in
          if suffix = "" then
            `Encoded (name, value)
          else
            let is_encoded = String.ends_with ~suffix:"*" suffix in
            let num_str =
              if is_encoded then
                String.sub suffix 0 (String.length suffix - 1)
              else
                suffix
            in
            match int_of_string_opt num_str with
            | Some num -> `Continuation (name, num, is_encoded, value)
            | None -> `Regular (key, value)
    in
    List.iter
      (fun ((key, value)) ->
        match parse_key key value with
        | `Regular (name, value) ->
            Cell.set regular ((name, value) :: Cell.get regular)
        | `Encoded (name, value) ->
            let _charset, decoded = parse_rfc2231_value value in
            Cell.set regular ((name, decoded) :: Cell.get regular)
        | `Continuation (name, num, is_encoded, value) ->
            let decoded =
              if is_encoded then
                let _charset, decoded = parse_rfc2231_value value in
                decoded
              else
                value
            in
            add_continuation ~name ~num ~decoded)
      raw_params;
    let combined = Cell.create (Cell.get regular) in
    Collections.HashMap.iter
      (fun name parts_cell ->
        let parts =
          Cell.get parts_cell
          |> List.sort
            (fun ((a, _)) ((b, _)) ->
              Int.compare a b)
        in
        let value = String.concat "" (List.map snd parts) in
        Cell.set combined ((name, value) :: Cell.get combined))
      continuations;
    Cell.get combined

let parse_content_type = fun value ->
    let main_type, params = parse_content_type_string value in
    match String.index_opt main_type '/' with
    | None -> {media_type = main_type; subtype = ""; parameters = params}
    | Some slash ->
        let media = String.sub main_type 0 slash in
        let sub = String.sub main_type (slash + 1) (String.length main_type - slash - 1) in
        {media_type = media; subtype = sub; parameters = params}

let parse_content_disposition = fun value ->
    let value = String.trim value in
    let disp_type, params =
      match String.index_opt value ';' with
      | None -> (value, [])
      | Some idx ->
          let dtype = String.trim (String.sub value 0 idx) in
          let rest = String.sub value (idx + 1) (String.length value - idx - 1) in
          (dtype, snd (parse_content_type_string ("x;" ^ rest)))
    in
    let filename = List.assoc_opt "filename" params in
    match String.lowercase_ascii disp_type with
    | "inline" -> Inline {filename}
    | "attachment" -> Attachment {filename}
    | _ -> Inline {filename = None}

let parse_header = fun ((name, value)) ->
    let name_lower = String.lowercase_ascii name in
    match name_lower with
    | "content-type" -> ContentType (parse_content_type value)
    | "content-disposition" -> ContentDisposition (parse_content_disposition value)
    | "content-transfer-encoding" -> ContentTransferEncoding (parse_encoding_string value)
    | "content-id" -> ContentId value
    | "content-description" -> ContentDescription value
    | _ -> Other (name, value)

let parse_headers = fun raw_headers ->
    List.map parse_header raw_headers

let parse_part_headers_and_body = fun content ->
    let lines = String.split_on_char '\n' content in
    let rec collect_headers acc lines =
      match lines with
      | [] -> (List.rev acc, "")
      | line :: rest -> (
          let trimmed = String.trim line in
          if trimmed = "" then
            (List.rev acc, String.concat "\n" rest)
          else
            match String.index_opt line ':' with
            | None -> collect_headers acc rest
            | Some idx ->
                let name = String.sub line 0 idx in
                let value_start = idx + 1 in
                let value =
                  if value_start < String.length line then
                    String.trim (String.sub line value_start (String.length line - value_start))
                  else
                    ""
                in
                collect_headers ((name, value) :: acc) rest
        )
    in
    collect_headers [] lines

let rec parse = fun ~headers ~body ->
    let typed_headers = parse_headers headers in
    match find_header "content-type" typed_headers with
    | None ->
        Ok (SinglePart {headers = typed_headers; content = body})
    | Some (ContentType ct) ->
        let full_type = ct.media_type ^ "/" ^ ct.subtype in
        if String.starts_with ~prefix:"multipart/" full_type then
          match List.assoc_opt "boundary" ct.parameters with
          | None -> Error "Multipart message missing boundary parameter"
          | Some boundary ->
              let parts = parse_multipart boundary body in
              Ok (MultiPart {boundary; parts})
        else
          Ok (SinglePart {headers = typed_headers; content = body})
    | Some _ ->
        Ok (SinglePart {headers = typed_headers; content = body})

and parse_multipart = fun boundary body ->
    let delimiter = "--" ^ boundary in
    let end_delimiter = "--" ^ boundary ^ "--" in
    let lines = String.split_on_char '\n' body in
    let rec find_parts acc current_part lines =
      match lines with
      | [] ->
          let acc =
            match current_part with
            | Some part -> part :: acc
            | None -> acc
          in
          List.rev acc
      | line :: rest -> (
          let trimmed = String.trim line in
          if trimmed = delimiter then
            let acc =
              match current_part with
              | Some part -> part :: acc
              | None -> acc
            in
            find_parts acc (Some []) rest
          else if trimmed = end_delimiter then
            let acc =
              match current_part with
              | Some part -> part :: acc
              | None -> acc
            in
            List.rev acc
          else
            match current_part with
            | None -> find_parts acc None rest
            | Some part -> find_parts acc (Some (line :: part)) rest
        )
    in
    let part_contents = find_parts [] None lines in
    List.filter_map
      (fun part_lines ->
        if List.length part_lines = 0 then
          None
        else
          let content = String.concat "\n" (List.rev part_lines) in
          let raw_headers, body = parse_part_headers_and_body content in
          match parse ~headers:raw_headers ~body with
          | Ok parsed_part -> Some parsed_part
          | Error _ -> Some (SinglePart {headers = parse_headers raw_headers; content = body}))
      part_contents

let is_attachment = fun part ->
    match find_header "content-disposition" part.headers with
    | Some (ContentDisposition (Attachment _)) -> true
    | _ -> false

let get_filename = fun part ->
    match find_header "content-disposition" part.headers with
    | Some (ContentDisposition (Inline { filename })) -> filename
    | Some (ContentDisposition (Attachment { filename })) -> filename
    | _ -> None

let get_content_type = fun part ->
    match find_header "content-type" part.headers with
    | Some (ContentType ct) -> Some ct
    | _ -> None

let get_encoding = fun part ->
    match find_header "content-transfer-encoding" part.headers with
    | Some (ContentTransferEncoding enc) -> Some enc
    | _ -> None

let rec attachments = fun mime ->
    match mime with
    | SinglePart part ->
        if is_attachment part then
          [ part ]
        else
          []
    | MultiPart { parts; _ } -> List.concat_map attachments parts

let quoted_printable_decode = fun s ->
    let len = String.length s in
    let buf = Buffer.create len in
    let rec decode i =
      if i >= len then
        Ok (Buffer.contents buf)
      else if s.[i] = '=' then
        if i + 1 >= len then
          Ok (Buffer.contents buf)
        else if s.[i + 1] = '\n' then
          decode (i + 2)
        else if s.[i + 1] = '\r' && i + 2 < len && s.[i + 2] = '\n' then
          decode (i + 3)
        else if i + 2 < len then
          (
            let hex = String.sub s (i + 1) 2 in
            match int_of_string_opt ("0x" ^ hex) with
            | Some code ->
                Buffer.add_char buf (Char.chr code);
                decode (i + 3)
            | None ->
                Buffer.add_char buf s.[i];
                decode (i + 1)
          )
        else (
          Buffer.add_char buf s.[i];
          decode (i + 1)
        )
      else (
        Buffer.add_char buf s.[i];
        decode (i + 1)
      )
    in
    decode 0

let base64_decode = fun s ->
    let s = String.trim s in
    let len = String.length s in
    let buf = Buffer.create ((len * 3 / 4) + 3) in
    let char_to_value c =
      match c with
      | 'A' .. 'Z' -> Char.code c - Char.code 'A'
      | 'a' .. 'z' -> Char.code c - Char.code 'a' + 26
      | '0' .. '9' -> Char.code c - Char.code '0' + 52
      | '+' -> 62
      | '/' -> 63
      | '=' -> (-1)
      | '\n'
      | '\r'
      | ' '
      | '\t' -> (-3)
      | _ -> (-2)
    in
    let rec decode_chunk i =
      if i >= len then
        Ok ()
      else
        let v1 = char_to_value (String.get s i) in
        if v1 = (-3) then
          decode_chunk (i + 1)
        else if i + 3 < len then
          let v2 = char_to_value (String.get s (i + 1)) in
          let v3 = char_to_value (String.get s (i + 2)) in
          let v4 = char_to_value (String.get s (i + 3)) in
          if v1 = (-2) || v2 = (-2) || v3 = (-2) || v4 = (-2) then
            Error "Invalid base64 character"
          else
            (
              if v1 >= 0 && v2 >= 0 then
                Buffer.add_char buf (Char.chr ((v1 lsl 2) lor (v2 lsr 4) land 0xff));
              if v2 >= 0 && v3 >= 0 then
                Buffer.add_char buf (Char.chr ((v2 lsl 4) lor (v3 lsr 2) land 0xff));
              if v3 >= 0 && v4 >= 0 then
                Buffer.add_char buf (Char.chr ((v3 lsl 6) lor v4 land 0xff));
              decode_chunk (i + 4)
            )
        else
          Ok ()
    in
    match decode_chunk 0 with
    | Ok () -> Ok (Buffer.contents buf)
    | Error e -> Error e

let get_decoded_content = fun part ->
    match get_encoding part with
    | None
    | Some SevenBit
    | Some EightBit
    | Some Binary -> Ok part.content
    | Some Base64 -> base64_decode part.content
    | Some QuotedPrintable -> quoted_printable_decode part.content
    | Some (Other _) -> Ok part.content
