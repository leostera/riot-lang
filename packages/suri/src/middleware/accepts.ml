open Std

(** {1 Type Matching} *)

(** Check if content type matches pattern.
    
    Supports:
    - Exact match: "application/json" = "application/json"
    - Type wildcard: "text/*" matches "text/plain", "text/html", etc.
    - Full wildcard: "*/*" matches anything *)
let matches_pattern = fun ~pattern ~content_type ->
  match (pattern, content_type) with
  | "*/*", _ -> true
  | pat, ct -> (
      (* Split type/subtype *)
      match (String.split_on_char '/' pat, String.split_on_char '/' ct) with
      | [type1;"*"], [type2;_] -> String.equal type1 type2
      | [type1;sub1], [type2;sub2] -> String.equal type1 type2 && String.equal sub1 sub2
      | _ -> false
    )

(** {1 Accept Header Parsing} *)

type accept_entry = {
  media_type : string;
  quality : float;
}

(** Parse quality value from parameter string.
    
    Example: "q=0.8" -> Some 0.8 *)
let parse_quality = fun param ->
  match String.split_on_char '=' (String.trim param) with
  | ["q";value] -> Float.of_string_opt (String.trim value)
  | _ -> None

(** Parse single Accept header entry with quality value.
    
    Examples:
    - "application/json" -> { media_type = "application/json"; quality = 1.0 }
    - "text/html;q=0.9" -> { media_type = "text/html"; quality = 0.9 } *)
let parse_accept_entry = fun entry ->
  match String.split_on_char ';' entry with
  | [] -> {media_type = "*/*"; quality = 1.0}
  | media_type :: params ->
      let quality = List.find_map parse_quality params |> Option.unwrap_or ~default:1.0 in
      {media_type = String.trim media_type; quality}

(** Parse full Accept header.
    
    Returns list sorted by quality (highest first). *)
let parse_accept =
  fun header ->
    String.split_on_char ',' header
    |> List.map String.trim
    |> List.filter (fun s -> String.length s > 0)
    |> List.map parse_accept_entry
    |> List.sort
      (fun a b ->
        Float.compare b.quality a.quality)

(** {1 Content-Type Parsing} *)

(** Extract base content type, stripping parameters.
    
    Examples:
    - "application/json" -> Some "application/json"
    - "application/json; charset=utf-8" -> Some "application/json"
    - "multipart/form-data; boundary=..." -> Some "multipart/form-data" *)
let get_base_content_type = fun ct ->
  match String.split_on_char ';' ct with
  | [] -> None
  | base :: _ ->
      let trimmed = String.trim base in
      if String.length trimmed = 0 then
        None
      else
        Some trimmed

(** {1 Configuration} *)

type config = {
  types : string list;
  check_accept : bool;
  check_content_type : bool;
  on_reject : (Conn.t -> string option -> Conn.t) option;
}

let default_config = {
  types = [ "*/*" ];
  check_accept = true;
  check_content_type = true;
  on_reject = None;

}

(** {1 HTTP Responses} *)

(** Send 406 Not Acceptable response *)
let reject_not_acceptable = fun conn config received ->
  match config.on_reject with
  | Some handler -> handler conn received
  | None -> Conn.respond conn ~status:NotAcceptable ~body:"Not Acceptable" |> Conn.halt

(** Send 415 Unsupported Media Type response *)
let reject_unsupported_media_type = fun conn config received ->
  match config.on_reject with
  | Some handler -> handler conn received
  | None -> Conn.respond conn ~status:UnsupportedMediaType ~body:"Unsupported Media Type" |> Conn.halt

(** {1 Validation Logic} *)

(** Check if Accept header matches any accepted types *)
let check_accept_header = fun conn config ->
  let headers = Conn.headers conn in
  match Net.Http.Header.get headers "accept" with
  | None -> (true, None)
  | Some accept ->
      let entries = parse_accept accept in
      let matches =
        List.exists
          (fun entry ->
            List.exists
            (fun pattern -> matches_pattern ~pattern ~content_type:entry.media_type)
            config.types)
          entries
      in
      (matches, Some accept)

(** Check if Content-Type header matches any accepted types *)
let check_content_type_header = fun conn config ->
  let headers = Conn.headers conn in
  match Net.Http.Header.get headers "content-type" with
  | None -> (false, None)
  | Some ct -> (
      match get_base_content_type ct with
      | Some base ->
          let matches =
            List.exists (fun pattern -> matches_pattern ~pattern ~content_type:base) config.types
          in
          (matches, Some ct)
      | None -> (false, Some ct)
    )

(** Check if request method has a body *)
let has_request_body = fun method_ ->
  match method_ with
  | Net.Http.Method.Post
  | Put
  | Patch -> true
  | _ -> false

(** {1 Middleware} *)

let make = fun config ->
  fun ~conn ~next ->
    let method_ = Conn.method_ conn in
    let has_body = has_request_body method_ in
    (* Check Accept header *)
    let accept_ok, accept_value =
      if config.check_accept then
        check_accept_header conn config
      else
        (true, None)
    in
    (* Check Content-Type (only for requests with body) *)
    let content_type_ok, content_type_value =
      if config.check_content_type && has_body then
        check_content_type_header conn config
      else
        (true, None)
    in
    (* Both must pass *)
    if accept_ok && content_type_ok then
      next conn
    else if not accept_ok then
      reject_not_acceptable conn config accept_value
    else
      reject_unsupported_media_type conn config content_type_value

let middleware = fun ?config:(cfg = default_config) types ->
  (* If types list is provided, override config.types *)
  let cfg' =
    if List.is_empty types then
      cfg
    else
      {cfg with types}
  in
  make cfg'
