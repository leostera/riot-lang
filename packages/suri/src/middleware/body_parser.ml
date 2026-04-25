open Std

type parser =
  | Urlencoded
  | Json
  | Multipart

type config = { parsers: parser list; max_body_size: int }

let default_config = fun () -> { parsers = [ Urlencoded; Json ]; max_body_size = 10 * 1_024 * 1_024 }

(** Parse application/x-www-form-urlencoded body using Net.Uri.Query.parse *)
let parse_urlencoded = fun body -> Net.Uri.Query.parse body

(** Parse JSON body - convert JSON object to string pairs *)
let parse_json = fun body ->
  match Data.Json.of_string body with
  | Ok (Data.Json.Object fields) -> (* Convert JSON object to string pairs *)
  List.filter_map ~fn:(
    fun ((k, v)) ->
      match v with
      | Data.Json.String s -> Some (k, s)
      | Data.Json.Int i -> Some (k, Int.to_string i)
      | Data.Json.Float f -> Some (k, Float.to_string f)
      | Data.Json.Bool b -> Some (k, Bool.to_string b)
      | Data.Json.Null -> Some (k, "")
      | _ -> None
  ) fields
  | _ -> []

(** Parse multipart/form-data - TODO: use Mime library *)
let parse_multipart = fun ~boundary:_ _body -> (* TODO: Implement proper multipart parsing with Mime library *)
(* For now, return empty list *)
[]

let handle = fun config conn ->
  (* Get Content-Type header *)
  match Net.Http.Header.get (Conn.headers conn) "content-type" with
  | None -> conn
  | Some content_type ->
      let body = Conn.body conn in
      (* Check body size limit *)
      if String.length body > config.max_body_size then
        conn
      else
        (* Parse based on Content-Type *)
        let body_params =
          if String.starts_with ~prefix:"application/x-www-form-urlencoded" content_type && List.contains config.parsers ~value:Urlencoded then
            parse_urlencoded body
          else
            if String.starts_with ~prefix:"application/json" content_type && List.contains config.parsers ~value:Json then
              parse_json body
            else
              if String.starts_with ~prefix:"multipart/form-data" content_type && List.contains config.parsers ~value:Multipart then
                let parts = String.split_on_char ';' content_type in
                let boundary_opt = List.filter_map ~fn:(
                  fun part ->
                    let trimmed = String.trim part in
                    if String.starts_with ~prefix:"boundary=" trimmed then
                      Some (String.sub trimmed ~offset:9 ~len:(String.length trimmed - 9))
                    else None
                ) parts |> List.head in
                (
                  match boundary_opt with
                  | Some boundary -> parse_multipart ~boundary body
                  | None -> []
                )
              else []
        in
        Conn.set_body_params body_params conn

let make = fun ?(config = default_config ()) () ->
  fun ~conn ~next ->
    let conn = handle config conn in next conn
