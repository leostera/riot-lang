open Std

type t = {
  http_request: Net.Http.Request.t;
  body: string;
  remaining: int;
}

let of_http = fun ~body http_request ->
  let remaining =
    match Net.Http.Request.get_header http_request "content-length" with
    | Some len_str -> (
        match int_of_string_opt len_str with
        | Some len -> len - String.length body
        | None -> 0
      )
    | None -> 0
  in
  {http_request; body; remaining}

let method_ = fun t -> Net.Http.Request.method_ t.http_request

let uri = fun t -> Net.Http.Request.uri t.http_request |> Net.Uri.to_string

let version = fun t -> Net.Http.Request.version t.http_request

let headers = fun t -> Net.Http.Request.headers t.http_request

let body = fun t -> t.body

let remaining = fun t -> t.remaining
