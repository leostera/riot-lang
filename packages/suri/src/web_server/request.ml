open Std

type t = {
  http_request: Net.Http.Request.t;
  body: string;
  remaining: int;
}

let from_http = fun ~body http_request ->
  let remaining =
    match Net.Http.Request.get_header http_request "content-length" with
    | Some len_str -> (
        match Int.of_string_opt len_str with
        | Some len -> len - String.length body
        | None -> 0
      )
    | None -> 0
  in
  { http_request; body; remaining }

let method_ = fun t -> Net.Http.Request.method_ t.http_request

let uri = fun t ->
  Net.Http.Request.uri t.http_request
  |> Net.Uri.to_string

let version = fun t -> Net.Http.Request.version t.http_request

let headers = fun t -> Net.Http.Request.headers t.http_request

let with_header = fun name value t -> {
  t with
  http_request = Net.Http.Request.with_header t.http_request name value;
}

let body = fun t -> t.body

let remaining = fun t -> t.remaining
