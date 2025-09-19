type t = {
  method_ : Method.t;
  uri : Uri.t;
  version : Version.t;
  headers : Header.t;
  body : string option;
}

let create method_ uri =
  {
    method_;
    uri;
    version = Version.Http11;
    headers = Header.empty;
    body = None;
  }

let method_ request = request.method_
let uri request = request.uri
let version request = request.version
let headers request = request.headers
let body request = request.body
let with_method request method_ = { request with method_ }
let with_uri request uri = { request with uri }
let with_version request version = { request with version }
let with_headers request headers = { request with headers }
let with_body request body = { request with body = Some body }
let without_body request = { request with body = None }

let with_header request name value =
  { request with headers = Header.set request.headers name value }

let add_header request name value =
  { request with headers = Header.add request.headers name value }

let remove_header request name =
  { request with headers = Header.remove request.headers name }

let get_header request name = Header.get request.headers name
let has_header request name = Header.has request.headers name

module Builder = struct
  type request = t

  type t = {
    method_ : Method.t;
    uri : Uri.t;
    version : Version.t;
    headers : Header.t;
    body : string option;
  }

  let create method_ uri =
    {
      method_;
      uri;
      version = Version.Http11;
      headers = Header.empty;
      body = None;
    }

  let method_ builder method_ = { builder with method_ }
  let uri builder uri = { builder with uri }
  let version builder version = { builder with version }
  let headers builder headers = { builder with headers }
  let body builder body = { builder with body = Some body }

  let header builder name value =
    { builder with headers = Header.set builder.headers name value }

  let build builder : request =
    {
      method_ = builder.method_;
      uri = builder.uri;
      version = builder.version;
      headers = builder.headers;
      body = builder.body;
    }
end

let get uri = create Method.Get uri
let head uri = create Method.Head uri
let delete uri = create Method.Delete uri
let options uri = create Method.Options uri

let post uri body =
  let request = create Method.Post uri in
  with_body request body

let put uri body =
  let request = create Method.Put uri in
  with_body request body

let patch uri body =
  let request = create Method.Patch uri in
  with_body request body
