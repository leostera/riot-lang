type t = {
  method_: Method.t;
  uri: Uri.t;
  version: Version.t;
  headers: Header.t;
  body: Body.t option;
}

let create = fun method_ uri ->
  {
    method_;
    uri;
    version = Version.Http11;
    headers = Header.empty;
    body = None;
  }

let method_ = fun request -> request.method_

let uri = fun request -> request.uri

let version = fun request -> request.version

let headers = fun request -> request.headers

let body = fun request -> request.body

let body_string = fun request -> Option.map ~fn:Body.to_string request.body

let with_method = fun request method_ -> { request with method_ }

let with_uri = fun request uri -> { request with uri }

let with_version = fun request version -> { request with version }

let with_headers = fun request headers -> { request with headers }

let with_body_data = fun request body -> { request with body = Some body }

let with_body = fun request body -> with_body_data request (Body.from_string body)

let with_body_slice = fun request body -> with_body_data request (Body.from_slice body)

let without_body = fun request -> { request with body = None }

let with_header = fun request name value -> {
  request with
  headers = Header.set request.headers name value;
}

let add_header = fun request name value -> {
  request with
  headers = Header.add request.headers name value;
}

let remove_header = fun request name -> {
  request with
  headers = Header.remove request.headers name;
}

let get_header = fun request name -> Header.get request.headers name

let has_header = fun request name -> Header.has request.headers name

module Builder = struct
  type request = t

  type t = {
    method_: Method.t;
    uri: Uri.t;
    version: Version.t;
    headers: Header.t;
    body: Body.t option;
  }

  let create = fun method_ uri ->
    {
      method_;
      uri;
      version = Version.Http11;
      headers = Header.empty;
      body = None;
    }

  let method_ = fun builder method_ -> { builder with method_ }

  let uri = fun builder uri -> { builder with uri }

  let version = fun builder version -> { builder with version }

  let headers = fun builder headers -> { builder with headers }

  let body_data = fun builder body -> { builder with body = Some body }

  let body = fun builder body -> body_data builder (Body.from_string body)

  let body_slice = fun builder body -> body_data builder (Body.from_slice body)

  let header = fun builder name value -> {
    builder with
    headers = Header.set builder.headers name value;
  }

  let build builder: request = {
    method_ = builder.method_;
    uri = builder.uri;
    version = builder.version;
    headers = builder.headers;
    body = builder.body;
  }
end

let get = fun uri -> create Method.Get uri

let head = fun uri -> create Method.Head uri

let delete = fun uri -> create Method.Delete uri

let options = fun uri -> create Method.Options uri

let post = fun uri body ->
  let request = create Method.Post uri in
  with_body request body

let put = fun uri body ->
  let request = create Method.Put uri in
  with_body request body

let patch = fun uri body ->
  let request = create Method.Patch uri in
  with_body request body
