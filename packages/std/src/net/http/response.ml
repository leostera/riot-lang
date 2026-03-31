type t = {
  status : Status.t;
  version : Version.t;
  headers : Header.t;
  body : string option;
}

let create = fun status -> {status; version = Version.Http11; headers = Header.empty; body = None}

let status = fun response -> response.status

let version = fun response -> response.version

let headers = fun response -> response.headers

let body = fun response -> response.body

let with_status = fun response status -> {response with status}

let with_version = fun response version -> {response with version}

let with_headers = fun response headers -> {response with headers}

let with_body = fun response body -> {response with body = Some body}

let without_body = fun response -> {response with body = None}

let with_header = fun response name value ->
  {response with headers = Header.set response.headers name value}

let add_header = fun response name value ->
  {response with headers = Header.add response.headers name value}

let remove_header = fun response name ->
  {response with headers = Header.remove response.headers name}

let get_header = fun response name ->
  Header.get response.headers name

let has_header = fun response name ->
  Header.has response.headers name

module Builder = struct
  type response = t

  type t = {
    status : Status.t;
    version : Version.t;
    headers : Header.t;
    body : string option;
  }

  let create = fun status -> {status; version = Version.Http11; headers = Header.empty; body = None}

  let status = fun builder status -> {builder with status}

  let version = fun builder version -> {builder with version}

  let headers = fun builder headers -> {builder with headers}

  let body = fun builder body -> {builder with body = Some body}

  let header = fun builder name value ->
    {builder with headers = Header.set builder.headers name value}

  let build builder : response = {
    status = builder.status;
    version = builder.version;
    headers = builder.headers;
    body = builder.body;

  }
end

let ok = fun body ->
  let response = create Status.Ok in
  with_body response body

let created = fun body ->
  let response = create Status.Created in
  with_body response body

let accepted = fun body ->
  let response = create Status.Accepted in
  with_body response body

let no_content = fun () -> create Status.NoContent

let bad_request = fun body ->
  let response = create Status.BadRequest in
  with_body response body

let unauthorized = fun body ->
  let response = create Status.Unauthorized in
  with_body response body

let forbidden = fun body ->
  let response = create Status.Forbidden in
  with_body response body

let not_found = fun body ->
  let response = create Status.NotFound in
  with_body response body

let method_not_allowed = fun body ->
  let response = create Status.MethodNotAllowed in
  with_body response body

let internal_server_error = fun body ->
  let response = create Status.InternalServerError in
  with_body response body

let not_implemented = fun body ->
  let response = create Status.NotImplemented in
  with_body response body

let bad_gateway = fun body ->
  let response = create Status.BadGateway in
  with_body response body

let service_unavailable = fun body ->
  let response = create Status.ServiceUnavailable in
  with_body response body
