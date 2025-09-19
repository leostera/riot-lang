type t = {
  status : Status.t;
  version : Version.t;
  headers : Header.t;
  body : string option;
}

let create status =
  {
    status;
    version = Version.Http11;
    headers = Header.empty;
    body = None;
  }

let status response = response.status
let version response = response.version
let headers response = response.headers
let body response = response.body
let with_status response status = { response with status }
let with_version response version = { response with version }
let with_headers response headers = { response with headers }
let with_body response body = { response with body = Some body }
let without_body response = { response with body = None }

let with_header response name value =
  { response with headers = Header.set response.headers name value }

let add_header response name value =
  { response with headers = Header.add response.headers name value }

let remove_header response name =
  { response with headers = Header.remove response.headers name }

let get_header response name = Header.get response.headers name
let has_header response name = Header.has response.headers name

module Builder = struct
  type response = t

  type t = {
    status : Status.t;
    version : Version.t;
    headers : Header.t;
    body : string option;
  }

  let create status =
    {
      status;
      version = Version.Http11;
      headers = Header.empty;
      body = None;
    }

  let status builder status = { builder with status }
  let version builder version = { builder with version }
  let headers builder headers = { builder with headers }
  let body builder body = { builder with body = Some body }

  let header builder name value =
    { builder with headers = Header.set builder.headers name value }

  let build builder : response =
    {
      status = builder.status;
      version = builder.version;
      headers = builder.headers;
      body = builder.body;
    }
end

let ok body =
  let response = create Status.Ok in
  with_body response body

let created body =
  let response = create Status.Created in
  with_body response body

let accepted body =
  let response = create Status.Accepted in
  with_body response body

let no_content () = create Status.NoContent

let bad_request body =
  let response = create Status.BadRequest in
  with_body response body

let unauthorized body =
  let response = create Status.Unauthorized in
  with_body response body

let forbidden body =
  let response = create Status.Forbidden in
  with_body response body

let not_found body =
  let response = create Status.NotFound in
  with_body response body

let method_not_allowed body =
  let response = create Status.MethodNotAllowed in
  with_body response body

let internal_server_error body =
  let response = create Status.InternalServerError in
  with_body response body

let not_implemented body =
  let response = create Status.NotImplemented in
  with_body response body

let bad_gateway body =
  let response = create Status.BadGateway in
  with_body response body

let service_unavailable body =
  let response = create Status.ServiceUnavailable in
  with_body response body
