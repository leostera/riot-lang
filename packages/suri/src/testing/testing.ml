module Request = Testing_request
module Conn = Testing_conn
module Middleware = Testing_middleware
module App = Testing_app
module Expect = Expect
module Internal = Testing_internal

type outcome = App.outcome =
  | Responded of Web_server.Response.t
  | Upgraded

type response_error = App.response_error =
  | ExpectedResponseButUpgraded

let response_error_to_string = App.response_error_to_string
