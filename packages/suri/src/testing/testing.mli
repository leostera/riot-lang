(**
   Test helpers for exercising real Suri applications and middleware.

   This module builds normal request/connection values and runs the real Suri
   middleware pipeline. It does not provide replacement protocol parsers,
   replacement components, or fake application modules.
*)
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
  | InvalidRequest of Request.error
  | ExpectedResponseButUpgraded
val response_error_to_string: response_error -> string
