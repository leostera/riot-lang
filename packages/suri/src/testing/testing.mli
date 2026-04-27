(**
   Test helpers for exercising real Suri applications and middleware.

   This module builds normal request/connection values and runs the real Suri
   middleware pipeline. It does not provide replacement protocol parsers,
   replacement components, or fake application modules.
*)
module Request = Suri__Testing__Request

module Conn = Suri__Testing__Conn

module Middleware = Suri__Testing__Middleware

module App = Suri__Testing__App

module Expect = Suri__Testing__Expect

type outcome = App.outcome =
  | Responded of Web_server.Response.t
  | Upgraded
type response_error = App.response_error =
  | ExpectedResponseButUpgraded
val response_error_to_string: response_error -> string
