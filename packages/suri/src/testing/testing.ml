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

let response_error_to_string = App.response_error_to_string
