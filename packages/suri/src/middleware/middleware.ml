module Conn = Conn
module Pipeline = Pipeline
module Router = Router
module Logger = Logger
module Request_id = Request_id
module Debugger = Debugger

(** Convenience alias for Router.middleware *)
let router = Router.middleware

(** Convenience alias for Logger.logger *)  
let logger = Logger.logger

(** Convenience alias for Request_id.request_id *)
let request_id = Request_id.request_id

(** Convenience alias for Debugger.debugger *)
let debugger = Debugger.debugger
module Cors = Cors

(** Convenience alias for Cors.middleware *)
let cors = Cors.middleware

module Session = Session

(** Convenience alias for Session.middleware *)
let session = Session.middleware

module Csrf = Csrf

(** Convenience alias for Csrf.middleware *)
let csrf = Csrf.middleware

module Body_parser = Body_parser

(** Convenience alias for Body_parser.make *)
let body_parser = Body_parser.make

module Static = Static

(** Convenience alias for Static.middleware *)
let static = Static.middleware

module Basic_auth = Basic_auth

(** Convenience alias for Basic_auth.middleware *)
let basic_auth = Basic_auth.middleware

(** Convenience alias for Basic_auth.middleware_with_validation *)
let basic_auth_with_validation = Basic_auth.middleware_with_validation

module Accepts = Accepts

(** Convenience alias for Accepts.middleware *)
let accepts = Accepts.middleware

module Head = Head

(** Convenience alias for Head.middleware *)
let head = Head.middleware

module Runtime = Runtime

(** Convenience alias for Runtime.middleware *)
let runtime = Runtime.middleware

module Method_override = Method_override

(** Convenience alias for Method_override.middleware with default param *)
let method_override = fun ~conn ~next -> Method_override.middleware ?param:None ~conn ~next

module Remote_ip = Remote_ip

(* No convenience alias - Remote_ip.middleware requires ~proxies parameter *)

module Etag = Etag

(** Convenience alias for Etag.middleware with default param *)
let etag = fun ~conn ~next -> Etag.middleware ?weak:None ~conn ~next

module Conditional_get = Conditional_get

(** Convenience alias for Conditional_get.middleware *)
let conditional_get = Conditional_get.middleware
