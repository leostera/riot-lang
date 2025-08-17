include Common
module Client = Client
module Server = Server

(** Helper functions for creating responses *)

let result ~result ~id =
  { jsonrpc = "2.0"; result = Some result; error = None; id }

let error_response ~error ~id =
  { jsonrpc = "2.0"; result = None; error = Some error; id }
