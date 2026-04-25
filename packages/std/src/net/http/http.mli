(**
   HTTP types and utilities.

   ## Example

   ```ocaml
   let response =
     Http.Response.ok "hello"
     |> Http.Response.with_header "content-type" "text/plain"
   ```
*)
module Method = Method

module Header = Header

module Version = Version

module Status = Status

module Body = Body

module Request = Request

module Response = Response
