(**
   Request ID middleware

   Ensures that every request has an [x-request-id] header.

   - If the client sends a valid [x-request-id] header, it will be preserved
   - If no [x-request-id] is present, or the client value is invalid, a new
     UUID v7 will be generated
   - The [x-request-id] is added to both the request (for downstream handlers)
     and the response (for the client)

   This middleware is useful for request tracing and debugging.

   Example usage:
   {[
     let app = Middleware.[
       request_id;  (* Generate/preserve request IDs *)
       logger;      (* Logger can now log the request ID if desired *)
       router routes;
     ]
   ]}
*)
val request_id: Pipeline.middleware

(**
   Middleware that ensures an [x-request-id] header is present in both
   the request and response.
*)
module For_testing: sig
  val max_request_id_length: int

  val is_valid_request_id: string -> bool

  val choose_request_id: ?generate:(unit -> string) -> string option -> string
end
