(**
   Request ID middleware

   Ensures that every request has an [x-request-id] header.

   - If the client sends an [x-request-id] header, it will be preserved
   - If no [x-request-id] is present, a new UUID v7 will be generated
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
