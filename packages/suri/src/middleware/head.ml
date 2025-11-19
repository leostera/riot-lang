open Std

(** HEAD request handler middleware
    
    Simply strips the response body for HEAD requests.
    Routes can still match HEAD explicitly, or they'll 404 like any unmatched method.
    
    The key insight: HEAD responses MUST NOT have a body per HTTP spec,
    regardless of how the route is defined. *)
let middleware ~conn ~next =
  let original_method = Conn.method_ conn in
  let conn' = next conn in
  
  (* If original request was HEAD, strip the response body *)
  match original_method with
  | Net.Http.Method.Head -> Conn.with_body "" conn'
  | _ -> conn'
