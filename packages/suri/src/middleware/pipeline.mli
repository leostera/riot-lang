(**
   # Middleware Pipeline

   Execute a sequence of middleware functions on a connection.

   ## Example

   ```ocaml
   let timer ~conn ~next =
     let start = Time.Instant.now () in
     let conn' = next conn in
     Log.debug (Printf.sprintf "Took %.2fms" 
       (Time.Instant.elapsed start |> Time.Duration.to_millis));
     conn'

   let app = Middleware.[
     logger ();
     timer;
     router routes;
   ]

   let handler socket_conn req = 
     let conn = Conn.make socket_conn req in
     let conn = Pipeline.run conn app in
     Conn.to_response conn
   ``` 
*)
(**
   A middleware function that can wrap the next handler.

   Middleware receives the connection and a [next] function to call
   the rest of the pipeline. This allows middleware to:
   - Execute code before the handler (just call [next conn])
   - Execute code after the handler ([let conn' = next conn in ...])
   - Skip the handler entirely (return without calling [next])
   - Modify the connection before/after

   Example:
   {[
     let logger ~conn ~next =
       Log.info "Before handler";
       let conn' = next conn in
       Log.info "After handler";
       conn'
   ]} 
*)
(** A pipeline is a list of middleware *)
type middleware = conn:Conn.t -> next:(Conn.t -> Conn.t) -> Conn.t

(** Run a pipeline on a connection, stopping if halted *)
type t = middleware list

val run: Conn.t -> t -> Conn.t
