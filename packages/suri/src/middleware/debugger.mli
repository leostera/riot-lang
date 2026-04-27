(**
   Visual debugger middleware.

   Temporarily disabled while Suri removes its dependency on Riot's build model.
   The middleware currently passes the connection to the next handler unchanged.
*)
val debugger: conn:Conn.t -> next:(Conn.t -> Conn.t) -> Conn.t
