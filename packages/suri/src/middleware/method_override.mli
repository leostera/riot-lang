open Std

(**
   {1 Method Override Middleware}

   Allows HTML forms (which only support GET/POST) to emulate PUT/PATCH/DELETE
   via a [_method] parameter.

   {2 Quick Start}

   {[
     let app = Middleware.[
       logger;
       body_parser ();     (* MUST come before method_override! *)
       method_override;    (* Override POST to PUT/PATCH/DELETE *)
       csrf ();
       router routes;
     ]
   ]}

   {2 Why Use This?}

   HTML forms only support GET and POST methods:
   {v
   <form method="GET" action="/search">  <!-- OK -->
   <form method="POST" action="/users">  <!-- OK -->
   <form method="DELETE" action="/users/1">  <!-- NOT SUPPORTED -->
   v}

   This middleware lets you build RESTful applications with proper HTTP verbs:
   {v
   <form method="POST" action="/users/1">
     <input type="hidden" name="_method" value="DELETE">
     <button>Delete User</button>
   </form>
   v}

   {2 How It Works}

   1. Client sends POST request with [_method=DELETE] in body
   2. Body parser extracts [_method] parameter
   3. Method override changes POST → DELETE
   4. Router sees DELETE request

   {2 Security}

   {b Safe}: Only allows specific methods (PUT, PATCH, DELETE)

   - ✅ POST + [_method=PUT] → PUT
   - ✅ POST + [_method=PATCH] → PATCH
   - ✅ POST + [_method=DELETE] → DELETE
   - ❌ POST + [_method=GET] → POST (ignored)
   - ❌ GET + [_method=DELETE] → GET (ignored)

   {2 Order Matters}

   {b CRITICAL}: Place {b after} body_parser but {b before} router:

   {[
     (* CORRECT *)
     let app = Middleware.[
       body_parser ();     (* 1. Parse body to get _method *)
       method_override;    (* 2. Override method *)
       router routes;      (* 3. Route with new method *)
     ]

     (* WRONG - won't work! *)
     let app = Middleware.[
       method_override;    (* No body params yet! *)
       body_parser ();
       router routes;
     ]
   ]}

   {2 HTML Form Example}

   {v
   <!-- Delete a user -->
   <form method="POST" action="/users/123">
     <input type="hidden" name="_method" value="DELETE">
     <button type="submit">Delete User</button>
   </form>

   <!-- Update a user -->
   <form method="POST" action="/users/123">
     <input type="hidden" name="_method" value="PUT">
     <input name="name" value="Alice">
     <button type="submit">Update User</button>
   </form>
   v}

   {2 Custom Parameter Name}

   Change the parameter name if needed:

   {[
     let app = Middleware.[
       body_parser ();
       method_override ~param:"_http_method";
       router routes;
     ]
   ]}

   Then use in forms:
   {v
   <input type="hidden" name="_http_method" value="DELETE">
   v}
*)

(** {1 Middleware} *)

(**
   Method override middleware.

   Checks POST requests for a [_method] parameter and overrides the
   method to PUT, PATCH, or DELETE.

   {[
     let app = Middleware.[
       body_parser ();
       method_override;
       router routes;
     ]
   ]}

   {b Parameters}:
   - [param] - Parameter name to check (default: ["_method"])

   {b Requirements}:
   - Request method must be POST
   - Body must be parsed (use body_parser middleware first!)
   - Parameter value must be "PUT", "PATCH", or "DELETE" (case-insensitive)

   {b Example usage}:
   {[
     (* HTML form *)
     <form method="POST" action="/users/123">
       <input type="hidden" name="_method" value="DELETE">
       <button>Delete</button>
     </form>

     (* Route handler *)
     Router.delete "/users/:id" (fun ~conn ~next:_ ->
       let id = List.assoc "id" (Conn.params conn) in
       (* Delete user with id *)
       Conn.respond conn ~status:Ok ~body:"Deleted" |> Conn.send
     )
   ]}
*)
val middleware: ?param:string -> unit -> conn:Conn.t -> next:(Conn.t -> Conn.t) -> Conn.t
