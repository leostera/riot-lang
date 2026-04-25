(**
   Conditional GET Middleware

   This middleware implements HTTP conditional GET support by checking
   If-None-Match and If-Modified-Since headers against the response.
   If the content hasn't changed, it returns a 304 Not Modified response
   with an empty body.

   This is typically used after the ETag middleware to enable client-side caching.

   {2 Example Usage}

   {[
     (* Basic usage - automatically handles ETags and Last-Modified *)
     let app =
       Suri.router []
       |> Suri.middleware (Suri.Middleware.Conditional_get.middleware)
       |> Suri.middleware (Suri.Middleware.Etag.middleware)

     (* The middleware will:
        1. Check If-None-Match header against response ETag
        2. Check If-Modified-Since header against Last-Modified
        3. Return 304 if either matches (content hasn't changed)
        4. Otherwise pass through the full response *)
   ]}

   {2 HTTP Conditional Requests}

   The middleware implements RFC 7232 conditional request handling:

   - {b If-None-Match}: Checks against the response's ETag header
   - {b If-Modified-Since}: Checks against the response's Last-Modified header

   When a match is found (content unchanged), it returns:
   - Status: 304 Not Modified
   - Body: Empty
   - Headers: Preserves cache-related headers (ETag, Last-Modified, Cache-Control, etc.)

   {2 Behavior}

   {3 Matching Logic}
   - If-None-Match takes precedence over If-Modified-Since
   - Returns 304 if either condition matches
   - Only applies to GET and HEAD requests
   - Original response is returned if no conditional headers present

   {3 Header Preservation}
   On 304 responses, these headers are preserved:
   - Cache-Control
   - Content-Location  
   - Date
   - ETag
   - Expires
   - Vary
   - Last-Modified

   {2 Order Matters}

   This middleware should be placed {b after} middleware that sets ETags or Last-Modified:

   {[
     let app =
       Suri.router []
       |> Suri.middleware Conditional_get.middleware  (* Check conditions *)
       |> Suri.middleware Etag.middleware            (* Generate ETags *)
   ]}

   {2 Benefits}

   - Reduces bandwidth by not sending unchanged content
   - Improves performance for clients with cached content
   - Standard HTTP caching behavior
   - Works with both ETag and Last-Modified headers
*)
val middleware: conn:Conn.t -> next:(Conn.t -> Conn.t) -> Conn.t(**
   [middleware ~conn ~next] checks conditional request headers and returns
   304 Not Modified if the content hasn't changed.

   The middleware:
   1. Processes the request through the next handler
   2. Checks If-None-Match against response ETag
   3. Checks If-Modified-Since against Last-Modified
   4. Returns 304 with empty body if content is unchanged
   5. Otherwise returns the full response

   Only applies to GET and HEAD requests. Other methods pass through unchanged. 
*)
