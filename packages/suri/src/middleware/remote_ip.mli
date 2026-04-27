open Std

(**
   {1 Remote IP Middleware}

   Extracts the real client IP address from X-Forwarded-For headers when
   behind proxies or load balancers.

   {b ⚠️ SECURITY CRITICAL}: Only use with trusted proxy IPs!

   {2 Quick Start}

   {[
     (* Trust specific proxy IPs *)
     let app = Middleware.[
       remote_ip ~proxies:["10.0.1.50"; "10.0.1.51"];
       logger;  (* Now logs real client IP *)
       router routes;
     ]
   ]}

   {2 Why Use This?}

   When your application runs behind a proxy/load balancer:
   - nginx
   - HAProxy
   - CloudFlare
   - AWS ALB/ELB
   - Heroku

   The IP you see is the proxy's IP, not the client's!

   {v
   Client (1.2.3.4) → Proxy (10.0.1.50) → Your App
                                           ↑
                                     Sees 10.0.1.50
   v}

   This middleware extracts the real client IP from headers:
   {v
   X-Forwarded-For: 1.2.3.4, 10.0.1.50
                    ^^^^^^^^
                    Real client IP!
   v}

   {2 Security}

   {b ⚠️ NEVER trust X-Forwarded-For without validation!}

   Clients can spoof this header:
   {v
   X-Forwarded-For: 127.0.0.1
   v}

   This middleware only trusts forwarded headers when the immediate socket peer
   is one of your known proxies.

   {[
     (* SAFE - only trust your known proxies *)
     remote_ip ~proxies:["10.0.1.50"; "10.0.1.51"];

     (* DANGEROUS - trusts any IP! *)
     (* DON'T DO THIS! *)
   ]}

   {2 How It Works}

   Given header: [X-Forwarded-For: client, proxy1, proxy2]

   1. Check that the immediate socket peer is trusted
   2. Walk the forwarded chain from right to left
   3. Skip trusted proxy IPs
   4. First untrusted IP = real client
   5. Update [Conn.peer] with real IP

   {2 Multiple Proxies}

   {v
   Client (1.2.3.4) → CloudFlare (5.6.7.8) → Your Proxy (10.0.1.50) → App

   X-Forwarded-For: 1.2.3.4, 5.6.7.8, 10.0.1.50

   If you trust: [10.0.1.50]
   Result: 5.6.7.8 (CloudFlare's IP)

   If you trust: [10.0.1.50; 5.6.7.8]
   Result: 1.2.3.4 (Real client!)
   v}

   {2 Custom Headers}

   Different proxies use different headers:

   {[
     (* nginx: X-Real-IP *)
     remote_ip ~proxies:["10.0.1.50"] ~header:"x-real-ip";

     (* CloudFlare: CF-Connecting-IP *)
     remote_ip ~proxies:["173.245.48.1"] ~header:"cf-connecting-ip";

     (* Standard: X-Forwarded-For (default) *)
     remote_ip ~proxies:["10.0.1.50"];
   ]}

   {2 Common Proxy IP Ranges}

   {v
   Private networks (RFC 1918):
   - 10.0.0.0/8      (10.0.0.0 - 10.255.255.255)
   - 172.16.0.0/12   (172.16.0.0 - 172.31.255.255)
   - 192.168.0.0/16  (192.168.0.0 - 192.168.255.255)

   CloudFlare:
   - 173.245.48.0/20
   - 103.21.244.0/22
   - ... (see CloudFlare docs)

   AWS:
   - Varies by region (check AWS IP ranges)
   v}

   {b Note}: Current implementation uses exact IP matching.
   CIDR range support coming in future version.
*)

(** {1 Middleware} *)

(**
   Remote IP middleware - extracts real client IP from proxy headers.

   {[
     let app = Middleware.[
       remote_ip ~proxies:["10.0.1.50"; "10.0.1.51"];
       logger;
       router routes;
     ]
   ]}

   {b Parameters}:
   - [proxies] - List of trusted proxy IP addresses (exact match)
   - [header] - Header to check (default: "x-forwarded-for")

   {b Security}:
   - Only specify IPs you control and trust
   - {b Never} include public IPs unless you own them
   - Empty proxy list = no IP rewriting (safe default)

   {b Headers supported}:
   - [x-forwarded-for] (default) - Standard proxy header
   - [x-real-ip] - nginx single-IP header
   - [cf-connecting-ip] - CloudFlare
   - Custom headers via [~header] parameter

   {b Example with nginx}:
   {[
     (* nginx config:
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
     *)

     let app = Middleware.[
       remote_ip ~proxies:["10.0.1.50"] ~header:"x-real-ip";
       router routes;
     ]
   ]}
*)
val middleware:
  ?header:string ->
  unit ->
  proxies:string list ->
  conn:Conn.t ->
  next:(Conn.t -> Conn.t) ->
  Conn.t

val is_trusted_proxy: string list -> string -> bool

type resolve_error =
  | UntrustedPeer of { peer_ip: string }
  | EmptyForwardedFor
  | InvalidForwardedIp of { value: string }
  | NoClientIpInForwardedChain
val resolve_error_to_string: resolve_error -> string

val parse_forwarded_for: string -> string list

val is_valid_ip_literal: string -> bool

val find_real_ip_result: string list -> string list -> (string, resolve_error) Std.result

val find_real_ip: string list -> string list -> string option

val resolve_real_ip_result:
  proxies:string list ->
  peer_ip:string ->
  header_value:string ->
  (string, resolve_error) Std.result

val resolve_real_ip: proxies:string list -> peer_ip:string -> header_value:string -> string option
