open Std

module Test = Std.Test
module Kernel = Kernel

let ( let* ) value fn = Result.and_then value ~fn

let lift result =
  match result with
  | Kernel.Result.Ok value -> Ok value
  | Kernel.Result.Error error -> Error (Kernel.Error.to_string (Kernel.Error.from_net_addr error))

let all_ports_are = fun addrs expected_port ->
  let rec loop index =
    if index = Kernel.Array.length addrs then
      true
    else if
      Kernel.Net.SocketAddr.port (Kernel.Array.get_unchecked addrs ~at:index) = expected_port
    then
      loop (index + 1)
    else
      false
  in
  loop 0

let test_resolve_stream_fast_path_accepts_ip_literals = fun _ctx ->
  let* addrs = lift (Kernel.Net.Addr.resolve_stream ~host:"127.0.0.1" ~port:8_080) in
  if
    Kernel.Array.length addrs = 1
    && Kernel.Net.SocketAddr.to_string (Kernel.Array.get_unchecked addrs ~at:0) = "127.0.0.1:8080"
  then
    Ok ()
  else
    Error "expected literal IPv4 resolution to stay concrete and single-address"

let test_resolve_datagram_fast_path_accepts_ipv6_literals = fun _ctx ->
  let* addrs = lift (Kernel.Net.Addr.resolve_datagram ~host:"::1" ~port:5_353) in
  let addr = Kernel.Array.get_unchecked addrs ~at:0 in
  if
    Kernel.Array.length addrs = 1
    && Kernel.Net.SocketAddr.port addr = 5_353
    && Kernel.Net.IpAddr.equal (Kernel.Net.SocketAddr.ip addr) Kernel.Net.IpAddr.v6_loopback
  then
    Ok ()
  else
    Error "expected literal IPv6 datagram resolution to stay concrete and single-address"

let test_resolve_stream_resolves_localhost = fun _ctx ->
  let* addrs = lift (Kernel.Net.Addr.resolve_stream ~host:"localhost" ~port:9_001) in
  if Kernel.Array.length addrs > 0 && all_ports_are addrs 9_001 then
    Ok ()
  else
    Error "expected localhost stream resolution to return at least one concrete socket address"

let test_resolve_first_stream_rejects_invalid_port = fun _ctx ->
  match Kernel.Net.Addr.resolve_first_stream ~host:"127.0.0.1" ~port:(-1) with
  | Kernel.Result.Error (Kernel.Net.Addr.InvalidPort { port = (-1) }) -> Ok ()
  | Kernel.Result.Error error -> Error (Kernel.Net.Addr.error_to_string error)
  | Kernel.Result.Ok _ -> Error "expected invalid ports to be rejected before resolution"

let test_resolve_first_stream_reports_missing_hosts = fun _ctx ->
  match Kernel.Net.Addr.resolve_first_stream ~host:"riot-kernel-new.invalid" ~port:443 with
  | Kernel.Result.Error (Kernel.Net.Addr.HostNotFound { host }) ->
      if Kernel.String.equal host "riot-kernel-new.invalid" then
        Ok ()
      else
        Error "expected host-not-found error to preserve the original host"
  | Kernel.Result.Error error -> Error (Kernel.Net.Addr.error_to_string error)
  | Kernel.Result.Ok _ -> Error "expected invalid reserved hostnames to fail resolution"

let tests = [
  Test.case
    "Net.Addr resolves IPv4 literals without name lookup"
    test_resolve_stream_fast_path_accepts_ip_literals;
  Test.case
    "Net.Addr resolves IPv6 datagram literals without name lookup"
    test_resolve_datagram_fast_path_accepts_ipv6_literals;
  Test.case
    "Net.Addr resolves localhost to concrete stream addresses"
    test_resolve_stream_resolves_localhost;
  Test.case
    "Net.Addr rejects invalid ports before resolution"
    test_resolve_first_stream_rejects_invalid_port;
  Test.case
    "Net.Addr reports missing hosts as typed errors"
    test_resolve_first_stream_reports_missing_hosts;
]

let main ~args = Test.Cli.main ~name:"kernel_new_addr_tests" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
