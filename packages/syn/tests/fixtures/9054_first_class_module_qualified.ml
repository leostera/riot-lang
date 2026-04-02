(* First-class modules with qualified paths *)

let x =
  f ~transport:(module Std.Net.TcpClient)

(* Multiple levels *)

let y =
  g (module A.B.C.D)

(* In function calls *)

let result =
  Client.create ~transport:(module Std.Net.TcpClient) ()
