(* From Iterator.ml - simplified *)

let map (type a b) (iter: a t) ~fn : b t = make iter

(* From jsonrpc client *)

let call (type req res) (client: (req, res) t) ~method_ ?params () : res =
  let (Client c) = client in
  c

(* From MutIterator.ml *)

let filter (type a) (iter: a t) ~fn : a t = iter
