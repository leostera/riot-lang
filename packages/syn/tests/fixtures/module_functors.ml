(* Basic functor *)

module F (X : S) = struct
  type t = X.t
end

(* Multiple parameters *)

module G (X : S) (Y : T) = struct
  type t = X.t * Y.t
end

(* Functor application *)

module M = F (X)

(* Multiple functor applications *)

module N = F (X) (Y) (Z)

(* Nested application *)

module O = F (G (H (X)))

(* Functor with explicit return type *)

module P (X : S): T = struct
  type t = int
end

(* Include with functor application *)

include M (X)

(* Include with simple module *)

include M
