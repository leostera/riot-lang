module type S = sig
  type t
  type 'a result = (t, 'a) Result.t
end
