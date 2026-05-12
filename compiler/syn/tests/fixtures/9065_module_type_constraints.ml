type t = (module S with type t = int and type u = string)

let x =
  (module M : S with type config = Config.t)
