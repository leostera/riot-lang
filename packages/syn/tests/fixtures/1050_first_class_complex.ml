let apply (type a) ((module M : Monad with type t = a)) x = M.return x
