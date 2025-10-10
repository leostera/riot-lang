(* Test that trivia is preserved in module declarations *)

(* Simple module *)
module M = struct
  let x = 1
end

(* Module with signature constraint *)
module N (* name *) : (* constraint *) sig
  val x : int
end = struct
  let x = 1
end

(* Functor with trivia *)
module F (* functor *) (
  (* parameter *)
  X (* param name *) : (* param sig *) sig
    val x : int
  end
) = struct
  let y = X.x
end

(* Module type of *)
module type T = module type of (* the module *) List

(* First class module *)
let m = (module M (* the module *) : (* signature *) sig val x : int end)
