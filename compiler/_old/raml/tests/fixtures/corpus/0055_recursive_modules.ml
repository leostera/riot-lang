(* Recursive modules. *)
module rec Even : sig
  val check : int -> bool
end = struct
  let check n = n = 0 || Odd.check (n - 1)
end

and Odd : sig
  val check : int -> bool
end = struct
  let check n = n <> 0 && Even.check (n - 1)
end

let () = Printf.printf "%b %b\n" (Even.check 10) (Odd.check 11)
