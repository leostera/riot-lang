(* Functors over small module dictionaries. *)
module type SHOW = sig
  type t
  val show : t -> string
end

module Make_box (S : SHOW) = struct
  let render xs =
    String.concat "," (List.map S.show xs)
end

module Int_show = struct
  type t = int
  let show = string_of_int
end

module Int_box = Make_box (Int_show)

let () = print_endline (Int_box.render [ 1; 2; 3; 4 ])
