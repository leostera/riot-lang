(* oracle corpus fixture
   category: 12_gadts
   title: module_gadt_string
   complexity: 8
   min_ocaml: 4.08
   tags: gadts, modules
*)

module M = struct
  type _ t =
    | Value : 'a -> 'a t

  let unwrap : type a. a t -> a = function
    | Value x -> x
end

let answer = M.unwrap (M.Value "m")
