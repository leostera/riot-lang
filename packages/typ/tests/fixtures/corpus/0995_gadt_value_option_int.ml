(* oracle corpus fixture
   category: 14_schema_expansion
   title: gadt_value_option_int
   complexity: 7
   min_ocaml: 4.08
   tags: schema, gadt, module
*)

type 'a option =
  | Some of 'a
  | None

module M = struct
  type _ t = Value : 'a -> 'a t
  let unwrap : type a. a t -> a = function
    | Value x -> x
end

let answer = M.unwrap (M.Value (Some 0))
