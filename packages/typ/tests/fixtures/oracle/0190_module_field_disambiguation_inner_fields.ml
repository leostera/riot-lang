(* oracle corpus fixture
   category: 04_records
   title: module_field_disambiguation_inner_fields
   complexity: 4
   min_ocaml: 4.08
   tags: records, modules, field_disambiguation
*)

module A = struct
  type t = { value : float }
end

module B = struct
  type t = { value : unit }
end

let get_a (x : A.t) = x.value
let get_b (x : B.t) = x.value

let answer = (get_a { A.value = 1.0 }, get_b { B.value = () })
