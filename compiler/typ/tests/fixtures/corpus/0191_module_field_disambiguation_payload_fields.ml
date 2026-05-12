(* oracle corpus fixture
   category: 04_records
   title: module_field_disambiguation_payload_fields
   complexity: 4
   min_ocaml: 4.08
   tags: records, modules, field_disambiguation
*)

module A = struct
  type t = { value : string }
end

module B = struct
  type t = { value : char }
end

let get_a (x : A.t) = x.value
let get_b (x : B.t) = x.value

let answer = (get_a { A.value = "x" }, get_b { B.value = 'y' })
