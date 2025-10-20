open Std

type t = { uri : Uri.t; ns : Uri.t; kind : Uri.t; facts : Fact.t list }

let make ~uri ~ns ~kind ~facts = { uri; ns; kind; facts }
let with_facts entity facts = { entity with facts }
let add_fact entity fact = { entity with facts = fact :: entity.facts }
