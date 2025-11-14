open Std

type fact_tuple = Value.t list

module type STORAGE = sig
  type t
  
  val get_facts : t -> predicate:string -> fact_tuple Relation.t
  val predicates : t -> string list
  val iter_facts : t -> predicate:string -> (fact_tuple -> unit) -> unit
  val get_facts_matching : t -> predicate:string -> pattern:Value.t option list -> fact_tuple Relation.t
end

let matches_pattern pattern tuple =
  let pattern_len = List.length pattern in
  let tuple_len = List.length tuple in
  if pattern_len = tuple_len then
    List.for_all2 (fun pat_opt value ->
      match pat_opt with
      | None -> true  (* Wildcard matches anything *)
      | Some pat_val -> Value.equal pat_val value
    ) pattern tuple
  else false
