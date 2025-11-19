module PoneglyphStorage : sig
  type t = Graph_store.t

  val get_facts_matching :
    t ->
    predicate:string ->
    pattern:Datalog.Value.t option list ->
    Datalog.Value.t list Datalog.Relation.t
end
