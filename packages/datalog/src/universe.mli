open Std

(** {1 Universe - Datalog Knowledge Base}
    
    A Universe contains:
    - Base facts (from storage backend)
    - Derived facts (computed by rules)
    - Rules for deriving new facts
    
    The Universe is a functor over a storage backend, allowing you to
    use different backends (InMemory, Poneglyph, SQLite, etc.).
    
    {2 Key Design}
    
    - Base facts live in storage (e.g., Poneglyph graph)
    - Derived facts are computed in-memory during evaluation
    - Rules are stored in a vector for iteration
    - Storage is read-only (never modified during evaluation)
    
    {2 Example}
    
    {[
      (* Create with in-memory storage *)
      module U = Universe.Make(InmemoryStorage)
      
      let storage = InmemoryStorage.create () in
      InmemoryStorage.add_fact storage 
        ~predicate:"edge" ~tuple:[Int 1; Int 2];
      
      let universe = U.create storage in
      let universe = U.add_rule universe rule in
      let universe = U.eval universe in
      
      (* Query derived facts *)
      U.get_facts universe ~predicate:"reachable"
    ]}
*)

(** {2 Universe Functor} *)

module Make (S : Storage.STORAGE) : sig
  type t
  (** A Datalog universe with storage backend [S] *)
  
  (** {3 Construction} *)
  
  val create : S.t -> t
  (** Create universe with given storage backend.
      The storage contains base facts (never modified).
  *)
  
  (** {3 Rules} *)
  
  val add_rule : t -> Ast.rule -> t
  (** Add a derivation rule.
      Rules are evaluated during [eval] to compute derived facts.
      
      Example:
      {[
        let rule = Ast.rule 
          ~head:(Ast.atom ~predicate:"path" ~args:[Var "X"; Var "Y"])
          ~body:[Ast.Atom (Ast.atom ~predicate:"edge" ~args:[Var "X"; Var "Y"])]
        in
        add_rule universe rule
      ]}
  *)
  
  val add_rules : t -> Ast.rule list -> t
  (** Add multiple rules at once *)
  
  val rules : t -> Ast.rule list
  (** Get all rules in universe *)
  
  (** {3 Derived Facts} *)
  
  val add_derived_fact : t -> predicate:string -> tuple:Storage.fact_tuple -> unit
  (** Add a derived fact (computed by evaluator).
      This is typically called by the evaluation engine, not users.
  *)
  
  val add_derived_facts : t -> predicate:string -> tuples:Storage.fact_tuple Relation.t -> unit
  (** Add multiple derived facts at once *)
  
  val clear_derived : t -> unit
  (** Clear all derived facts (keeps rules and base facts) *)
  
  (** {3 Fact Access} *)
  
  val get_facts : t -> predicate:string -> Storage.fact_tuple Relation.t
  (** Get all facts for a predicate (base + derived).
      This merges facts from storage with computed derived facts.
      
      Time: O(n + m) where n=base facts, m=derived facts
  *)
  
  val get_base_facts : t -> predicate:string -> Storage.fact_tuple Relation.t
  (** Get only base facts from storage (no derived facts) *)
  
  val get_derived_facts : t -> predicate:string -> Storage.fact_tuple Relation.t
  (** Get only derived facts (no base facts) *)
  
  val contains_fact : t -> predicate:string -> tuple:Storage.fact_tuple -> bool
  (** Check if a fact exists (base or derived) *)
  
  (** {3 Introspection} *)
  
  val predicates : t -> string list
  (** All predicate names (base + derived) *)
  
  val base_predicates : t -> string list
  (** Predicate names from storage *)
  
  val derived_predicates : t -> string list
  (** Predicate names with derived facts *)
  
  val storage : t -> S.t
  (** Access underlying storage (for advanced use) *)
end

(** {2 Default Universe}
    
    Pre-instantiated Universe with InMemory storage for convenience.
*)

module InMemory : sig
  include module type of Make(Inmemory_storage)
  
  val create_empty : unit -> t
  (** Create universe with empty in-memory storage *)
  
  val of_facts : (string * Storage.fact_tuple list) list -> t
  (** Create universe from facts list *)
end
