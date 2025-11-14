open Std

(** {1 Variable - Semi-Naive Evaluation Support}
    
    In semi-naive evaluation, we track which facts are "new" (recent) vs "old" (stable).
    This allows us to only process the delta (Δ) facts each iteration, dramatically
    improving performance.
    
    {2 Semi-Naive Principle}
    
    Instead of joining stable ⋈ stable (already computed!), we only join:
    - recent ⋈ stable  
    - stable ⋈ recent
    
    {2 Example}
    
    {[
      (* Iteration 1 *)
      recent: [(1, 2)]
      stable: []
      
      (* Iteration 2 *)  
      recent: [(1, 3), (2, 3)]   (* New facts *)
      stable: [(1, 2)]           (* Previous recent moved here *)
      
      (* Iteration 3 *)
      recent: []                 (* No new facts - done! *)
      stable: [(1, 2), (1, 3), (2, 3)]
    ]}
*)

type 'a t
(** A variable tracks recent (Δ) and stable facts *)

(** {2 Construction} *)

val create : unit -> 'a t
(** Create empty variable (no facts) *)

val of_relation : 'a Relation.t -> 'a t
(** Create variable with initial facts in recent *)

(** {2 Access} *)

val recent : 'a t -> 'a Relation.t
(** Get recent (new) facts *)

val stable : 'a t -> 'a Relation.t
(** Get stable (old) facts *)

val all : 'a t -> 'a Relation.t
(** Get all facts (recent ∪ stable) *)

(** {2 Mutation} *)

val insert : 'a t -> 'a Relation.t -> unit
(** Add facts to recent.
    Only truly new facts (not in recent or stable) are added.
    
    Example:
    {[
      let var = create () in
      insert var (Relation.of_list [1; 2]);
      (* recent: [1; 2], stable: [] *)
      
      insert var (Relation.of_list [2; 3]);
      (* recent: [1; 2; 3], stable: [] - 2 was already there *)
    ]}
*)

val complete : 'a t -> unit
(** Move recent → stable, clear recent.
    Call this at end of each iteration.
    
    Example:
    {[
      let var = of_relation (Relation.of_list [1; 2]) in
      (* recent: [1; 2], stable: [] *)
      
      complete var;
      (* recent: [], stable: [1; 2] *)
    ]}
*)

(** {2 Queries} *)

val changed : 'a t -> bool
(** Check if recent is non-empty (indicates work to do).
    
    Example:
    {[
      let var = create () in
      assert (not (changed var));  (* No recent facts *)
      
      insert var (Relation.singleton 1);
      assert (changed var);  (* Has recent facts *)
      
      complete var;
      assert (not (changed var));  (* Recent now empty *)
    ]}
*)

val is_empty : 'a t -> bool
(** Check if variable has no facts at all (recent or stable) *)

val size : 'a t -> int
(** Total number of facts (recent + stable) *)

(** {2 Semi-Naive Helpers} *)

val new_facts : 'a t -> 'a Relation.t -> 'a Relation.t
(** Filter relation to only facts not in this variable.
    Used to compute Δ (delta).
    
    Example:
    {[
      let var = of_relation (Relation.of_list [1; 2]) in
      complete var;  (* Move to stable *)
      
      let candidates = Relation.of_list [2; 3; 4] in
      let delta = new_facts var candidates in
      (* Returns [3; 4] - only truly new facts *)
    ]}
*)
