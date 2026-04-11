(** Normalize [NIR] into a more regular expression shape. The pass lifts nested
    [let] bindings out of call positions and conditions, flattens adjacent
    [let] chains, and nudges the tree toward an ANF-like form so later native
    passes can reason about one predictable structure. *)
val program: Types.Program.t -> Types.Program.t
