(** Insert explicit [raml_poll] calls into [MIR] before ordinary calls. The
    pass walks the structured instruction tree, applies the rule under
    conditionals as well, and turns an implicit runtime obligation into normal
    IR that later passes and snapshots can see directly. *)
val program: Types.Program.t -> Types.Program.t
