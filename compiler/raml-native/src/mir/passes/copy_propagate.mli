(** Push cheap copied values forward through structured [MIR]. The pass tracks
    copies in a forward environment, substitutes later uses with the cheaper
    source operand when it is safe, and preserves branch knowledge only when
    both sides agree. The main effect is to expose temporary traffic that the
    next cleanup pass can delete. *)
val program: Types.Program.t -> Types.Program.t
