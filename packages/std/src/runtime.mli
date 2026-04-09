(** # Runtime - Actor runtime owned by Std

    `Std.Runtime` is the runtime surface that currently delegates to the
    external `actors` package. `std` modules should depend on this module
    rather than naming `Actors.*` directly so the implementation can move
    inward later without another public-surface rewrite. *)

include module type of Actors
