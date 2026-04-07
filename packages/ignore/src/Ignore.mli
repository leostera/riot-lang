(** Ignore-aware recursive walking layered above {!Std.Fs.Walker}.

    Use this package when you need filesystem traversal that respects
    gitignore-style rules, custom ignore files, and explicit override globs.
*)
(** Ignore-rule match results. *)
module Match = Match

(** Ignore-aware walking with gitignore-style sources and override globs.

    This is the entrypoint most callers want when they need to walk a tree
    while pruning ignored directories before descent.
*)
module Walker = Walker
