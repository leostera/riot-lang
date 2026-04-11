(** Late JIR passes exposed individually.

    Composition stays explicit in [Jir.Lowering]; this module is only the
    namespace that groups the concrete passes. *)
module Flatten = Flatten

(** See [Alpha] for the printable-name collision pass. *)
module Alpha = Alpha

(** See [Remove_aliases] for trivial alias elimination. *)
module Remove_aliases = Remove_aliases

(** See [Dce] for local dead code elimination. *)
module Dce = Dce

(** See [Normalize] for structural cleanup and import collection. *)
module Normalize = Normalize

(** See [Materialize_imports] for the resolved-JIR boundary pass. *)
module Materialize_imports = Materialize_imports

(** See [Prune_imports] for late dead-import cleanup. *)
module Prune_imports = Prune_imports
