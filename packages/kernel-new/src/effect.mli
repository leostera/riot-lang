(** Extensible effect row reserved for Riot-owned runtime hooks. `kernel-new` exposes the row so
    higher layers can name it without tying themselves to backend details. *)
type _ t = ..
