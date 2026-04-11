  type t

    val create : unit -> t

    val report : Diagnostic.t -> t -> unit
