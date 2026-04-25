open Std

type t

type lane = { profile: string; target: Riot_model.Target.t }

val retry_interval: Time.Duration.t

val path: target_dir_root:Path.t -> profile:string -> target:Riot_model.Target.t -> Path.t

val existing_lanes: target_dir_root:Path.t -> lane list

val release: t -> unit

val wait: on_waiting:(Path.t -> unit) -> target_dir_root:Path.t -> profile:string -> target:Riot_model.Target.t -> (t, exn) result

val acquire: on_waiting:(Path.t -> unit) -> target_dir_root:Path.t -> profile:string -> target:Riot_model.Target.t -> (unit -> ('a, 'b) result) -> ('a, 'b) result

val acquire_existing_lanes: on_waiting:(Path.t -> unit) -> target_dir_root:Path.t -> (unit -> ('a, 'b) result) -> ('a, 'b) result
