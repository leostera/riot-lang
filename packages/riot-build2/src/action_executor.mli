open Std

type t

val create: store:Riot_store.Store.t -> toolchains:Toolchain_service.t -> unit -> t

val results: t -> Action_execution.result list

val find_result: t -> Action_execution.ref_ -> Action_execution.result option

val artifact: t -> Action_execution.ref_ -> Riot_store.Artifact.t option

val failure: t -> Action_execution.ref_ -> string option

val requires_toolchain: Action_execution.t -> bool

val execute: t -> Action_execution.t -> (Work_result.t, Error.t) result
