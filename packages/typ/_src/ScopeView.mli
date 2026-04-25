open Std
open Model

type t

val empty: unit -> t

val create: visible_modules:(SurfacePath.t * PackageEnv.ModuleId.t) list -> implicit_open_modules:(SurfacePath.t * PackageEnv.ModuleId.t) list -> t

val resolve_visible_module_prefix: t -> SurfacePath.t -> (SurfacePath.t * PackageEnv.ModuleId.t * SurfacePath.t) option

val implicit_open_modules: t -> (SurfacePath.t * PackageEnv.ModuleId.t) list

val visible_modules: t -> (SurfacePath.t * PackageEnv.ModuleId.t) list

val visible_module_ids: t -> PackageEnv.ModuleId.t list
