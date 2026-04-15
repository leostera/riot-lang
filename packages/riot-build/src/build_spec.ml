open Std

type scope = Resolved_build.scope =
  | Runtime
  | Dev

type t = Resolved_build.t

let make = Resolved_build.make

let workspace = Resolved_build.workspace

let package_names = Resolved_build.package_names

let targets = Resolved_build.targets

let scope = Resolved_build.scope

let profile = Resolved_build.profile

let requested_parallelism = Resolved_build.requested_parallelism
