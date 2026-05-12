open Std
open Std.Data

type backend =
  | Js
  | Wasm
  | Native
type t = {
  architecture: string;
  vendor: string;
  system: string;
  abi: string option;
}
val make: architecture:string -> vendor:string -> system:string -> ?abi:string -> unit -> t

val from_string: string -> (t, string) Std.Result.t

val backend_to_string: backend -> string

val backend_to_json: backend -> Json.t

val to_string: t -> string

val to_json: t -> Json.t

val backend: t -> backend

val select_backend: host:t -> target:t -> backend

val unknown_unknown_unknown: t

val js_unknown_ecma: t

val wasm32_unknown_unknown: t

val aarch64_apple_darwin: t

val aarch64_unknown_linux_gnu: t

val x86_64_unknown_linux_gnu: t

val x86_64_pc_windows_msvc: t
