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

let make = fun ~architecture ~vendor ~system ?abi () -> { architecture; vendor; system; abi }

let backend_to_string = fun backend ->
  match backend with
  | Js -> "js"
  | Wasm -> "wasm"
  | Native -> "native"

let backend_to_json = fun backend -> Json.string (backend_to_string backend)

let to_string = fun target ->
  let parts = [ Some target.architecture; Some target.vendor; Some target.system; target.abi ] in
  parts |> List.filter_map (fun value -> value) |> String.concat "-"

let to_json = fun target -> Json.string (to_string target)

let backend = fun target ->
  if String.equal target.architecture "js" then
    Js
  else if String.equal target.system "ecma" then
    Js
  else if String.starts_with ~prefix:"wasm" target.architecture then
    Wasm
  else
    Native

let select_backend = fun ~host:_ ~target -> backend target

let unknown_unknown_unknown = make ~architecture:"unknown" ~vendor:"unknown" ~system:"unknown" ()

let js_unknown_ecma = make ~architecture:"js" ~vendor:"unknown" ~system:"ecma" ()

let wasm32_unknown_unknown = make ~architecture:"wasm32" ~vendor:"unknown" ~system:"unknown" ()

let aarch64_apple_darwin = make ~architecture:"aarch64" ~vendor:"apple" ~system:"darwin" ()

let aarch64_unknown_linux_gnu = make
  ~architecture:"aarch64"
  ~vendor:"unknown"
  ~system:"linux"
  ~abi:"gnu"
  ()

let x86_64_unknown_linux_gnu = make
  ~architecture:"x86_64"
  ~vendor:"unknown"
  ~system:"linux"
  ~abi:"gnu"
  ()

let x86_64_pc_windows_msvc = make ~architecture:"x86_64" ~vendor:"pc" ~system:"windows" ~abi:"msvc" ()
