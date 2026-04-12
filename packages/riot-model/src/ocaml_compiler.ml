open Std

(** Compilation mode *)
type mode =
  | Compile
  (** Compile to object file (-c flag) *)
  | Library
  (** Create library archive (-a flag) *)
  | Executable
  (** Link executable (default, no special flag) *)
  | CustomExe

(** Link with C stubs (-custom flag) *)
(** OCaml compiler warnings *)
type warning =
  | PartialMatch
  (** Warning 8: partial pattern match *)
  | BadModuleName
  (** Warning 24: source file name is not a valid module name *)
  | UnusedVariable
  (** Warning 26: unused variable *)
  | UnusedOpen
  (** Warning 33: unused open *)
  | UnusedConstructor
  (** Warning 34: unused constructor *)
  | UnusedMatch
  (** Warning 11: unused match case *)
  | NoCmiFile
  (** Warning 49: missing cmi when looking up module alias *)
  | All

(** All warnings *)
let warning_to_number = function
  | PartialMatch -> 8
  | BadModuleName -> 24
  | UnusedVariable -> 26
  | UnusedOpen -> 33
  | UnusedConstructor -> 34
  | UnusedMatch -> 11
  | NoCmiFile -> 49
  | All -> (-1)

(* Special: use 'a' *)

let warning_to_string = function
  | PartialMatch -> "partial-match"
  | BadModuleName -> "bad-module-name"
  | UnusedVariable -> "unused-variable"
  | UnusedOpen -> "unused-open"
  | UnusedConstructor -> "unused-constructor"
  | UnusedMatch -> "unused-match"
  | NoCmiFile -> "no-cmi-file"
  | All -> "all"

let warning_of_string = function
  | "partial-match" -> Some PartialMatch
  | "bad-module-name" -> Some BadModuleName
  | "unused-variable" -> Some UnusedVariable
  | "unused-open" -> Some UnusedOpen
  | "unused-constructor" -> Some UnusedConstructor
  | "unused-match" -> Some UnusedMatch
  | "no-cmi-file" -> Some NoCmiFile
  | "all" -> Some All
  | _ -> None

(** OCaml compiler flags - commonly used options *)
type flag =
  | NoAliasDeps
  (** -no-alias-deps: Do not record dependencies for module aliases *)
  | Open of string
  (** -open Module: Auto-open module before typing *)
  | NoStdlib
  (** -nostdlib: Do not automatically link with stdlib *)
  | NoPervasives
  (** -nopervasives: Do not open Pervasives/Stdlib *)
  | Inline of int
  | NoAssert
  | Compact
  | Unsafe
  | Impl of Path.t
  (** -impl <file>: Compile <file> as implementation *)
  | Warning of warning list
  | WarnError of warning list
  | Raw of string
  | LinkAll

(** -w: Configure warning flags *)
let warning_code = function
  | All -> "a"
  | warning -> warning_to_number warning |> Int.to_string

let render_warning_spec = fun ~sign warnings ->
  warnings |> List.map (fun warning -> String.make 1 sign ^ warning_code warning) |> String.concat ""

let parse_warning_spec = fun ~sign spec ->
  let warning_of_code = function
    | "8" -> Some PartialMatch
    | "24" -> Some BadModuleName
    | "11" -> Some UnusedMatch
    | "26" -> Some UnusedVariable
    | "33" -> Some UnusedOpen
    | "34" -> Some UnusedConstructor
    | "49" -> Some NoCmiFile
    | "a" -> Some All
    | _ -> None
  in
  if String.is_empty spec || not (String.starts_with ~prefix:(String.make 1 sign) spec) then
    None
  else
    String.split_on_char sign spec |> List.filter (fun token -> token != "") |> List.fold_left
      (fun acc token ->
        match (acc, warning_of_code token) with
        | Error _, _ -> acc
        | Ok warnings, Some warning -> Ok (warning :: warnings)
        | Ok _, None -> Error ())
      (Ok []) |> function
    | Ok warnings -> Some (List.rev warnings)
    | Error () -> None

let flags_to_string = fun flags ->
  List.fold_left
    (fun acc flag ->
      match flag with
      | Open m -> acc @ [ "-open"; m ]
      | NoAliasDeps -> acc @ [ "-no-alias-deps" ]
      | NoStdlib -> acc @ [ "-nostdlib" ]
      | NoPervasives -> acc @ [ "-nopervasives" ]
      | Inline threshold -> acc @ [ "-inline"; Int.to_string threshold ]
      | NoAssert -> acc @ [ "-noassert" ]
      | Compact -> acc @ [ "-compact" ]
      | Unsafe -> acc @ [ "-unsafe" ]
      | Impl file -> acc @ [ "-impl"; Path.to_string file ]
      | Warning warnings ->
          if List.is_empty warnings then
            acc
          else
            acc @ [ "-w"; render_warning_spec ~sign:'-' warnings ]
      | WarnError warnings ->
          if List.is_empty warnings then
            acc
          else
            acc @ [ "-warn-error"; render_warning_spec ~sign:'+' warnings ]
      | Raw flag -> acc @ [ flag ]
      | LinkAll -> acc @ [ "-linkall" ])
    []
    flags

let flags_of_string = fun raw_flags ->
  let rec go acc = function
    | [] ->
        List.rev acc
    | "-open" :: mod_name :: rest ->
        go (Open mod_name :: acc) rest
    | "-no-alias-deps" :: rest ->
        go (NoAliasDeps :: acc) rest
    | "-nostdlib" :: rest ->
        go (NoStdlib :: acc) rest
    | "-nopervasives" :: rest ->
        go (NoPervasives :: acc) rest
    | "-inline" :: threshold :: rest -> (
        try go (Inline (Int.parse threshold) :: acc) rest with
        | _ -> go (Raw threshold :: Raw "-inline" :: acc) rest
      )
    | "-noassert" :: rest ->
        go (NoAssert :: acc) rest
    | "-compact" :: rest ->
        go (Compact :: acc) rest
    | "-unsafe" :: rest ->
        go (Unsafe :: acc) rest
    | "-impl" :: file :: rest ->
        go (Impl (Path.v file) :: acc) rest
    | "-w" :: warning_spec :: rest -> (
        match parse_warning_spec ~sign:'-' warning_spec with
        | Some warnings -> go (Warning warnings :: acc) rest
        | None -> go (Raw warning_spec :: Raw "-w" :: acc) rest
      )
    | "-warn-error" :: warning_spec :: rest -> (
        match parse_warning_spec ~sign:'+' warning_spec with
        | Some warnings -> go (WarnError warnings :: acc) rest
        | None -> go (Raw warning_spec :: Raw "-warn-error" :: acc) rest
      )
    | "-linkall" :: rest ->
        go (LinkAll :: acc) rest
    | raw :: rest ->
        go (Raw raw :: acc) rest
  in
  go [] raw_flags

(** Compilation kind *)
type compilation_kind =
  | Bytecode
  (** ocamlc - fast compilation, slower runtime *)
  | Native

(** ocamlopt - slower compilation, optimized runtime *)
let compilation_kind_to_string = function
  | Bytecode -> "bytecode"
  | Native -> "native"

let compilation_kind_of_string = function
  | "bytecode" -> Some Bytecode
  | "native" -> Some Native
  | _ -> None
