open Std

type t = System.TargetTriple.t = {
  architecture: string;
  vendor: string;
  os: string;
  abi: string option;
}

type error = System.TargetTriple.error =
  | InvalidTripletFormat of { value: string }

module Set = struct
  type elt = t

  type t = elt Collections.HashSet.t

  let empty = fun () -> Collections.HashSet.create ()

  let singleton = fun target ->
    let set = empty () in
    let _ = Collections.HashSet.insert set ~value:target in
    set

  let from_list = Collections.HashSet.from_list

  let insert = fun set target ->
    let _ = Collections.HashSet.insert set ~value:target in
    ()

  let contains = fun set target -> Collections.HashSet.contains set ~value:target

  let length = Collections.HashSet.length

  let is_empty = Collections.HashSet.is_empty

  let to_list = fun set ->
    Collections.HashSet.to_list set
    |> List.sort
      ~compare:(fun left right ->
        String.compare
          (System.TargetTriple.to_string left)
          (System.TargetTriple.to_string right))
end

type request =
  | Host
  | All
  | Pattern of string
  | Exact of Set.t

type resolve_error = {
  pattern: string;
  available_targets: t list;
}

let current = System.TargetTriple.current

let error_message = System.TargetTriple.error_message

let from_string = System.TargetTriple.from_string

let to_string = System.TargetTriple.to_string

let equal = System.TargetTriple.equal

let compare = fun left right -> String.compare (to_string left) (to_string right)

let host = fun () -> current

let make_set = Set.from_list

let normalize_pattern = fun value ->
  String.trim value
  |> String.lowercase_ascii

let parse = fun value ->
  match normalize_pattern value with
  | "host"
  | "native" -> Host
  | "all" -> All
  | normalized -> (
      match from_string normalized with
      | Ok target -> Exact (Set.singleton target)
      | Error _ -> Pattern normalized
    )

let configured_targets = fun ~host (config: Toolchain_config.t) ->
  match config.targets with
  | [] -> Set.singleton host
  | targets ->
      let set = Set.from_list targets in
      if Set.is_empty set then
        Set.singleton host
      else
        set

let resolve = fun ~host ~configured_targets request ->
  match request with
  | Host -> Ok (Set.singleton host)
  | All -> Ok configured_targets
  | Exact targets -> Ok targets
  | Pattern pattern -> (
      match normalize_pattern pattern with
      | "host"
      | "native" -> Ok (Set.singleton host)
      | "all" -> Ok configured_targets
      | normalized -> (
          match from_string normalized with
          | Ok exact_target when Set.contains configured_targets exact_target ->
              Ok (Set.singleton exact_target)
          | Ok _
          | Error _ ->
              let matches =
                Set.to_list configured_targets
                |> List.filter ~fn:(fun target -> String.contains (to_string target) normalized)
              in
              if List.is_empty matches then
                Error { pattern = normalized; available_targets = Set.to_list configured_targets }
              else
                Ok (Set.from_list matches)
        )
    )

let request_to_string = fun __tmp1 ->
  match __tmp1 with
  | Host -> "host"
  | All -> "all"
  | Pattern pattern -> pattern
  | Exact targets ->
      Set.to_list targets
      |> List.map ~fn:to_string
      |> String.concat ","

let is_cross = fun target -> not (equal target current)

let platform_name = fun target ->
  match target.os with
  | "darwin" -> "macos"
  | "linux" -> "linux"
  | "windows" -> "windows"
  | other -> other

let hash = fun state target -> Crypto.Sha256.write state (to_string target)
