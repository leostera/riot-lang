open Std

type t = {
  by_name: (string, ModuleTypings.t) Collections.HashMap.t;
  count: int;
  mutable values_cache: ModuleTypings.t list option;
  mutable names_cache: string list option;
  mutable stable_key_cache: string option;
}

let make = fun ~by_name ~count -> {
  by_name;
  count;
  values_cache = None;
  names_cache = None;
  stable_key_cache = None;
}

let empty = make ~by_name:(Collections.HashMap.create ()) ~count:0

let of_list = fun summaries ->
  let by_name = Collections.HashMap.with_capacity (List.length summaries) in
  let count = ref 0 in
  let insert summary =
    let module_name = ModuleTypings.module_name summary in
    match Collections.HashMap.insert by_name module_name summary with
    | None -> count := !count + 1
    | Some _ -> ()
  in
  summaries |> List.iter insert;
  make ~by_name ~count:!count

let len = fun loaded_modules -> loaded_modules.count

let is_empty = fun loaded_modules -> Int.equal loaded_modules.count 0

let get = fun loaded_modules ~module_name ->
  Collections.HashMap.get loaded_modules.by_name module_name

let contains = fun loaded_modules ~module_name ->
  Collections.HashMap.contains_key loaded_modules.by_name module_name

let iter = fun f loaded_modules ->
  Collections.HashMap.iter f loaded_modules.by_name

let fold = fun f loaded_modules init ->
  Collections.HashMap.fold f loaded_modules.by_name init

let merge = fun ~preferred ~fallback ~combine ->
  let by_name = Collections.HashMap.with_capacity (preferred.count + fallback.count) in
  let count = ref 0 in
  let insert_summary summary =
    let module_name = ModuleTypings.module_name summary in
    match Collections.HashMap.insert by_name module_name summary with
    | None -> count := !count + 1
    | Some _ -> ()
  in
  iter
    (fun _module_name summary -> insert_summary summary)
    preferred;
  iter
    (fun module_name summary ->
      match Collections.HashMap.get by_name module_name with
      | None -> insert_summary summary
      | Some existing ->
          let _ = Collections.HashMap.insert by_name module_name (combine existing summary) in
          ())
    fallback;
  make ~by_name ~count:!count

let values = fun loaded_modules ->
  match loaded_modules.values_cache with
  | Some values -> values
  | None ->
      let values = Collections.HashMap.values loaded_modules.by_name in
      loaded_modules.values_cache <- Some values;
      values

let names = fun loaded_modules ->
  match loaded_modules.names_cache with
  | Some names -> names
  | None ->
      let names = Collections.HashMap.keys loaded_modules.by_name in
      loaded_modules.names_cache <- Some names;
      names

let stable_key = fun loaded_modules ->
  match loaded_modules.stable_key_cache with
  | Some cache_key -> cache_key
  | None ->
      let cache_key =
        values loaded_modules
        |> List.map
          (fun typings ->
            format
              Format.[ str (ModuleTypings.module_name typings); str ":"; str
                  (ModuleTypings.source_hash typings |> Crypto.Digest.hex); ])
        |> List.sort String.compare
        |> String.concat "|"
      in
      loaded_modules.stable_key_cache <- Some cache_key;
      cache_key
