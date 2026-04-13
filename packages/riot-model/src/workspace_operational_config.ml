open Std
open Std.Data

type cache_policy = {
  keep_generations: int;
  max_size_bytes: int64;
}

type test_policy = {
  small_test_timeout: Time.Duration.t option;
  flaky_max_retries: int;
}

type t = {
  cache: cache_policy;
  test: test_policy;
}

type error =
  | ReadFailed of { path: Path.t; error: string }
  | ParseFailed of { path: Path.t; error: string }
  | InvalidConfig of { path: Path.t; error: string }

let default_cache_policy = { keep_generations = 10; max_size_bytes = Int64.mul 50L 1_073_741_824L }

let default_test_policy = { small_test_timeout = None; flaky_max_retries = 0 }

let default = { cache = default_cache_policy; test = default_test_policy }

let message = function
  | ReadFailed { path; error } -> "failed to read workspace config '"
  ^ Path.to_string path
  ^ "': "
  ^ error
  | ParseFailed { path; error } -> "failed to parse workspace config '"
  ^ Path.to_string path
  ^ "': "
  ^ error
  | InvalidConfig { path; error } -> "invalid workspace config '" ^ Path.to_string path ^ "': " ^ error

let normalize_size = fun raw ->
  let raw = String.trim raw in
  let compact_length =
    String.fold_left
      ~fn:(fun count c ->
        if Char.equal c ' ' || Char.equal c '\t' then
          count
        else
          count + 1)
      ~acc:0
      raw
  in
  let compact = Kernel.Bytes.create ~size:compact_length in
  let _ =
    String.fold_left
      ~fn:(fun index c ->
        if Char.equal c ' ' || Char.equal c '\t' then
          index
        else (
          Kernel.Bytes.set_unchecked compact ~at:index ~char:c;
          index + 1
        ))
      ~acc:0
      raw
  in
  String.lowercase_ascii (Kernel.Bytes.to_string compact)

let unit_multiplier = function
  | ""
  | "b" -> Some 1L
  | "k"
  | "kb"
  | "kib" -> Some 1_024L
  | "m"
  | "mb"
  | "mib" -> Some 1_048_576L
  | "g"
  | "gb"
  | "gib" -> Some 1_073_741_824L
  | "t"
  | "tb"
  | "tib" -> Some 1_099_511_627_776L
  | _ -> None

let parse_max_size = fun raw ->
  let normalized = normalize_size raw in
  let len = String.length normalized in
  let rec split idx =
    if idx >= len then
      (normalized, "")
    else
      match String.get_unchecked normalized ~at:idx with
      | '0' .. '9'
      | '.' -> split (idx + 1)
      | _ ->
          (String.sub normalized ~offset:0 ~len:idx, String.sub normalized ~offset:idx ~len:(len - idx))
  in
  let number_str, unit_str = split 0 in
  if String.equal number_str "" then
    Error "max_size must start with a number"
  else
    match unit_multiplier unit_str with
    | None -> Error ("unsupported max_size unit '" ^ unit_str ^ "'")
    | Some multiplier -> (
        match Float.parse number_str with
        | None -> Error ("invalid max_size value '" ^ raw ^ "'")
        | Some number ->
            if number < 0.0 then
              Error "max_size must be non-negative"
            else
              let bytes = number *. Int64.to_float multiplier in
              Ok (Int64.from_float bytes)
      )

let parse_cache_policy = fun ~path fields ->
  let keep_generations =
    match Fields.get "keep_generations" fields with
    | None -> Ok default_cache_policy.keep_generations
    | Some value -> (
        match Toml.get_int value with
        | Some n when n > 0 -> Ok n
        | Some _ -> Error "riot.cache.keep_generations must be greater than 0"
        | None -> Error "riot.cache.keep_generations must be an integer"
      )
  in
  let max_size_bytes =
    match Fields.get "max_size" fields with
    | None -> Ok default_cache_policy.max_size_bytes
    | Some (Toml.String raw) -> parse_max_size raw
    | Some _ -> Error "riot.cache.max_size must be a string like \"50 GiB\""
  in
  match keep_generations, max_size_bytes with
  | Ok keep_generations, Ok max_size_bytes -> Ok { keep_generations; max_size_bytes }
  | (Error error, _)
  | (_, Error error) -> Error (InvalidConfig { path; error })

let normalize_duration = fun raw ->
  let raw = String.trim raw in
  let compact_length =
    String.fold_left
      ~fn:(fun count c ->
        if Char.equal c ' ' || Char.equal c '\t' then
          count
        else
          count + 1)
      ~acc:0
      raw
  in
  let compact = Kernel.Bytes.create ~size:compact_length in
  let _ =
    String.fold_left
      ~fn:(fun index c ->
        if Char.equal c ' ' || Char.equal c '\t' then
          index
        else (
          Kernel.Bytes.set_unchecked compact ~at:index ~char:c;
          index + 1
        ))
      ~acc:0
      raw
  in
  String.lowercase_ascii (Kernel.Bytes.to_string compact)

let duration_unit_seconds = function
  | ""
  | "s" -> Some 1.0
  | "ms" -> Some 0.001
  | "us" -> Some 0.000_001
  | "ns" -> Some 0.000_000_001
  | "m" -> Some 60.0
  | "h" -> Some 3600.0
  | _ -> None

let parse_duration = fun raw ->
  let normalized = normalize_duration raw in
  let len = String.length normalized in
  let rec split idx =
    if idx >= len then
      (normalized, "")
    else
      match String.get_unchecked normalized ~at:idx with
      | '0' .. '9'
      | '.' -> split (idx + 1)
      | _ ->
          (String.sub normalized ~offset:0 ~len:idx, String.sub normalized ~offset:idx ~len:(len - idx))
  in
  let number_str, unit_str = split 0 in
  if String.equal number_str "" then
    Error "small_test_timeout must start with a number"
  else
    match duration_unit_seconds unit_str with
    | None -> Error ("unsupported small_test_timeout unit '" ^ unit_str ^ "'")
    | Some multiplier -> (
        match Float.parse number_str with
        | None -> Error ("invalid small_test_timeout value '" ^ raw ^ "'")
        | Some number ->
            if number < 0.0 then
              Error "small_test_timeout must be non-negative"
            else
              Ok (Time.Duration.from_secs_float (number *. multiplier))
      )

let find_field = fun names fields ->
  Fields.get_first names fields

let parse_test_policy = fun ~path fields ->
  let small_test_timeout =
    match find_field [ "small_test_timeout" ] fields with
    | None ->
        Ok default_test_policy.small_test_timeout
    | Some (Toml.String raw) ->
        parse_duration raw |> Result.map ~fn:Option.some
    | Some value -> (
        match Toml.get_int value with
        | Some millis when millis >= 0 -> Ok (Some (Time.Duration.from_millis millis))
        | Some _ -> Error "riot.test.small_test_timeout must be non-negative"
        | None -> Error "riot.test.small_test_timeout must be a duration string like \"500ms\""
      )
  in
  let flaky_max_retries =
    match find_field
      [ "flaky_max_retries"; "flaky_max_retry"; "flakey_max_retries"; "flakey_max_retry" ]
      fields with
    | None -> Ok default_test_policy.flaky_max_retries
    | Some value -> (
        match Toml.get_int value with
        | Some n when n >= 0 -> Ok n
        | Some _ -> Error "riot.test.flaky_max_retries must be greater than or equal to 0"
        | None -> Error "riot.test.flaky_max_retries must be an integer"
      )
  in
  match small_test_timeout, flaky_max_retries with
  | Ok small_test_timeout, Ok flaky_max_retries -> Ok { small_test_timeout; flaky_max_retries }
  | (Error error, _)
  | (_, Error error) -> Error (InvalidConfig { path; error })

let of_toml = fun ~path toml ->
  match toml with
  | Toml.Table fields -> (
      match Fields.get "riot" fields with
      | None ->
          Ok default
      | Some (Toml.Table riot_fields) -> (
          let cache =
            match Fields.get "cache" riot_fields with
            | None -> Ok default_cache_policy
            | Some (Toml.Table cache_fields) -> parse_cache_policy ~path cache_fields
            | Some _ -> Error (InvalidConfig {
              path;
              error = "top-level [riot.cache] must be a table"
            })
          in
          let test =
            match Fields.get "test" riot_fields with
            | None -> Ok default_test_policy
            | Some (Toml.Table test_fields) -> parse_test_policy ~path test_fields
            | Some _ -> Error (InvalidConfig {
              path;
              error = "top-level [riot.test] must be a table"
            })
          in
          (
            match cache, test with
            | Ok cache, Ok test -> Ok { cache; test }
            | (Error err, _)
            | (_, Error err) -> Error err
          )
        )
      | Some _ ->
          Error (InvalidConfig { path; error = "top-level [riot] must be a table" })
    )
  | _ -> Error (InvalidConfig { path; error = "workspace config must be a TOML table" })

let load = fun ~workspace_root ->
  let path = Riot_dirs.workspace_operational_config_path ~workspace_root in
  match Fs.exists path with
  | Ok false ->
      Ok default
  | Error err ->
      Error (ReadFailed { path; error = IO.error_message err })
  | Ok true -> (
      match Fs.read_to_string path with
      | Error err -> Error (ReadFailed { path; error = IO.error_message err })
      | Ok source -> (
          match Toml.parse source with
          | Error err -> Error (ParseFailed { path; error = Toml.error_to_string err })
          | Ok toml -> of_toml ~path toml
        )
    )
