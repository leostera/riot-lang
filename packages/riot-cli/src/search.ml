open Std

type error =
  | MissingQuery
  | InvalidLimit of string
  | SearchFailed of Riot_deps.package_error

let command =
  let open ArgParser in
    let open Arg in command "search"
    |> about "Search pkgs.ml for packages by name"
    |> args
      [
        positional "query" |> help "Package query, for example mini or jsonrpc";
        option "limit" |> long "limit" |> short 'n' |> help "Maximum number of results to return (default: 10)";
        flag "json" |> long "json" |> help "Render results as JSON";
      ]

let message = function
  | MissingQuery -> "missing search query"
  | InvalidLimit value -> "invalid --limit value '" ^ value ^ "'"
  | SearchFailed error -> Riot_deps.package_error_message error

let fail = fun err ->
  eprintln ("\027[1;31mError\027[0m: " ^ message err);
  Error (Failure (message err))

let int_of_string_opt = fun value ->
  try Some (int_of_string value) with
  | Failure _ -> None

let request_of_matches = fun matches ->
  let query =
    match ArgParser.get_one matches "query" with
    | Some query when not (String.equal (String.trim query) "") -> Ok query
    | _ -> Error MissingQuery
  in
  let limit =
    match ArgParser.get_one matches "limit" with
    | None -> Ok 10
    | Some value -> (
        match int_of_string_opt value with
        | Some limit when limit > 0 -> Ok limit
        | _ -> Error (InvalidLimit value)
      )
  in
  match query, limit with
  | Ok query, Ok limit -> Ok Riot_deps.{ query; limit }
  | (Error err, _)
  | (_, Error err) -> Error err

let json_of_result = fun (result: Riot_deps.suggested_package) ->
  Data.Json.Object [
    ("package", Data.Json.String result.package);
    ("latest_version", Data.Json.String result.latest_version);
    (
      "description",
      match result.description with
      | Some description -> Data.Json.String description
      | None -> Data.Json.Null
    );
  ]

let write_human_results = fun ~query results ->
  match results with
  | [] -> println ("No packages found for '" ^ query ^ "'")
  | results ->
      List.iter
        (fun (result: Riot_deps.suggested_package) ->
          match result.description with
          | Some description -> println
            (result.package ^ "@" ^ result.latest_version ^ " - " ^ description)
          | None -> println (result.package ^ "@" ^ result.latest_version))
        results

let run = fun matches ->
  let json = ArgParser.get_flag matches "json" in
  match request_of_matches matches with
  | Error err -> fail err
  | Ok request -> (
      match Riot_deps.search ~request () with
      | Error error -> fail (SearchFailed error)
      | Ok results ->
          if json then
            results
            |> List.map json_of_result
            |> fun items -> Data.Json.Array items |> Data.Json.to_string |> println
          else
            write_human_results ~query:request.query results;
          Ok ()
    )
