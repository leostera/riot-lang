open Std
open Result.Syntax

module Bytes = Std.IO.Bytes

let mysql_container_port = 3_306

let mysql_container_user = "riot"

let mysql_container_password = "riot"

let mysql_container_database = "riot_test"

let mysql_url () =
  match Dotenv.load_if_exists ~env:"test" () with
  | Error error -> Error (Dotenv.error_to_string error)
  | Ok _bindings -> Ok (Env.get Env.String ~var:"MYSQL_TEST_URL")

let mysql_url_from_env = fun name ->
  match Dotenv.load_if_exists ~env:"test" () with
  | Error error -> Error (Dotenv.error_to_string error)
  | Ok _bindings -> Ok (Env.get Env.String ~var:name)

let with_connection_config config fn =
  match Mysql.Driver.connect config with
  | Error error -> Error (Mysql.Driver.error_to_string error)
  | Ok connection ->
      let result = fn connection in
      Mysql.Driver.close connection;
      result

let with_connection url fn =
  match Mysql.Config.from_string url with
  | Error error -> Error ("invalid mysql test url: " ^ Mysql.Config.parse_error_to_string error)
  | Ok config -> with_connection_config config fn

let execute connection sql params =
  let* statement =
    Mysql.Driver.prepare connection sql
    |> Result.map_err ~fn:Mysql.Driver.error_to_string
  in
  let* result_set =
    Mysql.Driver.execute statement params
    |> Result.map_err ~fn:Mysql.Driver.error_to_string
  in
  Ok result_set

let execute_unit connection sql params =
  let* _result_set = execute connection sql params in
  Ok ()

let query_one connection sql params =
  let* result_set = execute connection sql params in
  match Mysql.Driver.fetch_row result_set with
  | Some row -> Ok row
  | None -> Error "expected one row"

let option_string_to_string = fun value ->
  match value with
  | Some value -> "Some(" ^ value ^ ")"
  | None -> "None"

let option_int_to_string = fun value ->
  match value with
  | Some value -> "Some(" ^ Int.to_string value ^ ")"
  | None -> "None"

let option_float_to_string = fun value ->
  match value with
  | Some value -> "Some(" ^ Float.to_string value ^ ")"
  | None -> "None"

let option_date_to_string = fun value ->
  match value with
  | Some (year, month, day) ->
      "Some(" ^ Int.to_string year ^ "-" ^ Int.to_string month ^ "-" ^ Int.to_string day ^ ")"
  | None -> "None"

let option_time_to_string = fun value ->
  match value with
  | Some (hour, minute, second, micros) ->
      "Some("
      ^ Int.to_string hour
      ^ ":"
      ^ Int.to_string minute
      ^ ":"
      ^ Int.to_string second
      ^ "."
      ^ Int.to_string micros
      ^ ")"
  | None -> "None"

let expect_string = fun ~field ~expected actual ->
  if Option.equal actual (Some expected) ~fn:String.equal then
    Ok ()
  else
    Error ("field "
    ^ field
    ^ " expected Some("
    ^ expected
    ^ "), got "
    ^ option_string_to_string actual)

let expect_int = fun ~field ~expected actual ->
  if Option.equal actual (Some expected) ~fn:Int.equal then
    Ok ()
  else
    Error ("field "
    ^ field
    ^ " expected Some("
    ^ Int.to_string expected
    ^ "), got "
    ^ option_int_to_string actual)

let expect_float = fun ~field ~expected actual ->
  match actual with
  | Some actual when Float.abs (actual -. expected) <= 0.000_001 -> Ok ()
  | _ ->
      Error ("field "
      ^ field
      ^ " expected approximately "
      ^ Float.to_string expected
      ^ ", got "
      ^ option_float_to_string actual)

let expect_date = fun ~field ~expected actual ->
  if Option.equal actual (Some expected) ~fn:(fun left right -> left = right) then
    Ok ()
  else
    Error ("field "
    ^ field
    ^ " expected "
    ^ option_date_to_string (Some expected)
    ^ ", got "
    ^ option_date_to_string actual)

let expect_time = fun ~field ~expected actual ->
  if Option.equal actual (Some expected) ~fn:(fun left right -> left = right) then
    Ok ()
  else
    Error ("field "
    ^ field
    ^ " expected "
    ^ option_time_to_string (Some expected)
    ^ ", got "
    ^ option_time_to_string actual)

let expect_bytes = fun ~field ~expected actual ->
  match actual with
  | Some actual when String.equal (Bytes.to_string actual) expected -> Ok ()
  | Some actual ->
      Error ("field "
      ^ field
      ^ " expected "
      ^ Int.to_string (String.length expected)
      ^ " bytes, got "
      ^ Int.to_string (Bytes.length actual)
      ^ " bytes")
  | None -> Error ("field " ^ field ^ " expected bytes, got None")

let expect_null = fun ~field row ->
  match Sqlx_driver.Row.get field row with
  | Some Sqlx_driver.Value.Null -> Ok ()
  | Some value ->
      Error ("field " ^ field ^ " expected NULL, got " ^ Sqlx_driver.Value.to_string value)
  | None -> Error ("field " ^ field ^ " expected NULL, got missing field")

let timestamp_matches = fun actual expected ->
  let (actual_micros, _) = DateTime.(actual.microseconds) in
  let (expected_micros, _) = DateTime.(expected.microseconds) in
  DateTime.(actual.year) = DateTime.(expected.year)
  && DateTime.(actual.month) = DateTime.(expected.month)
  && DateTime.(actual.day) = DateTime.(expected.day)
  && DateTime.(actual.hour) = DateTime.(expected.hour)
  && DateTime.(actual.minute) = DateTime.(expected.minute)
  && DateTime.(actual.second) = DateTime.(expected.second)
  && actual_micros = expected_micros

let expect_timestamp_fields = fun ~field ~expected actual ->
  match actual with
  | Some actual when timestamp_matches actual expected -> Ok ()
  | Some actual ->
      Error ("field "
      ^ field
      ^ " expected "
      ^ DateTime.to_iso8601 expected
      ^ ", got "
      ^ DateTime.to_iso8601 actual)
  | None -> Error ("field " ^ field ^ " expected timestamp, got None")

let row_int_like = fun field row ->
  match Sqlx_driver.Row.get field row with
  | None -> None
  | Some value -> (
      match Sqlx_driver.Value.to_int value with
      | Some value -> Some value
      | None -> (
          match Sqlx_driver.Value.to_int16 value with
          | Some value -> Some value
          | None -> (
              match Sqlx_driver.Value.to_int64 value with
              | Some value -> Some (Int64.to_int value)
              | None -> None
            )
        )
    )

let row_text_like = fun field row ->
  match Sqlx_driver.Row.get field row with
  | None -> None
  | Some value -> (
      match Sqlx_driver.Value.to_numeric value with
      | Some value -> Some value
      | None -> Sqlx_driver.Value.to_string_value value
    )

let timestamp = fun ~year ~month ~day ~hour ~minute ~second ~microsecond ->
  DateTime.from_naive
    DateTime.{
      year;
      month;
      day;
      hour;
      minute;
      second;
      microsecond;
    }
    ~tz:DateTime.Tz.Etc_UTC

let run_cases = fun cases ~fn ->
  let rec loop cases =
    match cases with
    | [] -> Ok ()
    | (name, value) :: rest -> (
        match fn name value with
        | Ok () -> loop rest
        | Error message -> Error (name ^ ": " ^ message)
      )
  in
  loop cases

let assert_parameterized_query_roundtrips = fun connection ->
  let* row =
    query_one
      connection
      "SELECT CAST(? AS CHAR) AS value, CAST(? AS SIGNED) AS n"
      [ Sqlx_driver.Value.string "hello"; Sqlx_driver.Value.int 42 ]
  in
  let* () = expect_string
    ~field:"value"
    ~expected:"hello"
    (Sqlx_driver.Row.string "value" row)
  in
  expect_int
    ~field:"n"
    ~expected:42
    (row_int_like "n" row)

let assert_transaction_roundtrips = fun connection ->
  let* () =
    Mysql.Driver.begin_transaction connection
    |> Result.map_err ~fn:Mysql.Driver.error_to_string
  in
  let* () =
    Mysql.Driver.rollback connection
    |> Result.map_err ~fn:Mysql.Driver.error_to_string
  in
  Ok ()

let assert_innodb_crud_roundtrips = fun connection ->
  let table_name = "riot_mysql_container_items" in
  let* () = execute_unit connection ("DROP TABLE IF EXISTS " ^ table_name) [] in
  let* () =
    execute_unit
      connection
      ("CREATE TABLE "
      ^ table_name
      ^ " (id BIGINT PRIMARY KEY, name VARCHAR(64) NOT NULL) ENGINE=InnoDB")
      []
  in
  let* result_set =
    execute
      connection
      ("INSERT INTO " ^ table_name ^ " (id, name) VALUES (?, ?)")
      [ Sqlx_driver.Value.int64 7L; Sqlx_driver.Value.string "Ada"; ]
  in
  if Mysql.Driver.rows_affected result_set != 1 then
    Error ("insert expected 1 affected row, got "
    ^ Int.to_string (Mysql.Driver.rows_affected result_set))
  else
    let* row =
      query_one
        connection
        ("SELECT name FROM " ^ table_name ^ " WHERE id = ?")
        [ Sqlx_driver.Value.int64 7L ]
    in
    let* () = expect_string
      ~field:"name"
      ~expected:"Ada"
      (Sqlx_driver.Row.string "name" row)
    in
    execute_unit connection ("DROP TABLE IF EXISTS " ^ table_name) []

let assert_unsigned_bigint_roundtrips = fun connection ->
  let* row =
    query_one
      connection
      "SELECT CAST(18446744073709551615 AS UNSIGNED) AS max_u64, CAST(? AS SIGNED) AS marker"
      [ Sqlx_driver.Value.int 1 ]
  in
  let* () = expect_int
    ~field:"marker"
    ~expected:1
    (row_int_like "marker" row)
  in
  let actual =
    match Sqlx_driver.Row.get "max_u64" row with
    | Some value -> Sqlx_driver.Value.to_numeric value
    | None -> None
  in
  expect_string ~field:"max_u64" ~expected:"18446744073709551615" actual

let assert_text_protocol_scalar_properties = fun connection ->
  let* row =
    query_one
      connection
      "SELECT CAST('-42' AS SIGNED) AS i, CAST('12345.6789' AS DECIMAL(20,4)) AS n, CAST('2026-05-10' AS DATE) AS d, CAST('12:34:56.123456' AS TIME(6)) AS t, CAST('2026-05-10 12:34:56.123456' AS DATETIME(6)) AS dt, NULL AS missing"
      []
  in
  let* () = expect_int
    ~field:"i"
    ~expected:(-42)
    (row_int_like "i" row)
  in
  let* () = expect_string
    ~field:"n"
    ~expected:"12345.6789"
    (row_text_like "n" row)
  in
  let* () =
    expect_date
      ~field:"d"
      ~expected:(2_026, 5, 10)
      (
        match Sqlx_driver.Row.get "d" row with
        | Some value -> Sqlx_driver.Value.to_date value
        | None -> None
      )
  in
  let* () =
    expect_time
      ~field:"t"
      ~expected:(12, 34, 56, 123_456)
      (
        match Sqlx_driver.Row.get "t" row with
        | Some value -> Sqlx_driver.Value.to_time value
        | None -> None
      )
  in
  let* () =
    expect_timestamp_fields
      ~field:"dt"
      ~expected:(timestamp
        ~year:2_026
        ~month:5
        ~day:10
        ~hour:12
        ~minute:34
        ~second:56
        ~microsecond:123_456)
      (Sqlx_driver.Row.timestamp "dt" row)
  in
  expect_null ~field:"missing" row

let assert_prepared_integer_properties = fun connection ->
  run_cases
    [
      ("zero", 0);
      ("one", 1);
      ("negative one", (-1));
      ("short min", (-32_768));
      ("short max", 32_767);
      ("large positive", 2_147_483_647);
      ("large negative", (-2_147_483_648));
    ]
    ~fn:(fun _name value ->
      let* row =
        query_one
          connection
          "SELECT CAST(? AS SIGNED) AS value"
          [ Sqlx_driver.Value.int64 (Int64.from_int value) ]
      in
      expect_int
        ~field:"value"
        ~expected:value
        (row_int_like "value" row))

let assert_prepared_string_properties = fun connection ->
  run_cases
    [
      ("empty", "");
      ("ascii", "hello mysql");
      ("quotes", "single ' quote and double \" quote");
      ("punctuation", "symbols !@#$%^&*()[]{}");
      ("newline", "line one\nline two");
    ]
    ~fn:(fun _name value ->
      let* row = query_one connection "SELECT ? AS value" [ Sqlx_driver.Value.string value ] in
      expect_string
        ~field:"value"
        ~expected:value
        (Sqlx_driver.Row.string "value" row))

let assert_prepared_float_properties = fun connection ->
  run_cases
    [ ("zero", 0.0); ("one quarter", 0.25); ("negative", (-3.5)); ("large", 12_345.5); ]
    ~fn:(fun _name value ->
      let* row =
        query_one connection "SELECT CAST(? AS DOUBLE) AS value" [ Sqlx_driver.Value.float value ]
      in
      expect_float
        ~field:"value"
        ~expected:value
        (Sqlx_driver.Row.float "value" row))

let assert_prepared_decimal_properties = fun connection ->
  run_cases
    [
      ("zero", ("0.0000", "0.0000"));
      ("positive", ("12345.6789", "12345.6789"));
      ("negative", ("-99.1250", "-99.1250"));
    ]
    ~fn:(fun _name (input, expected) ->
      let* row =
        query_one
          connection
          "SELECT CAST(? AS DECIMAL(20,4)) AS value"
          [ Sqlx_driver.Value.numeric input ]
      in
      expect_string
        ~field:"value"
        ~expected
        (row_text_like "value" row))

let assert_prepared_temporal_properties = fun connection ->
  let expected_timestamp =
    timestamp ~year:2_026 ~month:5 ~day:10 ~hour:12 ~minute:34 ~second:56 ~microsecond:123_456
  in
  let* row =
    query_one
      connection
      "SELECT CAST(? AS DATE) AS d, CAST(? AS TIME(6)) AS t, CAST(? AS DATETIME(6)) AS dt"
      [
        Sqlx_driver.Value.date 2_026 5 10;
        Sqlx_driver.Value.time 26 3 4 500_000;
        Sqlx_driver.Value.timestamp expected_timestamp;
      ]
  in
  let* () =
    expect_date
      ~field:"d"
      ~expected:(2_026, 5, 10)
      (
        match Sqlx_driver.Row.get "d" row with
        | Some value -> Sqlx_driver.Value.to_date value
        | None -> None
      )
  in
  let* () =
    expect_time
      ~field:"t"
      ~expected:(26, 3, 4, 500_000)
      (
        match Sqlx_driver.Row.get "t" row with
        | Some value -> Sqlx_driver.Value.to_time value
        | None -> None
      )
  in
  expect_timestamp_fields
    ~field:"dt"
    ~expected:expected_timestamp
    (Sqlx_driver.Row.timestamp "dt" row)

let assert_null_binding_properties = fun connection ->
  let* row =
    query_one
      connection
      "SELECT ? AS missing, COALESCE(?, ?) AS fallback"
      [ Sqlx_driver.Value.null; Sqlx_driver.Value.null; Sqlx_driver.Value.string "fallback"; ]
  in
  let* () = expect_null ~field:"missing" row in
  expect_string
    ~field:"fallback"
    ~expected:"fallback"
    (Sqlx_driver.Row.string "fallback" row)

let assert_innodb_operation_properties = fun connection ->
  let table_name = "riot_mysql_property_items" in
  let* () = execute_unit connection ("DROP TABLE IF EXISTS " ^ table_name) [] in
  let* () =
    execute_unit
      connection
      ("CREATE TABLE "
      ^ table_name
      ^ " (id BIGINT PRIMARY KEY, name VARCHAR(128) NOT NULL, payload BLOB NOT NULL, amount DECIMAL(20,4) NOT NULL, active TINYINT(1) NOT NULL, created_on DATE NOT NULL) ENGINE=InnoDB")
      []
  in
  let rows = [
    ("row 1", (1L, "Ada", "payload-one", "10.2500", 1, (2_026, 5, 10)));
    ("row 2", (2L, "Grace", "\x00binary\xff", "20.5000", 0, (2_026, 5, 11)));
    ("row 3", (3L, "Edsger", "payload-three", "-7.1250", 1, (2_026, 5, 12)));
  ]
  in
  let* () =
    run_cases
      rows
      ~fn:(fun _name (id, name, payload, amount, active, (year, month, day)) ->
        execute_unit
          connection
          ("INSERT INTO "
          ^ table_name
          ^ " (id, name, payload, amount, active, created_on) VALUES (?, ?, ?, ?, ?, ?)")
          [
            Sqlx_driver.Value.int64 id;
            Sqlx_driver.Value.string name;
            Sqlx_driver.Value.bytes (Bytes.from_string payload);
            Sqlx_driver.Value.numeric amount;
            Sqlx_driver.Value.int active;
            Sqlx_driver.Value.date year month day;
          ])
  in
  let* row = query_one connection ("SELECT COUNT(*) AS n FROM " ^ table_name) [] in
  let* () = expect_int
    ~field:"n"
    ~expected:3
    (row_int_like "n" row)
  in
  let* () =
    execute_unit
      connection
      ("UPDATE " ^ table_name ^ " SET name = ?, amount = amount + 1 WHERE id = ?")
      [ Sqlx_driver.Value.string "Ada Lovelace"; Sqlx_driver.Value.int64 1L ]
  in
  let* row =
    query_one
      connection
      ("SELECT name, payload, amount, active, created_on FROM " ^ table_name ^ " WHERE id = ?")
      [ Sqlx_driver.Value.int64 1L ]
  in
  let* () = expect_string
    ~field:"name"
    ~expected:"Ada Lovelace"
    (Sqlx_driver.Row.string "name" row)
  in
  let* () =
    expect_bytes
      ~field:"payload"
      ~expected:"payload-one"
      (Sqlx_driver.Row.bytes "payload" row)
  in
  let* () = expect_string
    ~field:"amount"
    ~expected:"11.2500"
    (row_text_like "amount" row)
  in
  let* () = expect_int
    ~field:"active"
    ~expected:1
    (row_int_like "active" row)
  in
  let* () =
    expect_date
      ~field:"created_on"
      ~expected:(2_026, 5, 10)
      (
        match Sqlx_driver.Row.get "created_on" row with
        | Some value -> Sqlx_driver.Value.to_date value
        | None -> None
      )
  in
  let* () =
    execute_unit
      connection
      ("DELETE FROM " ^ table_name ^ " WHERE id = ?")
      [ Sqlx_driver.Value.int64 2L ]
  in
  let* row = query_one connection ("SELECT COUNT(*) AS n FROM " ^ table_name) [] in
  let* () = expect_int
    ~field:"n"
    ~expected:2
    (row_int_like "n" row)
  in
  let* () =
    Mysql.Driver.begin_transaction connection
    |> Result.map_err ~fn:Mysql.Driver.error_to_string
  in
  let* () =
    execute_unit
      connection
      ("INSERT INTO "
      ^ table_name
      ^ " (id, name, payload, amount, active, created_on) VALUES (?, ?, ?, ?, ?, ?)")
      [
        Sqlx_driver.Value.int64 9L;
        Sqlx_driver.Value.string "Rollback";
        Sqlx_driver.Value.bytes (Bytes.from_string "rolled-back");
        Sqlx_driver.Value.numeric "1.0000";
        Sqlx_driver.Value.int 1;
        Sqlx_driver.Value.date 2_026 5 13;
      ]
  in
  let* () =
    Mysql.Driver.rollback connection
    |> Result.map_err ~fn:Mysql.Driver.error_to_string
  in
  let* row = query_one connection ("SELECT COUNT(*) AS n FROM " ^ table_name ^ " WHERE id = 9") [] in
  let* () = expect_int
    ~field:"n"
    ~expected:0
    (row_int_like "n" row)
  in
  execute_unit connection ("DROP TABLE IF EXISTS " ^ table_name) []

let assert_live_property_battery = fun connection ->
  let* () = assert_text_protocol_scalar_properties connection in
  let* () = assert_prepared_integer_properties connection in
  let* () = assert_prepared_string_properties connection in
  let* () = assert_prepared_float_properties connection in
  let* () = assert_prepared_decimal_properties connection in
  let* () = assert_prepared_temporal_properties connection in
  let* () = assert_null_binding_properties connection in
  assert_innodb_operation_properties connection

let test_parameterized_query_roundtrips = fun _ctx url ->
  with_connection
    url
    assert_parameterized_query_roundtrips

let test_transaction_roundtrips = fun _ctx url -> with_connection url assert_transaction_roundtrips

let path_exists = fun path ->
  match Fs.exists path with
  | Ok true -> true
  | Ok false
  | Error _ -> false

let local_docker_socket_available = fun () ->
  let home = Env.home_dir () in
  let from_home suffix =
    match home with
    | None -> None
    | Some home -> Some Path.(home / Path.v suffix)
  in
  let runtime =
    Env.var Env.String ~name:"XDG_RUNTIME_DIR"
    |> Option.map ~fn:(fun dir -> Path.(Path.v dir / Path.v ".docker/run/docker.sock"))
  in
  List.any
    [
      Some (Path.v "/var/run/docker.sock");
      runtime;
      from_home ".docker/run/docker.sock";
      from_home ".docker/desktop/docker.sock";
    ]
    ~fn:(fun candidate ->
      match candidate with
      | Some path -> path_exists path
      | None -> false)

let docker_endpoint_available = fun () ->
  match Env.var Env.String ~name:"DOCKER_HOST" with
  | Some raw -> (
      let raw = String.trim raw in
      if String.equal raw "" then
        local_docker_socket_available ()
      else if String.starts_with ~prefix:"tcp://" raw then
        true
      else if String.starts_with ~prefix:"unix://" raw then
        let path =
          String.sub
            raw
            ~offset:(String.length "unix://")
            ~len:(String.length raw - String.length "unix://")
        in
        path_exists (Path.v path)
      else
        false
    )
  | None -> local_docker_socket_available ()

let mysql_container_image = fun () ->
  Testcontainers.Generic_image.(make "mysql" "8.0"
  |> with_cmd ~cmd:[ "--default-authentication-plugin=mysql_native_password" ]
  |> with_env_var ~name:"MYSQL_DATABASE" ~value:mysql_container_database
  |> with_env_var ~name:"MYSQL_USER" ~value:mysql_container_user
  |> with_env_var ~name:"MYSQL_PASSWORD" ~value:mysql_container_password
  |> with_env_var ~name:"MYSQL_ROOT_PASSWORD" ~value:"riot-root"
  |> with_exposed_port ~port:mysql_container_port
  |> with_readiness_policy
    ~policy:(ReadinessPolicy.log ~message:"port: 3306" ~duration:(Duration.of_secs 90) ~retry:180)
  |> with_readiness_policy ~policy:(ReadinessPolicy.delay ~duration:(Duration.of_secs 2)))

let container_config = fun container ->
  match Testcontainers.Container.host_port container ~port:mysql_container_port with
  | Error error -> Error (Testcontainers.error_to_string error)
  | Ok addr ->
      Ok {
        (Mysql.Config.default ()) with
        host = Net.Addr.ip addr;
        port = Net.Addr.port addr;
        database = Some mysql_container_database;
        user = mysql_container_user;
        password = mysql_container_password;
        ssl_mode = Mysql.Config.Disable;
        connect_timeout = Time.Duration.from_secs 30;
      }

let with_mysql_container = fun fn ->
  Testcontainers.with_container
    (mysql_container_image ())
    (fun container ->
      match container_config container with
      | Error message -> Error (Testcontainers.StartupTimeout message)
      | Ok config -> (
          match with_connection_config config fn with
          | Ok value -> Ok value
          | Error message -> Error (Testcontainers.StartupTimeout message)
        ))
  |> Result.map_err ~fn:Testcontainers.error_to_string

let test_mysql_container_roundtrips = fun _ctx ->
  with_mysql_container
    (fun connection ->
      let* () = assert_parameterized_query_roundtrips connection in
      let* () = assert_transaction_roundtrips connection in
      let* () = assert_innodb_crud_roundtrips connection in
      let* () = assert_unsigned_bigint_roundtrips connection in
      assert_live_property_battery connection)

let live_case = fun name fn ->
  match mysql_url () with
  | Error error -> Test.case ~size:Large name (fun _ctx -> Error error)
  | Ok None -> Test.skip ~size:Large name (fun _ctx -> Ok ())
  | Ok (Some url) -> Test.case ~size:Large name (fun ctx -> fn ctx url)

let live_case_from_env = fun env name fn ->
  match mysql_url_from_env env with
  | Error error -> Test.case ~size:Large name (fun _ctx -> Error error)
  | Ok None -> Test.skip ~size:Large name (fun _ctx -> Ok ())
  | Ok (Some url) -> Test.case ~size:Large name (fun ctx -> fn ctx url)

let container_property_case = fun name ~examples fn ->
  if docker_endpoint_available () then
    Test.property ~size:Large name ~examples fn
  else
    Test.skip ~size:Large name (fun _ctx -> Ok ())

let tests =
  Test.[
    live_case "parameterized query roundtrips" test_parameterized_query_roundtrips;
    live_case "transaction roundtrips" test_transaction_roundtrips;
    live_case_from_env
      "MYSQL_TEST_TLS_URL"
      "tls parameterized query roundtrips"
      test_parameterized_query_roundtrips;
    live_case_from_env
      "MYSQL_TEST_NATIVE_PASSWORD_URL"
      "mysql_native_password query roundtrips"
      test_parameterized_query_roundtrips;
    container_property_case
      "property: mysql container values operations and transactions"
      ~examples:48
      test_mysql_container_roundtrips;
  ]

let main ~args = Test.Cli.main ~name:"mysql_tests" ~tests ~args ()

let () = Std.Runtime.run ~main ~args:Std.Env.args ()
