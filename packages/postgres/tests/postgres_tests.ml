open Std
open Result.Syntax

module Arbitrary = Propane.Arbitrary
module Bytes = Std.IO.Bytes
module Generator = Propane.Generator
module Property = Propane.Property
module Row = Sqlx_driver.Row
module Value = Sqlx_driver.Value

let postgres_container_port = 5_432

let postgres_container_user = "riot"

let postgres_container_password = "riot"

let postgres_container_database = "riot_test"

let property_examples = 72

let property_config = {
  Property.default_config with
  test_count = property_examples;
  max_size = 64;
  seed = Some 5_432;
}

let postgres_url () =
  match Dotenv.load_if_exists ~env:"test" () with
  | Error error -> Error (Dotenv.error_to_string error)
  | Ok _bindings -> (
      match Env.get Env.String ~var:"POSTGRES_TEST_URL" with
      | Some url -> Ok (Some url)
      | None ->
          match Env.get Env.String ~var:"MULE_TEST_POSTGRES_URL" with
          | Some url -> Ok (Some url)
          | None ->
              match Env.get Env.String ~var:"HYPEKIT_POSTGRES_URL" with
              | Some url -> Ok (Some url)
              | None -> Ok (Env.get Env.String ~var:"SURI_JOBS_TEST_POSTGRES_URL")
    )

let with_connection_config config fn =
  match Postgres.Driver.connect config with
  | Error error -> Error (Postgres.Driver.error_to_string error)
  | Ok connection ->
      let result = fn connection in
      Postgres.Driver.close connection;
      result

let with_connection url fn =
  match Postgres.Config.from_string url with
  | Error error ->
      Error ("invalid postgres test url: " ^ Postgres.Config.parse_error_to_string error)
  | Ok config -> with_connection_config config fn

let execute connection sql params =
  let* statement =
    Postgres.Driver.prepare connection sql
    |> Result.map_err ~fn:Postgres.Driver.error_to_string
  in
  let* result_set =
    Postgres.Driver.execute statement params
    |> Result.map_err ~fn:Postgres.Driver.error_to_string
  in
  Ok result_set

let execute_unit connection sql params =
  let* _result_set = execute connection sql params in
  Ok ()

let query_one connection sql params =
  let* result_set = execute connection sql params in
  match Postgres.Driver.fetch_row result_set with
  | Some row -> Ok row
  | None -> Error "expected one row"

let query_optional connection sql params =
  let* result_set = execute connection sql params in
  Ok (Postgres.Driver.fetch_row result_set)

let option_to_string render value =
  match value with
  | Some value -> "Some(" ^ render value ^ ")"
  | None -> "None"

let expect_option = fun ~field ~expected ~actual ~equal ~render ->
  if Option.equal actual (Some expected) ~fn:equal then
    Ok ()
  else
    Error ("field "
    ^ field
    ^ " expected "
    ^ option_to_string render (Some expected)
    ^ ", got "
    ^ option_to_string render actual)

let expect_string = fun ~field ~expected actual ->
  expect_option
    ~field
    ~expected
    ~actual
    ~equal:String.equal
    ~render:(fun value -> value)

let expect_bool = fun ~field ~expected actual ->
  expect_option
    ~field
    ~expected
    ~actual
    ~equal:Bool.equal
    ~render:Bool.to_string

let expect_int = fun ~field ~expected actual ->
  expect_option
    ~field
    ~expected
    ~actual
    ~equal:Int.equal
    ~render:Int.to_string

let expect_int64 = fun ~field ~expected actual ->
  expect_option
    ~field
    ~expected
    ~actual
    ~equal:Int64.equal
    ~render:Int64.to_string

let expect_float = fun ~field ~expected actual ->
  match actual with
  | Some actual when Float.abs (actual -. expected) <= 0.000_001 -> Ok ()
  | _ ->
      Error ("field "
      ^ field
      ^ " expected approximately "
      ^ Float.to_string expected
      ^ ", got "
      ^ option_to_string Float.to_string actual)

let expect_date = fun ~field ~expected actual ->
  expect_option
    ~field
    ~expected
    ~actual
    ~equal:(fun left right -> left = right)
    ~render:(fun (year, month, day) ->
      Int.to_string year ^ "-" ^ Int.to_string month ^ "-" ^ Int.to_string day)

let expect_time = fun ~field ~expected actual ->
  expect_option
    ~field
    ~expected
    ~actual
    ~equal:(fun left right -> left = right)
    ~render:(fun (hour, minute, second, micros) ->
      Int.to_string hour
      ^ ":"
      ^ Int.to_string minute
      ^ ":"
      ^ Int.to_string second
      ^ "."
      ^ Int.to_string micros)

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
  match Row.get field row with
  | Some Value.Null -> Ok ()
  | Some value -> Error ("field " ^ field ^ " expected NULL, got " ^ Value.to_string value)
  | None -> Error ("field " ^ field ^ " expected NULL, got missing field")

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

let expect_timestamp = fun ~field ~expected actual ->
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
  match Row.get field row with
  | None -> None
  | Some value -> (
      match Value.to_int value with
      | Some value -> Some value
      | None -> (
          match Value.to_int16 value with
          | Some value -> Some value
          | None -> (
              match Value.to_int64 value with
              | Some value -> Some (Int64.to_int value)
              | None -> None
            )
        )
    )

let row_int64_like = fun field row ->
  match Row.get field row with
  | None -> None
  | Some value -> (
      match Value.to_int64 value with
      | Some value -> Some value
      | None -> (
          match Value.to_int value with
          | Some value -> Some (Int64.from_int value)
          | None -> (
              match Value.to_int16 value with
              | Some value -> Some (Int64.from_int value)
              | None -> None
            )
        )
    )

let value_date = fun field row ->
  match Row.get field row with
  | Some value -> Value.to_date value
  | None -> None

let value_time = fun field row ->
  match Row.get field row with
  | Some value -> Value.to_time value
  | None -> None

let value_numeric = fun field row ->
  match Row.get field row with
  | Some value -> Value.to_numeric value
  | None -> None

let value_uuid = fun field row ->
  match Row.get field row with
  | Some value -> Value.to_uuid value
  | None -> None

let value_json = fun field row ->
  match Row.get field row with
  | Some value -> Value.to_json value
  | None -> None

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
      "select $1::text as value, $2::int4 as n"
      [ Value.string "hello"; Value.int 42 ]
  in
  let* () = expect_string
    ~field:"value"
    ~expected:"hello"
    (Row.string "value" row)
  in
  expect_int
    ~field:"n"
    ~expected:42
    (Row.int "n" row)

let assert_large_text_roundtrips = fun connection ->
  let body = String.make ~len:20_000 ~char:'x' in
  let* row =
    query_one
      connection
      "select $1::text as body, length($1::text)::int4 as len"
      [ Value.string body ]
  in
  let* () = expect_string
    ~field:"body"
    ~expected:body
    (Row.string "body" row)
  in
  expect_int
    ~field:"len"
    ~expected:20_000
    (Row.int "len" row)

let assert_transaction_roundtrips = fun connection ->
  let* () =
    Postgres.Driver.begin_transaction connection
    |> Result.map_err ~fn:Postgres.Driver.error_to_string
  in
  let* () =
    Postgres.Driver.rollback connection
    |> Result.map_err ~fn:Postgres.Driver.error_to_string
  in
  Ok ()

let assert_text_protocol_scalar_roundtrips = fun connection ->
  let expected_timestamp =
    timestamp ~year:2_026 ~month:5 ~day:10 ~hour:12 ~minute:34 ~second:56 ~microsecond:123_456
  in
  let* row =
    query_one
      connection
      "select true as b, false as f, '-12'::int2 as i2, '42'::int4 as i4, '9000000000'::int8 as i8, '12.5'::float8 as f8, '12345.6789'::numeric as n, '2026-05-10'::date as d, '12:34:56.123456'::time(6) as t, '2026-05-10 12:34:56.123456'::timestamp as ts, '2026-05-10 12:34:56.123456+00'::timestamptz as tstz, '550e8400-e29b-41d4-a716-446655440000'::uuid as uuid, '{\"ok\":true}'::json as json, decode('000102ff', 'hex')::bytea as bytes, null::text as missing"
      []
  in
  let* () = expect_bool
    ~field:"b"
    ~expected:true
    (Row.bool "b" row)
  in
  let* () = expect_bool
    ~field:"f"
    ~expected:false
    (Row.bool "f" row)
  in
  let* () = expect_int
    ~field:"i2"
    ~expected:(-12)
    (row_int_like "i2" row)
  in
  let* () = expect_int
    ~field:"i4"
    ~expected:42
    (row_int_like "i4" row)
  in
  let* () = expect_int64
    ~field:"i8"
    ~expected:9_000_000_000L
    (row_int64_like "i8" row)
  in
  let* () = expect_float
    ~field:"f8"
    ~expected:12.5
    (Row.float "f8" row)
  in
  let* () = expect_string
    ~field:"n"
    ~expected:"12345.6789"
    (value_numeric "n" row)
  in
  let* () = expect_date
    ~field:"d"
    ~expected:(2_026, 5, 10)
    (value_date "d" row)
  in
  let* () = expect_time
    ~field:"t"
    ~expected:(12, 34, 56, 123_456)
    (value_time "t" row)
  in
  let* () = expect_timestamp
    ~field:"ts"
    ~expected:expected_timestamp
    (Row.timestamp "ts" row)
  in
  let* () =
    expect_timestamp
      ~field:"tstz"
      ~expected:expected_timestamp
      (
        match Row.get "tstz" row with
        | Some value -> Value.to_timestamp_with_timezone value
        | None -> None
      )
  in
  let* () =
    expect_string
      ~field:"uuid"
      ~expected:"550e8400-e29b-41d4-a716-446655440000"
      (value_uuid "uuid" row)
  in
  let* () = expect_string
    ~field:"json"
    ~expected:"{\"ok\":true}"
    (value_json "json" row)
  in
  let* () = expect_bytes
    ~field:"bytes"
    ~expected:"\x00\x01\x02\xff"
    (Row.bytes "bytes" row)
  in
  expect_null ~field:"missing" row

let assert_crud_roundtrips = fun connection ->
  let table_name = "riot_postgres_container_items" in
  let* () = execute_unit connection ("drop table if exists " ^ table_name) [] in
  let* () =
    execute_unit
      connection
      ("create table "
      ^ table_name
      ^ " (id bigint primary key, name text not null, payload bytea not null, amount numeric(20,4) not null, active boolean not null, created_on date not null)")
      []
  in
  let* result_set =
    execute
      connection
      ("insert into "
      ^ table_name
      ^ " (id, name, payload, amount, active, created_on) values ($1, $2, $3, $4, $5, $6)")
      [
        Value.int64 7L;
        Value.string "Ada";
        Value.bytes (Bytes.from_string "payload-one");
        Value.numeric "10.2500";
        Value.bool true;
        Value.date 2_026 5 10;
      ]
  in
  if not (Int.equal (Postgres.Driver.rows_affected result_set) 1) then
    Error ("insert expected 1 affected row, got "
    ^ Int.to_string (Postgres.Driver.rows_affected result_set))
  else
    let* () =
      execute_unit
        connection
        ("update " ^ table_name ^ " set name = $1, amount = amount + 1 where id = $2")
        [ Value.string "Ada Lovelace"; Value.int64 7L ]
    in
    let* row =
      query_one
        connection
        ("select name, payload, amount, active, created_on from " ^ table_name ^ " where id = $1")
        [ Value.int64 7L ]
    in
    let* () = expect_string
      ~field:"name"
      ~expected:"Ada Lovelace"
      (Row.string "name" row)
    in
    let* () = expect_bytes
      ~field:"payload"
      ~expected:"payload-one"
      (Row.bytes "payload" row)
    in
    let* () = expect_string
      ~field:"amount"
      ~expected:"11.2500"
      (value_numeric "amount" row)
    in
    let* () = expect_bool
      ~field:"active"
      ~expected:true
      (Row.bool "active" row)
    in
    let* () = expect_date
      ~field:"created_on"
      ~expected:(2_026, 5, 10)
      (value_date "created_on" row)
    in
    let* () =
      execute_unit connection ("delete from " ^ table_name ^ " where id = $1") [ Value.int64 7L ]
    in
    let* row = query_one connection ("select count(*) as n from " ^ table_name) [] in
    let* () = expect_int
      ~field:"n"
      ~expected:0
      (row_int_like "n" row)
    in
    execute_unit connection ("drop table if exists " ^ table_name) []

type scalar_case =
  | CaseText of string
  | CaseBool of bool
  | CaseInt2 of int
  | CaseInt4 of int
  | CaseInt8 of int64
  | CaseFloat8 of float
  | CaseNumeric of string
  | CaseDate of int * int * int
  | CaseTime of int * int * int * int
  | CaseTimestamp of DateTime.t
  | CaseBytes of string
  | CaseJson of string
  | CaseNullText

let pad_fixed = fun width value ->
  let raw = Int.to_string (Int.abs value) in
  String.make ~len:(max 0 (width - String.length raw)) ~char:'0' ^ raw

let decimal_string = fun (whole, frac) ->
  let sign =
    if whole < 0 then
      "-"
    else
      ""
  in
  sign ^ Int.to_string (Int.abs whole) ^ "." ^ pad_fixed 4 frac

let json_string = fun value -> "{\"n\":" ^ Int.to_string value ^ "}"

let bytes_from_codes = fun codes ->
  let bytes = Bytes.create ~size:(List.length codes) in
  List.enumerate codes
  |> List.for_each
    ~fn:(fun (index, code) ->
      Bytes.set_unchecked
        bytes
        ~at:index
        ~char:(Char.from_int_unchecked code));
  Bytes.to_string bytes

let scalar_case_to_string = fun value ->
  match value with
  | CaseText value -> "text(" ^ String.escaped value ^ ")"
  | CaseBool value -> "bool(" ^ Bool.to_string value ^ ")"
  | CaseInt2 value -> "int2(" ^ Int.to_string value ^ ")"
  | CaseInt4 value -> "int4(" ^ Int.to_string value ^ ")"
  | CaseInt8 value -> "int8(" ^ Int64.to_string value ^ ")"
  | CaseFloat8 value -> "float8(" ^ Float.to_string value ^ ")"
  | CaseNumeric value -> "numeric(" ^ value ^ ")"
  | CaseDate (year, month, day) ->
      "date(" ^ Int.to_string year ^ "-" ^ Int.to_string month ^ "-" ^ Int.to_string day ^ ")"
  | CaseTime (hour, minute, second, micros) ->
      "time("
      ^ Int.to_string hour
      ^ ":"
      ^ Int.to_string minute
      ^ ":"
      ^ Int.to_string second
      ^ "."
      ^ Int.to_string micros
      ^ ")"
  | CaseTimestamp value -> "timestamp(" ^ DateTime.to_iso8601 value ^ ")"
  | CaseBytes value -> "bytea(" ^ Encoding.Base16.encode value ^ ")"
  | CaseJson value -> "json(" ^ value ^ ")"
  | CaseNullText -> "null(text)"

let ascii_text_gen = Generator.string_size (Generator.int_range 0 96) Generator.char_printable

let bytes_gen =
  Generator.map
    bytes_from_codes
    (Generator.list_size (Generator.int_range 0 48) (Generator.int_range 0 255))

let date_gen =
  Generator.map3
    (fun year month day -> (year, month, day))
    (Generator.int_range 1_970 2_038)
    (Generator.int_range 1 12)
    (Generator.int_range 1 28)

let time_gen =
  Generator.quad
    (Generator.int_range 0 23)
    (Generator.int_range 0 59)
    (Generator.int_range 0 59)
    (Generator.int_range 0 999_999)

let timestamp_gen =
  Generator.map2
    (fun (year, month, day) (hour, minute, second, micros) ->
      timestamp
        ~year
        ~month
        ~day
        ~hour
        ~minute
        ~second
        ~microsecond:micros)
    date_gen
    time_gen

let scalar_case_gen =
  Generator.frequency
    [
      (8, Generator.map (fun value -> CaseText value) ascii_text_gen);
      (4, Generator.map (fun value -> CaseBool value) Generator.bool);
      (4, Generator.map (fun value -> CaseInt2 value) (Generator.int_range (-32_768) 32_767));
      (
        4,
        Generator.map
          (fun value -> CaseInt4 value)
          (Generator.int_range (-2_147_483_648) 2_147_483_647)
      );
      (
        4,
        Generator.map
          (fun value -> CaseInt8 value)
          (Generator.int64_range (-9_000_000_000_000L) 9_000_000_000_000L)
      );
      (
        4,
        Generator.map
          (fun value -> CaseFloat8 value)
          (Generator.float_range (-1_000_000.0) 1_000_000.0)
      );
      (
        4,
        Generator.map
          (fun value -> CaseNumeric (decimal_string value))
          (Generator.pair (Generator.int_range (-1_000_000) 1_000_000) (Generator.int_range 0 9_999))
      );
      (4, Generator.map (fun (year, month, day) -> CaseDate (year, month, day)) date_gen);
      (
        4,
        Generator.map
          (fun (hour, minute, second, micros) -> CaseTime (hour, minute, second, micros))
          time_gen
      );
      (4, Generator.map (fun value -> CaseTimestamp value) timestamp_gen);
      (4, Generator.map (fun value -> CaseBytes value) bytes_gen);
      (
        4,
        Generator.map
          (fun value -> CaseJson (json_string value))
          (Generator.int_range (-10_000) 10_000)
      );
      (1, Generator.return CaseNullText);
    ]

let scalar_case_arb = Arbitrary.make ~print:scalar_case_to_string scalar_case_gen

let assert_scalar_case = fun connection value ->
  match value with
  | CaseText expected ->
      let* row = query_one connection "select $1::text as value" [ Value.string expected ] in
      expect_string
        ~field:"value"
        ~expected
        (Row.string "value" row)
  | CaseBool expected ->
      let* row = query_one connection "select $1::boolean as value" [ Value.bool expected ] in
      expect_bool
        ~field:"value"
        ~expected
        (Row.bool "value" row)
  | CaseInt2 expected ->
      let* row = query_one connection "select $1::int2 as value" [ Value.int16 expected ] in
      expect_int
        ~field:"value"
        ~expected
        (row_int_like "value" row)
  | CaseInt4 expected ->
      let* row = query_one connection "select $1::int4 as value" [ Value.int expected ] in
      expect_int
        ~field:"value"
        ~expected
        (row_int_like "value" row)
  | CaseInt8 expected ->
      let* row = query_one connection "select $1::int8 as value" [ Value.int64 expected ] in
      expect_int64
        ~field:"value"
        ~expected
        (row_int64_like "value" row)
  | CaseFloat8 expected ->
      let* row = query_one connection "select $1::float8 as value" [ Value.float expected ] in
      expect_float
        ~field:"value"
        ~expected
        (Row.float "value" row)
  | CaseNumeric expected ->
      let* row = query_one connection "select $1::numeric(20,4) as value" [ Value.numeric expected ] in
      expect_string
        ~field:"value"
        ~expected
        (value_numeric "value" row)
  | CaseDate (year, month, day) ->
      let* row = query_one connection "select $1::date as value" [ Value.date year month day ] in
      expect_date
        ~field:"value"
        ~expected:(year, month, day)
        (value_date "value" row)
  | CaseTime (hour, minute, second, micros) ->
      let* row =
        query_one connection "select $1::time(6) as value" [ Value.time hour minute second micros ]
      in
      expect_time
        ~field:"value"
        ~expected:(hour, minute, second, micros)
        (value_time "value" row)
  | CaseTimestamp expected ->
      let* row = query_one connection "select $1::timestamp as value" [ Value.timestamp expected ] in
      expect_timestamp
        ~field:"value"
        ~expected
        (Row.timestamp "value" row)
  | CaseBytes expected ->
      let* row =
        query_one
          connection
          "select $1::bytea as value"
          [ Value.bytes (Bytes.from_string expected) ]
      in
      expect_bytes
        ~field:"value"
        ~expected
        (Row.bytes "value" row)
  | CaseJson expected ->
      let* row = query_one connection "select $1::json as value" [ Value.json expected ] in
      expect_string
        ~field:"value"
        ~expected
        (value_json "value" row)
  | CaseNullText ->
      let* row = query_one connection "select $1::text as value" [ Value.null ] in
      expect_null ~field:"value" row

type item = {
  id: int;
  name: string;
  payload: string;
  amount: string;
  active: bool;
  created_on: int * int * int;
}

type operation =
  | Put of item
  | Rename of int * string
  | SetActive of int * bool
  | Delete of int
  | Read of int

let item_to_string = fun item ->
  "{id="
  ^ Int.to_string item.id
  ^ ";name="
  ^ String.escaped item.name
  ^ ";payload="
  ^ Encoding.Base16.encode item.payload
  ^ ";amount="
  ^ item.amount
  ^ "}"

let operation_to_string = fun operation ->
  match operation with
  | Put item -> "put(" ^ item_to_string item ^ ")"
  | Rename (id, name) -> "rename(" ^ Int.to_string id ^ "," ^ String.escaped name ^ ")"
  | SetActive (id, active) -> "set_active(" ^ Int.to_string id ^ "," ^ Bool.to_string active ^ ")"
  | Delete id -> "delete(" ^ Int.to_string id ^ ")"
  | Read id -> "read(" ^ Int.to_string id ^ ")"

let workload_to_string = fun operations ->
  "[" ^ String.concat "; " (List.map operations ~fn:operation_to_string) ^ "]"

let item_gen =
  let name_gen = Generator.string_size (Generator.int_range 0 32) Generator.char_printable in
  Generator.map3
    (fun (id, name) (payload, amount) (active, created_on) ->
      {
        id;
        name;
        payload;
        amount;
        active;
        created_on;
      })
    (Generator.pair (Generator.int_range 0 12) name_gen)
    (Generator.pair
      bytes_gen
      (Generator.map
        decimal_string
        (Generator.pair (Generator.int_range (-10_000) 10_000) (Generator.int_range 0 9_999))))
    (Generator.pair Generator.bool date_gen)

let operation_gen =
  let id_gen = Generator.int_range 0 12 in
  let name_gen = Generator.string_size (Generator.int_range 0 32) Generator.char_printable in
  Generator.frequency
    [
      (5, Generator.map (fun item -> Put item) item_gen);
      (3, Generator.map2 (fun id name -> Rename (id, name)) id_gen name_gen);
      (3, Generator.map2 (fun id active -> SetActive (id, active)) id_gen Generator.bool);
      (2, Generator.map (fun id -> Delete id) id_gen);
      (3, Generator.map (fun id -> Read id) id_gen);
    ]

let workload_arb =
  Arbitrary.make
    ~print:workload_to_string
    (Generator.list_size (Generator.int_range 1 28) operation_gen)

let model_find = fun model id -> List.find model ~fn:(fun item -> Int.equal item.id id)

let model_put = fun model item ->
  if List.any model ~fn:(fun current -> Int.equal current.id item.id) then
    List.map
      model
      ~fn:(fun current ->
        if Int.equal current.id item.id then
          item
        else
          current)
  else
    item :: model

let model_rename = fun model id name ->
  List.map
    model
    ~fn:(fun item ->
      if Int.equal item.id id then
        { item with name }
      else
        item)

let model_set_active = fun model id active ->
  List.map
    model
    ~fn:(fun item ->
      if Int.equal item.id id then
        { item with active }
      else
        item)

let model_delete = fun model id -> List.filter model ~fn:(fun item -> not (Int.equal item.id id))

let assert_db_item = fun connection table_name id expected ->
  let* actual =
    query_optional
      connection
      ("select id, name, payload, amount, active, created_on from " ^ table_name ^ " where id = $1")
      [ Value.int id ]
  in
  match (expected, actual) with
  | (None, None) -> Ok ()
  | (None, Some _) -> Error ("expected id " ^ Int.to_string id ^ " to be absent")
  | (Some expected, None) -> Error ("expected id " ^ Int.to_string id ^ " to be present")
  | (Some expected, Some row) ->
      let* () = expect_int
        ~field:"id"
        ~expected:expected.id
        (row_int_like "id" row)
      in
      let* () = expect_string
        ~field:"name"
        ~expected:expected.name
        (Row.string "name" row)
      in
      let* () = expect_bytes
        ~field:"payload"
        ~expected:expected.payload
        (Row.bytes "payload" row)
      in
      let* () = expect_string
        ~field:"amount"
        ~expected:expected.amount
        (value_numeric "amount" row)
      in
      let* () = expect_bool
        ~field:"active"
        ~expected:expected.active
        (Row.bool "active" row)
      in
      expect_date
        ~field:"created_on"
        ~expected:expected.created_on
        (value_date "created_on" row)

let apply_operation = fun connection table_name model operation ->
  match operation with
  | Put item ->
      let (year, month, day) = item.created_on in
      let* () =
        execute_unit
          connection
          ("insert into "
          ^ table_name
          ^ " (id, name, payload, amount, active, created_on) values ($1, $2, $3, $4, $5, $6) on conflict (id) do update set name = excluded.name, payload = excluded.payload, amount = excluded.amount, active = excluded.active, created_on = excluded.created_on")
          [
            Value.int item.id;
            Value.string item.name;
            Value.bytes (Bytes.from_string item.payload);
            Value.numeric item.amount;
            Value.bool item.active;
            Value.date year month day;
          ]
      in
      Ok (model_put model item)
  | Rename (id, name) ->
      let* () =
        execute_unit
          connection
          ("update " ^ table_name ^ " set name = $1 where id = $2")
          [ Value.string name; Value.int id ]
      in
      Ok (model_rename model id name)
  | SetActive (id, active) ->
      let* () =
        execute_unit
          connection
          ("update " ^ table_name ^ " set active = $1 where id = $2")
          [ Value.bool active; Value.int id ]
      in
      Ok (model_set_active model id active)
  | Delete id ->
      let* () =
        execute_unit connection ("delete from " ^ table_name ^ " where id = $1") [ Value.int id ]
      in
      Ok (model_delete model id)
  | Read id ->
      let* () = assert_db_item
        connection
        table_name
        id
        (model_find model id)
      in
      Ok model

let assert_workload = fun connection table_name operations ->
  let* () = execute_unit connection ("delete from " ^ table_name) [] in
  let rec loop model operations =
    match operations with
    | [] -> Ok model
    | operation :: rest ->
        let* model = apply_operation connection table_name model operation in
        loop model rest
  in
  let* model = loop [] operations in
  let* row = query_one connection ("select count(*) as n from " ^ table_name) [] in
  let* () = expect_int
    ~field:"n"
    ~expected:(List.length model)
    (row_int_like "n" row)
  in
  run_cases
    (List.map model ~fn:(fun item -> (Int.to_string item.id, item)))
    ~fn:(fun _name item ->
      assert_db_item connection table_name item.id (Some item))

let with_rollback = fun connection fn ->
  let* () =
    Postgres.Driver.begin_transaction connection
    |> Result.map_err ~fn:Postgres.Driver.error_to_string
  in
  let result = fn () in
  let rollback =
    Postgres.Driver.rollback connection
    |> Result.map_err ~fn:Postgres.Driver.error_to_string
  in
  match (result, rollback) with
  | (Ok value, Ok ()) -> Ok value
  | (Error error, Ok ()) -> Error error
  | (Ok _, Error error) -> Error ("rollback failed: " ^ error)
  | (Error error, Error rollback_error) -> Error (error ^ "; rollback failed: " ^ rollback_error)

let setup_workload_table = fun connection table_name ->
  let* () = execute_unit connection ("drop table if exists " ^ table_name) [] in
  execute_unit
    connection
    ("create table "
    ^ table_name
    ^ " (id int primary key, name text not null, payload bytea not null, amount numeric(20,4) not null, active boolean not null, created_on date not null)")
    []

let run_property = fun ctx name arb predicate ->
  let property = Property.for_all arb predicate in
  match Property.check
    ~config:property_config
    ~on_progress:(Test.Context.emit_progress ctx)
    property with
  | Property.Success -> Ok ()
  | Property.Failure { counter_example; shrink_steps } ->
      Error (String.concat
        "\n"
        [
          name ^ " failed";
          "Counter-example after " ^ Int.to_string shrink_steps ^ " shrink steps:";
          counter_example;
        ])
  | Property.Error { exception_; backtrace } ->
      Error (String.concat "\n" [ name ^ " raised"; Exception.to_string exception_; backtrace ])
  | Property.Assumption_violated -> Error (name ^ " exhausted assumptions")

let assert_generated_scalar_properties = fun ctx connection ->
  run_property
    ctx
    "postgres scalar value roundtrip"
    scalar_case_arb
    (fun value ->
      match assert_scalar_case connection value with
      | Ok () -> true
      | Error error -> Property.fail (scalar_case_to_string value ^ ": " ^ error))

let assert_generated_workload_properties = fun ctx connection ->
  let table_name = "riot_postgres_property_items" in
  let* () = setup_workload_table connection table_name in
  let result =
    run_property
      ctx
      "postgres operation workload"
      workload_arb
      (fun operations ->
        match with_rollback connection (fun () -> assert_workload connection table_name operations) with
        | Ok () -> true
        | Error error -> Property.fail (workload_to_string operations ^ ": " ^ error))
  in
  let cleanup = execute_unit connection ("drop table if exists " ^ table_name) [] in
  match (result, cleanup) with
  | (Ok (), Ok ()) -> Ok ()
  | (Error error, Ok ()) -> Error error
  | (Ok (), Error error) -> Error ("property table cleanup failed: " ^ error)
  | (Error error, Error cleanup_error) ->
      Error (error ^ "; property table cleanup failed: " ^ cleanup_error)

let assert_live_property_battery = fun ctx connection ->
  let* () = assert_text_protocol_scalar_roundtrips connection in
  let* () = assert_crud_roundtrips connection in
  let* () = assert_generated_scalar_properties ctx connection in
  assert_generated_workload_properties ctx connection

let test_parameterized_query_roundtrips = fun _ctx url ->
  with_connection
    url
    assert_parameterized_query_roundtrips

let test_parameterized_query_handles_large_text = fun _ctx url ->
  with_connection
    url
    assert_large_text_roundtrips

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

let postgres_container_image = fun () ->
  Testcontainers.Generic_image.(make "postgres" "16-alpine"
  |> with_env_var ~name:"POSTGRES_DB" ~value:postgres_container_database
  |> with_env_var ~name:"POSTGRES_USER" ~value:postgres_container_user
  |> with_env_var ~name:"POSTGRES_PASSWORD" ~value:postgres_container_password
  |> with_exposed_port ~port:postgres_container_port
  |> with_readiness_policy
    ~policy:(ReadinessPolicy.log
      ~message:"database system is ready to accept connections"
      ~duration:(Duration.of_secs 90)
      ~retry:180)
  |> with_readiness_policy ~policy:(ReadinessPolicy.delay ~duration:(Duration.of_secs 1)))

let container_config = fun container ->
  match Testcontainers.Container.host_port container ~port:postgres_container_port with
  | Error error -> Error (Testcontainers.error_to_string error)
  | Ok addr ->
      Ok {
        (Postgres.Config.default ()) with
        host = Net.Addr.ip addr;
        port = Net.Addr.port addr;
        database = postgres_container_database;
        user = postgres_container_user;
        password = postgres_container_password;
        ssl_mode = Postgres.Config.Disable;
        connect_timeout = Time.Duration.from_secs 30;
      }

let with_postgres_container = fun fn ->
  Testcontainers.with_container
    (postgres_container_image ())
    (fun container ->
      match container_config container with
      | Error message -> Error (Testcontainers.StartupTimeout message)
      | Ok config -> (
          match with_connection_config config fn with
          | Ok value -> Ok value
          | Error message -> Error (Testcontainers.StartupTimeout message)
        ))
  |> Result.map_err ~fn:Testcontainers.error_to_string

let test_postgres_container_properties = fun ctx ->
  with_postgres_container
    (fun connection ->
      let* () = assert_parameterized_query_roundtrips connection in
      let* () = assert_large_text_roundtrips connection in
      let* () = assert_transaction_roundtrips connection in
      assert_live_property_battery ctx connection)

let live_case = fun name fn ->
  match postgres_url () with
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
    live_case "parameterized query handles large text" test_parameterized_query_handles_large_text;
    container_property_case
      "property: postgres container values operations and transactions"
      ~examples:property_examples
      test_postgres_container_properties;
  ]

let main ~args = Test.Cli.main ~name:"postgres_tests" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
