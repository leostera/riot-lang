open Std
open Propane

module Test = Std.Test
module Units = Human_units

let examples = 5_000

let property_config = { Property.default_config with test_count = examples }

let assert_property = fun name property ->
  match Property.check ~config:property_config property with
  | Property.Success -> Ok ()
  | Property.Failure { counter_example; shrink_steps } ->
      Error (name
      ^ " failed\nCounter-example:\n"
      ^ counter_example
      ^ "\nShrink steps: "
      ^ Int.to_string shrink_steps)
  | Property.Error { exception_; backtrace } ->
      Error (name ^ " raised " ^ Exception.to_string exception_ ^ "\n" ^ backtrace)
  | Property.Assumption_violated -> Error (name ^ " exhausted assumptions")

let pow = fun base exponent ->
  let rec loop acc remaining =
    if remaining = 0 then
      acc
    else
      loop (acc * base) (remaining - 1)
  in
  loop 1 exponent

let int64_to_int = fun value -> Int64.to_int value

let duration_nanos = fun duration ->
  Time.Duration.to_nanos duration
  |> int64_to_int

let byte_suffix = fun exponent ->
  match exponent with
  | 0 -> "B"
  | 1 -> "KiB"
  | 2 -> "MiB"
  | 3 -> "GiB"
  | 4 -> "TiB"
  | 5 -> "PiB"
  | _ -> "EiB"

let byte_unit_cases = [
  ("", 1);
  ("B", 1);
  ("byte", 1);
  ("bytes", 1);
  ("K", 1_000);
  ("KB", 1_000);
  ("MB", 1_000_000);
  ("GB", 1_000_000_000);
  ("TB", 1_000_000_000_000);
  ("PB", 1_000_000_000_000_000);
  ("KiB", pow 1_024 1);
  ("MiB", pow 1_024 2);
  ("GiB", pow 1_024 3);
  ("TiB", pow 1_024 4);
  ("PiB", pow 1_024 5);
]

let one_of_list = fun values ->
  values
  |> List.map ~fn:(fun value -> Generator.return value)
  |> Generator.one_of

let known_byte_quantity =
  Arbitrary.make
    ~print:(fun (amount, (suffix, multiplier)) ->
      "{ amount = "
      ^ Int.to_string amount
      ^ "; suffix = "
      ^ suffix
      ^ "; multiplier = "
      ^ Int.to_string multiplier
      ^ " }")
    Generator.(pair (int_range 0 1_000) (one_of_list byte_unit_cases))

let exact_binary_quantity =
  Arbitrary.make
    ~print:(fun (exponent, amount) ->
      "{ exponent = " ^ Int.to_string exponent ^ "; amount = " ^ Int.to_string amount ^ " }")
    Generator.(pair (int_range 0 5) (int_range 1 1_023))

let input_with_suffix = fun amount suffix ->
  if String.is_empty suffix then
    Int.to_string amount
  else
    Int.to_string amount ^ " " ^ suffix

let parse_bytes_known_units =
  Property.for_all
    known_byte_quantity
    (fun (amount, (suffix, multiplier)) ->
      let input = input_with_suffix amount suffix in
      match Units.parse_bytes input with
      | Ok actual -> Int.equal actual (amount * multiplier)
      | Error error ->
          Property.fail ("parse_bytes rejected " ^ input ^ ": " ^ Units.error_to_string error))

let bytes_formats_exact_binary_multiples =
  Property.for_all
    exact_binary_quantity
    (fun (exponent, amount) ->
      let count = amount * pow 1_024 exponent in
      let expected = Int.to_string amount ^ " " ^ byte_suffix exponent in
      let actual = Units.bytes count in
      if String.equal actual expected then
        true
      else
        Property.fail ("expected " ^ expected ^ ", got " ^ actual))

let duration_unit_cases = [
  ("ns", 1);
  ("nsec", 1);
  ("us", 1_000);
  ("µs", 1_000);
  ("ms", 1_000_000);
  ("s", 1_000_000_000);
  ("sec", 1_000_000_000);
  ("mins", 60 * 1_000_000_000);
  ("hrs", 3_600 * 1_000_000_000);
  ("days", 86_400 * 1_000_000_000);
  ("weeks", 604_800 * 1_000_000_000);
  ("M", 2_630_016 * 1_000_000_000);
  ("months", 2_630_016 * 1_000_000_000);
  ("years", 31_557_600 * 1_000_000_000);
]

let known_duration_quantity =
  Arbitrary.make
    ~print:(fun (amount, (suffix, multiplier)) ->
      "{ amount = "
      ^ Int.to_string amount
      ^ "; suffix = "
      ^ suffix
      ^ "; nanos = "
      ^ Int.to_string multiplier
      ^ " }")
    Generator.(pair (int_range 0 100) (one_of_list duration_unit_cases))

type duration_fields = { years: int; months: int; days: int; hours: int; mins: int; secs: int }

let duration_fields =
  let left = Generator.(triple
    (int_range 0 4)
    (int_range 0 10)
    (int_range 0 29))
  in
  let right = Generator.(triple
    (int_range 0 23)
    (int_range 0 59)
    (int_range 0 59))
  in
  Arbitrary.make
    ~print:(fun fields ->
      "{ years = "
      ^ Int.to_string fields.years
      ^ "; months = "
      ^ Int.to_string fields.months
      ^ "; days = "
      ^ Int.to_string fields.days
      ^ "; hours = "
      ^ Int.to_string fields.hours
      ^ "; mins = "
      ^ Int.to_string fields.mins
      ^ "; secs = "
      ^ Int.to_string fields.secs
      ^ " }")
    Generator.(map2
      (fun (years, months, days) (hours, mins, secs) ->
        {
          years;
          months;
          days;
          hours;
          mins;
          secs;
        })
      left
      right)

let parse_duration_known_units =
  Property.for_all
    known_duration_quantity
    (fun (amount, (suffix, multiplier)) ->
      let input = Int.to_string amount ^ suffix in
      match Units.parse_duration input with
      | Ok actual -> Int.equal (duration_nanos actual) (amount * multiplier)
      | Error error ->
          Property.fail ("parse_duration rejected " ^ input ^ ": " ^ Units.error_to_string error))

let add_duration_part = fun value singular plural parts ->
  if value = 0 then
    parts
  else
    (
      Int.to_string value ^ if value = 1 then
        singular
      else
        plural
    ) :: parts

let expected_duration = fun fields ->
  let parts =
    []
    |> add_duration_part fields.years "year" "years"
    |> add_duration_part fields.months "month" "months"
    |> add_duration_part fields.days "day" "days"
    |> add_duration_part fields.hours "hr" "hrs"
    |> add_duration_part fields.mins "min" "mins"
    |> add_duration_part fields.secs "sec" "secs"
    |> List.reverse
  in
  if List.is_empty parts then
    "0secs"
  else
    String.concat " " parts

let duration_seconds = fun fields ->
  (fields.years * 31_557_600)
  + (fields.months * 2_630_016)
  + (fields.days * 86_400)
  + (fields.hours * 3_600)
  + (fields.mins * 60)
  + fields.secs

let duration_formats_known_fields =
  Property.for_all
    duration_fields
    (fun fields ->
      let expected = expected_duration fields in
      let actual = Units.duration (Time.Duration.from_secs (duration_seconds fields)) in
      if String.equal actual expected then
        true
      else
        Property.fail ("expected " ^ expected ^ ", got " ^ actual))

let tests =
  Test.[
    property
      "parse_bytes accepts known units"
      ~examples
      (fun _ctx -> assert_property "parse_bytes accepts known units" parse_bytes_known_units);
    property
      "bytes formats exact binary multiples"
      ~examples
      (fun _ctx ->
        assert_property
          "bytes formats exact binary multiples"
          bytes_formats_exact_binary_multiples);
    property
      "parse_duration accepts known units"
      ~examples
      (fun _ctx -> assert_property "parse_duration accepts known units" parse_duration_known_units);
    property
      "duration formats canonical known fields"
      ~examples
      (fun _ctx ->
        assert_property
          "duration formats canonical known fields"
          duration_formats_known_fields);
  ]

let main ~args = Test.Cli.main ~name:"human_units_property_tests" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
