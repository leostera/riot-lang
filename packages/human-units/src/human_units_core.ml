open Std

type error =
  | Empty
  | ExpectedNumber of int
  | InvalidNumber of string
  | MissingUnit of string
  | UnknownUnit of string
  | Overflow
  | PrecisionLoss of string

type decimal = { numerator: int; denominator: int; raw: string }

type byte_unit =
  | Byte
  | Decimal of int
  | Binary of int

type byte_quantity = {
  byte_amount: decimal;
  byte_unit: byte_unit;
}

type duration_unit =
  | Nanosecond
  | Microsecond
  | Millisecond
  | Second
  | Minute
  | Hour
  | Day
  | Week
  | Month
  | Year

type duration_item = {
  duration_amount: decimal;
  duration_unit: duration_unit;
}

let ( let* ) result fn =
  match result with
  | Ok value -> fn value
  | Error _ as error -> error

let error_to_string = fun error ->
  match error with
  | Empty -> "value was empty"
  | ExpectedNumber offset -> "expected number at " ^ Int.to_string offset
  | InvalidNumber value -> "invalid number " ^ value
  | MissingUnit value -> "unit needed for " ^ value
  | UnknownUnit unit -> "unknown unit " ^ unit
  | Overflow -> "number is too large"
  | PrecisionLoss value -> "value cannot be represented exactly: " ^ value

let is_space = fun char ->
  match char with
  | ' '
  | '\t'
  | '\n'
  | '\r' -> true
  | _ -> false

let is_digit = fun char ->
  let code = Char.code char in
  code >= Char.code '0' && code <= Char.code '9'

let digit_value = fun char -> Char.code char - Char.code '0'

let checked_add = fun left right ->
  if right > 0 && left > Int.max_int - right then
    Error Overflow
  else
    Ok (left + right)

let checked_mul = fun left right ->
  if left = 0 || right = 0 then
    Ok 0
  else if left > Int.max_int / right then
    Error Overflow
  else
    Ok (left * right)

let checked_pow = fun base exponent ->
  let rec loop remaining acc =
    if remaining = 0 then
      Ok acc
    else
      let* next = checked_mul acc base in
      loop (remaining - 1) next
  in
  loop exponent 1

module Lexer = struct
  type source = { text: string; len: int }

  type number_token = {
    amount: decimal;
    start: int;
    finish: int;
  }

  type unit_token = { raw: string; start: int; finish: int }

  let source = fun text -> { text; len = String.length text }

  let peek = fun source index ->
    if index < source.len then
      String.get source.text ~at:index
    else
      None

  let slice = fun source ~start ~finish ->
    let finish = Int.min finish source.len in
    if finish <= start then
      ""
    else
      String.sub source.text ~offset:start ~len:(finish - start)

  let skip_spaces = fun source index ->
    let rec loop index =
      match peek source index with
      | Some char when is_space char -> loop (index + 1)
      | Some _
      | None -> index
    in
    loop index

  let rec read_digits = fun source start index acc saw_digit ->
    match peek source index with
    | Some char when is_digit char ->
        let* shifted = checked_mul acc 10 in
        let* next = checked_add shifted (digit_value char) in
        read_digits source start (index + 1) next true
    | Some _
    | None ->
        if saw_digit then
          Ok (acc, index)
        else
          Error (ExpectedNumber start)

  let number = fun source index ->
    let start = index in
    let* (whole, after_whole) = read_digits source start index 0 false in
    match peek source after_whole with
    | Some '.' ->
        let after_dot = after_whole + 1 in
        let rec fraction index numerator denominator saw_digit =
          match peek source index with
          | Some char when is_digit char ->
              let* shifted = checked_mul numerator 10 in
              let* next_numerator = checked_add shifted (digit_value char) in
              let* next_denominator = checked_mul denominator 10 in
              fraction (index + 1) next_numerator next_denominator true
          | Some _
          | None ->
              if saw_digit then
                Ok (numerator, denominator, index)
              else
                Error (InvalidNumber (slice source ~start ~finish:after_dot))
        in
        let* (numerator, denominator, finish) = fraction after_dot whole 1 false in
        Ok (
          {
            amount = { numerator; denominator; raw = slice source ~start ~finish };
            start;
            finish;
          },
          finish
        )
    | Some _
    | None ->
        Ok (
          {
            amount = {
              numerator = whole;
              denominator = 1;
              raw = slice source ~start ~finish:after_whole;
            };
            start;
            finish = after_whole;
          },
          after_whole
        )

  let byte_unit = fun source index ->
    {
      raw =
        slice source ~start:index ~finish:source.len
        |> String.trim;
      start = index;
      finish = source.len;
    }

  let duration_unit = fun source index ->
    let rec loop index =
      match peek source index with
      | Some char ->
          if is_digit char || is_space char then
            index
          else
            loop (index + 1)
      | None -> index
    in
    let finish = loop index in
    if finish = index then
      None
    else
      Some ({ raw = slice source ~start:index ~finish; start = index; finish }, finish)
end

let round_scaled_to_int = fun decimal multiplier ->
  let* scaled = checked_mul decimal.numerator multiplier in
  let quotient = scaled / decimal.denominator in
  let remainder = scaled mod decimal.denominator in
  if remainder >= ((decimal.denominator + 1) / 2) then
    checked_add quotient 1
  else
    Ok quotient

let exact_scaled_to_int = fun decimal multiplier ->
  let* scaled = checked_mul decimal.numerator multiplier in
  if scaled mod decimal.denominator = 0 then
    Ok (scaled / decimal.denominator)
  else
    Error (PrecisionLoss decimal.raw)

let trim_trailing_zero_decimal = fun text ->
  if String.ends_with ~suffix:".0" text then
    String.sub text ~offset:0 ~len:(String.length text - 2)
  else
    text

let format_one_decimal = fun value ->
  Float.to_string ~precision:1 value
  |> trim_trailing_zero_decimal

let byte_suffixes = [|"B"; "KiB"; "MiB"; "GiB"; "TiB"; "PiB"; "EiB"|]

let bytes = fun count ->
  if count <= 0 then
    "0 B"
  else
    let unit = 1_024.0 in
    let size = Float.from_int count in
    let rec choose index scaled =
      if scaled >= unit && index + 1 < Array.length byte_suffixes then
        choose (index + 1) (scaled /. unit)
      else
        (index, scaled)
    in
    let (index, scaled) = choose 0 size in
    format_one_decimal scaled ^ " " ^ Array.get_unchecked byte_suffixes ~at:index

let byte_unit_of_token = fun (token: Lexer.unit_token) ->
  let raw = token.raw in
  let normalized = String.lowercase_ascii raw in
  match normalized with
  | ""
  | "b"
  | "byte"
  | "bytes" -> Ok Byte
  | "kib" -> Ok (Binary 1)
  | "mib" -> Ok (Binary 2)
  | "gib" -> Ok (Binary 3)
  | "tib" -> Ok (Binary 4)
  | "pib" -> Ok (Binary 5)
  | "eib" -> Ok (Binary 6)
  | "kb"
  | "k" -> Ok (Decimal 1)
  | "mb" -> Ok (Decimal 2)
  | "gb" -> Ok (Decimal 3)
  | "tb" -> Ok (Decimal 4)
  | "pb" -> Ok (Decimal 5)
  | "eb" -> Ok (Decimal 6)
  | _ -> Error (UnknownUnit raw)

let byte_unit_multiplier = fun unit ->
  match unit with
  | Byte -> Ok 1
  | Decimal exponent -> checked_pow 1_000 exponent
  | Binary exponent -> checked_pow 1_024 exponent

let tokenize_bytes = fun text ->
  let source = Lexer.source text in
  let start = Lexer.skip_spaces source 0 in
  if start >= source.Lexer.len then
    Error Empty
  else
    let* (number, after_number) = Lexer.number source start in
    let unit_start = Lexer.skip_spaces source after_number in
    let unit_token = Lexer.byte_unit source unit_start in
    let* byte_unit = byte_unit_of_token unit_token in
    Ok { byte_amount = number.Lexer.amount; byte_unit }

let parse_bytes = fun input ->
  let* quantity = tokenize_bytes input in
  let* multiplier = byte_unit_multiplier quantity.byte_unit in
  round_scaled_to_int quantity.byte_amount multiplier

let secs_per_min = 60

let secs_per_hour = 3_600

let secs_per_day = 86_400

let secs_per_week = 604_800

let secs_per_month = 2_630_016

let secs_per_year = 31_557_600

let nanos_per_micro = 1_000

let nanos_per_milli = 1_000_000

let nanos_per_sec = 1_000_000_000

let duration_unit_of_token = fun (token: Lexer.unit_token) ->
  match token.raw with
  | "M" -> Ok Month
  | raw -> (
      match String.lowercase_ascii raw with
      | "nanos"
      | "nsec"
      | "ns" -> Ok Nanosecond
      | "usec"
      | "us"
      | "µs" -> Ok Microsecond
      | "millis"
      | "msec"
      | "ms" -> Ok Millisecond
      | "seconds"
      | "second"
      | "secs"
      | "sec"
      | "s" -> Ok Second
      | "minutes"
      | "minute"
      | "mins"
      | "min"
      | "m" -> Ok Minute
      | "hours"
      | "hour"
      | "hrs"
      | "hr"
      | "h" -> Ok Hour
      | "days"
      | "day"
      | "d" -> Ok Day
      | "weeks"
      | "week"
      | "wks"
      | "wk"
      | "w" -> Ok Week
      | "months"
      | "month" -> Ok Month
      | "years"
      | "year"
      | "yrs"
      | "yr"
      | "y" -> Ok Year
      | _ -> Error (UnknownUnit raw)
    )

let duration_unit_multiplier = fun unit ->
  match unit with
  | Nanosecond -> Ok 1
  | Microsecond -> Ok nanos_per_micro
  | Millisecond -> Ok nanos_per_milli
  | Second -> Ok nanos_per_sec
  | Minute -> checked_mul secs_per_min nanos_per_sec
  | Hour -> checked_mul secs_per_hour nanos_per_sec
  | Day -> checked_mul secs_per_day nanos_per_sec
  | Week -> checked_mul secs_per_week nanos_per_sec
  | Month -> checked_mul secs_per_month nanos_per_sec
  | Year -> checked_mul secs_per_year nanos_per_sec

let tokenize_duration = fun text ->
  let source = Lexer.source text in
  let rec loop index items =
    let index = Lexer.skip_spaces source index in
    if index >= source.Lexer.len then
      if List.is_empty items then
        Error Empty
      else
        Ok (List.reverse items)
    else
      let* (number, after_number) = Lexer.number source index in
      let unit_start = Lexer.skip_spaces source after_number in
      match Lexer.duration_unit source unit_start with
      | None -> Error (MissingUnit number.Lexer.amount.raw)
      | Some (unit_token, after_unit) ->
          let* duration_unit = duration_unit_of_token unit_token in
          loop after_unit ({ duration_amount = number.Lexer.amount; duration_unit } :: items)
  in
  loop 0 []

let duration_item_nanos = fun item ->
  let* multiplier = duration_unit_multiplier item.duration_unit in
  exact_scaled_to_int item.duration_amount multiplier

let duration_items_to_duration = fun items ->
  let rec loop items total_nanos =
    match items with
    | [] -> Ok (Time.Duration.from_nanos total_nanos)
    | item :: rest ->
        let* nanos = duration_item_nanos item in
        let* total_nanos = checked_add total_nanos nanos in
        loop rest total_nanos
  in
  loop items 0

let parse_duration = fun input ->
  let text = String.trim input in
  if String.is_empty text then
    Error Empty
  else if String.equal text "0" then
    Ok Time.Duration.zero
  else
    let* items = tokenize_duration text in
    duration_items_to_duration items

let add_part = fun value singular plural parts ->
  if value = 0 then
    parts
  else
    (
      Int.to_string value ^ if value = 1 then
        singular
      else
        plural
    ) :: parts

let format_tenths = fun tenths suffix ->
  let whole = tenths / 10 in
  let frac = tenths mod 10 in
  if frac = 0 then
    Int.to_string whole ^ suffix
  else
    Int.to_string whole ^ "." ^ Int.to_string frac ^ suffix

let round_to_tenths = fun value scale ->
  let quotient = value / scale in
  let remainder = value mod scale in
  let base = quotient * 10 in
  let frac = (remainder * 10) / scale in
  let rest = (remainder * 10) mod scale in
  let rounded = base + frac in
  if rest >= ((scale + 1) / 2) then
    rounded + 1
  else
    rounded

let format_subsecond = fun nanos ->
  if nanos >= nanos_per_milli then
    format_tenths (round_to_tenths nanos nanos_per_milli) "ms"
  else if nanos >= nanos_per_micro then
    format_tenths (round_to_tenths nanos nanos_per_micro) "µs"
  else
    Int.to_string nanos ^ "ns"

let duration = fun value ->
  let secs = Time.Duration.to_secs value in
  let nanos = Time.Duration.subsec_nanos value in
  if secs = 0 && nanos = 0 then
    "0secs"
  else if secs = 0 then
    format_subsecond nanos
  else
    let years = secs / secs_per_year in
    let remaining = secs mod secs_per_year in
    let months = remaining / secs_per_month in
    let remaining = remaining mod secs_per_month in
    let days = remaining / secs_per_day in
    let remaining = remaining mod secs_per_day in
    let hours = remaining / secs_per_hour in
    let remaining = remaining mod secs_per_hour in
    let mins = remaining / secs_per_min in
    let secs = remaining mod secs_per_min in
    []
    |> add_part years "year" "years"
    |> add_part months "month" "months"
    |> add_part days "day" "days"
    |> add_part hours "hr" "hrs"
    |> add_part mins "min" "mins"
    |> add_part secs "sec" "secs"
    |> (fun parts ->
      if nanos = 0 then
        parts
      else
        format_subsecond nanos :: parts)
    |> List.reverse
    |> String.concat " "
