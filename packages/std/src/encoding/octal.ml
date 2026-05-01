open Global

type decode_error =
  | InvalidOctal

let digit_char = fun digit -> Char.from_int_unchecked (Char.to_int '0' + digit)

let rec encode_positive_int64 = fun value ->
  if (
    match Int64.compare value 8L with
    | Order.LT -> true
    | Order.EQ
    | Order.GT -> false
  ) then
    String.make ~len:1 ~char:(digit_char (Int64.to_int value))
  else
    encode_positive_int64 (Int64.div value 8L)
    ^ String.make ~len:1 ~char:(digit_char (Int64.to_int (Int64.rem value 8L)))

let rec encode_negative_int64 = fun value ->
  if (
    match Int64.compare value (-8L) with
    | Order.GT -> true
    | Order.LT
    | Order.EQ -> false
  ) then
    String.make ~len:1 ~char:(digit_char (Int64.to_int (Int64.neg value)))
  else
    encode_negative_int64 (Int64.div value 8L)
    ^ String.make ~len:1 ~char:(digit_char (Int64.to_int (Int64.neg (Int64.rem value 8L))))

let encode_signed_int64 = fun value ->
  if (
    match Int64.compare value 0L with
    | Order.LT -> true
    | Order.EQ
    | Order.GT -> false
  ) then
    "-" ^ encode_negative_int64 value
  else
    encode_positive_int64 value

let classify = fun s ->
  if String.equal s "" then
    Error InvalidOctal
  else
    let len = String.length s in
    let (sign, start) =
      if String.get_unchecked s ~at:0 = '-' || String.get_unchecked s ~at:0 = '+' then
        (String.make ~len:1 ~char:(String.get_unchecked s ~at:0), 1)
      else
        ("", 0)
    in
    if start >= len then
      Error InvalidOctal
    else
      let has_octal_prefix =
        if start + 1 < len then
          let marker = String.get_unchecked s ~at:(start + 1) in
          String.get_unchecked s ~at:start = '0' && (marker = 'o' || marker = 'O')
        else
          false
      in
      let digits =
        if has_octal_prefix then
          String.sub s ~offset:(start + 2) ~len:(len - start - 2)
        else
          String.sub s ~offset:start ~len:(len - start)
      in
      if String.equal digits "" then
        Error InvalidOctal
      else if String.for_all
        digits
        ~fn:(fun __tmp1 ->
          match __tmp1 with
          | '0' .. '7' -> true
          | _ -> false) then
        Ok (sign ^ "0o" ^ digits)
      else
        Error InvalidOctal

let encode_int = fun value -> encode_signed_int64 (Int64.from_int value)

let encode_int32 = fun value -> encode_signed_int64 (Int64.from_int32 value)

let encode_int64 = encode_signed_int64

let decode_int = fun s ->
  match classify s with
  | Error _ as err -> err
  | Ok normalized -> (
      match Int.parse normalized with
      | Some value -> Ok value
      | None -> Error InvalidOctal
    )

let decode_int32 = fun s ->
  match classify s with
  | Error _ as err -> err
  | Ok normalized -> (
      match Int32.parse normalized with
      | Some value -> Ok value
      | None -> Error InvalidOctal
    )

let decode_int64 = fun s ->
  match classify s with
  | Error _ as err -> err
  | Ok normalized -> (
      match Int64.parse normalized with
      | Some value -> Ok value
      | None -> Error InvalidOctal
    )
