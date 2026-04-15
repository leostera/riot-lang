open Std
open Std.IO

type t =
  | Null
  | Int of int
  | Int64 of int64
  | Int16 of int
  | Float of float
  | String of string
  | Bool of bool
  | Bytes of bytes
  | Timestamp of DateTime.t
  | TimestampWithTimezone of DateTime.t
  | Date of int * int * int
  | Time of int * int * int * int
  | Uuid of string
  | Json of string
  | Numeric of string

let null = Null

let int = fun n -> Int n

let int64 = fun n -> Int64 n

let int16 = fun n -> Int16 n

let string = fun s -> String s

let bool = fun b -> Bool b

let float = fun f -> Float f

let bytes = fun b -> Bytes b

let timestamp = fun dt -> Timestamp dt

let timestamp_with_timezone = fun dt -> TimestampWithTimezone dt

let date = fun y m d -> Date (y, m, d)

let time = fun h min s us -> Time (h, min, s, us)

let uuid = fun s -> Uuid s

let json = fun s -> Json s

let numeric = fun s -> Numeric s

let to_int = function
  | Int n -> Some n
  | _ -> None

let bytes_equal = fun left right ->
  String.equal (Bytes.to_string left) (Bytes.to_string right)

let bytes_compare = fun left right ->
  String.compare (Bytes.to_string left) (Bytes.to_string right)

let to_int64 = function
  | Int64 n -> Some n
  | _ -> None

let to_int16 = function
  | Int16 n -> Some n
  | _ -> None

let to_string_value = function
  | String s -> Some s
  | _ -> None

let to_bool = function
  | Bool b -> Some b
  | _ -> None

let to_float = function
  | Float f -> Some f
  | _ -> None

let to_bytes = function
  | Bytes b -> Some b
  | _ -> None

let to_timestamp = function
  | Timestamp dt -> Some dt
  | _ -> None

let to_timestamp_with_timezone = function
  | TimestampWithTimezone dt -> Some dt
  | _ -> None

let to_date = function
  | Date (y, m, d) -> Some (y, m, d)
  | _ -> None

let to_time = function
  | Time (h, m, s, us) -> Some (h, m, s, us)
  | _ -> None

let to_uuid = function
  | Uuid s -> Some s
  | _ -> None

let to_json = function
  | Json s -> Some s
  | _ -> None

let to_numeric = function
  | Numeric s -> Some s
  | _ -> None

let is_null = function
  | Null -> true
  | _ -> false

let to_string = function
  | Null ->
      "NULL"
  | Int n ->
      string_of_int n
  | Int64 n ->
      Int64.to_string n
  | Int16 n ->
      string_of_int n
  | Float f ->
      string_of_float f
  | String s ->
      "\"" ^ s ^ "\""
  | Bool b ->
      Bool.to_string b
  | Bytes b ->
      "<bytes:" ^ string_of_int (Bytes.length b) ^ ">"
  | Timestamp dt ->
      DateTime.to_iso8601 dt
  | TimestampWithTimezone dt ->
      DateTime.to_iso8601 dt
  | Date (y, m, d) ->
      let pad n width =
        let s = string_of_int n in
        String.make ~len:(max 0 (width - String.length s)) ~char:'0' ^ s
      in
      pad y 4 ^ "-" ^ pad m 2 ^ "-" ^ pad d 2
  | Time (h, min, s, us) ->
      let pad n width =
        let s = string_of_int n in
        String.make ~len:(max 0 (width - String.length s)) ~char:'0' ^ s
      in
      pad h 2 ^ ":" ^ pad min 2 ^ ":" ^ pad s 2 ^ "." ^ pad us 6
  | Uuid s ->
      s
  | Json s ->
      s
  | Numeric s ->
      s

let equal = fun a b ->
  match (a, b) with
  | Null, Null -> true
  | Int x, Int y -> x = y
  | Int64 x, Int64 y -> Int64.equal x y
  | Int16 x, Int16 y -> x = y
  | Float x, Float y -> x = y
  | String x, String y -> x = y
  | Bool x, Bool y -> x = y
  | Bytes x, Bytes y -> bytes_equal x y
  | Timestamp x, Timestamp y -> DateTime.equal x y
  | TimestampWithTimezone x, TimestampWithTimezone y -> DateTime.equal x y
  | Date (y1, m1, d1), Date (y2, m2, d2) -> y1 = y2 && m1 = m2 && d1 = d2
  | Time (h1, min1, s1, us1), Time (h2, min2, s2, us2) -> h1 = h2
  && min1 = min2
  && s1 = s2
  && us1 = us2
  | Uuid x, Uuid y -> x = y
  | Json x, Json y -> x = y
  | Numeric x, Numeric y -> x = y
  | _ -> false

let compare = fun a b ->
  match (a, b) with
  | Null, Null ->
      0
  | Null, _ ->
      (-1)
  | _, Null ->
      1
  | Int x, Int y ->
      Int.compare x y
  | Int64 x, Int64 y ->
      Int64.compare x y
  | Int16 x, Int16 y ->
      Int.compare x y
  | Float x, Float y ->
      Float.compare x y
  | String x, String y ->
      String.compare x y
  | Bool x, Bool y ->
      Bool.compare x y
  | Bytes x, Bytes y ->
      bytes_compare x y
  | Timestamp x, Timestamp y ->
      Time.SystemTime.compare (DateTime.to_system_time x) (DateTime.to_system_time y)
  | TimestampWithTimezone x, TimestampWithTimezone y ->
      Time.SystemTime.compare (DateTime.to_system_time x) (DateTime.to_system_time y)
  | Date (y1, m1, d1), Date (y2, m2, d2) -> (
      match Int.compare y1 y2 with
      | 0 -> (
          match Int.compare m1 m2 with
          | 0 -> Int.compare d1 d2
          | c -> c
        )
      | c -> c
    )
  | Time (h1, min1, s1, us1), Time (h2, min2, s2, us2) -> (
      match Int.compare h1 h2 with
      | 0 -> (
          match Int.compare min1 min2 with
          | 0 -> (
              match Int.compare s1 s2 with
              | 0 -> Int.compare us1 us2
              | c -> c
            )
          | c -> c
        )
      | c -> c
    )
  | Uuid x, Uuid y ->
      String.compare x y
  | Json x, Json y ->
      String.compare x y
  | Numeric x, Numeric y ->
      String.compare x y
  | Int _, _ ->
      (-1)
  | _, Int _ ->
      1
  | Int64 _, _ ->
      (-1)
  | _, Int64 _ ->
      1
  | Int16 _, _ ->
      (-1)
  | _, Int16 _ ->
      1
  | Float _, _ ->
      (-1)
  | _, Float _ ->
      1
  | String _, _ ->
      (-1)
  | _, String _ ->
      1
  | Bool _, _ ->
      (-1)
  | _, Bool _ ->
      1
  | Bytes _, _ ->
      (-1)
  | _, Bytes _ ->
      1
  | Timestamp _, _ ->
      (-1)
  | _, Timestamp _ ->
      1
  | TimestampWithTimezone _, _ ->
      (-1)
  | _, TimestampWithTimezone _ ->
      1
  | Date _, _ ->
      (-1)
  | _, Date _ ->
      1
  | Time _, _ ->
      (-1)
  | _, Time _ ->
      1
  | Uuid _, _ ->
      (-1)
  | _, Uuid _ ->
      1
  | Json _, _ ->
      (-1)
  | _, Json _ ->
      1
