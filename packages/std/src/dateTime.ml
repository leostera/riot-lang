open Global

(** Date and time utilities *)
(* Helper to format integer with zero padding *)

let pad2 = fun n ->
  if n < 10 then
    "0" ^ string_of_int n
  else
    string_of_int n

let pad3 = fun n ->
  if n < 10 then
    "00" ^ string_of_int n
  else if n < 100 then
    "0" ^ string_of_int n
  else
    string_of_int n

let pad4 = fun n ->
  if n < 10 then
    "000" ^ string_of_int n
  else if n < 100 then
    "00" ^ string_of_int n
  else if n < 1_000 then
    "0" ^ string_of_int n
  else
    string_of_int n

module Tz = struct
  type t =
    Etc_UTC
    | Local

  let to_string = function
    | Etc_UTC -> "UTC"
    | Local -> "Local"
end

type t = {
  microseconds: int * int;  (* (microseconds, precision) e.g. (426822, 6) *)
  second: int;
  minute: int;
  hour: int;
  day: int;
  month: int;
  year: int;
  time_zone: Tz.t;
  utc_offset: int;
  std_offset: int;
}

type naive = {
  year: int;
  month: int;
  day: int;
  hour: int;
  minute: int;
  second: int;
  microsecond: int;
}

let now = fun () ->
  let unix_time = Kernel.Time.gettimeofday () in
  let tm = Kernel.Time.localtime unix_time in
  let microseconds =
    let frac = unix_time -. floor unix_time in
    let micros = int_of_float (frac *. 1000000.0) in
    (micros, 6)
  in
  {
    microseconds;
    second = tm.tm_sec;
    minute = tm.tm_min;
    hour = tm.tm_hour;
    day = tm.tm_mday;
    month = tm.tm_mon + 1;
    year = tm.tm_year + 1_900;
    time_zone = Tz.Local;
    utc_offset = 0;
    std_offset = 0;
  }

let now_utc = fun () ->
  let unix_time = Kernel.Time.gettimeofday () in
  let tm = Kernel.Time.gmtime unix_time in
  let microseconds =
    let frac = unix_time -. floor unix_time in
    let micros = int_of_float (frac *. 1000000.0) in
    (micros, 6)
  in
  {
    microseconds;
    second = tm.tm_sec;
    minute = tm.tm_min;
    hour = tm.tm_hour;
    day = tm.tm_mday;
    month = tm.tm_mon + 1;
    year = tm.tm_year + 1_900;
    time_zone = Tz.Etc_UTC;
    utc_offset = 0;
    std_offset = 0;
  }

let now_naive = fun () ->
  let unix_time = Kernel.Time.gettimeofday () in
  let tm = Kernel.Time.localtime unix_time in
  let frac = unix_time -. floor unix_time in
  let microsecond = int_of_float (frac *. 1000000.0) in
  {
    year = tm.tm_year + 1_900;
    month = tm.tm_mon + 1;
    day = tm.tm_mday;
    hour = tm.tm_hour;
    minute = tm.tm_min;
    second = tm.tm_sec;
    microsecond;
  }

let from_system_time: Time.SystemTime.t -> t = fun sys_time ->
  let unix_time = Time.SystemTime.secs_float sys_time in
  let tm = Kernel.Time.gmtime unix_time in
  let nanos_total = Time.SystemTime.nanos sys_time in
  let microseconds =
    (* Extract microseconds from total nanoseconds *)
    let micros = Int64.div nanos_total 1_000L in
    let micros_part = Int64.to_int (Int64.rem micros 1_000_000L) in
    (micros_part, 6)
  in
  {
    microseconds;
    second = tm.tm_sec;
    minute = tm.tm_min;
    hour = tm.tm_hour;
    day = tm.tm_mday;
    month = tm.tm_mon + 1;
    year = tm.tm_year + 1_900;
    time_zone = Tz.Etc_UTC;
    utc_offset = 0;
    std_offset = 0;
  }

let to_system_time: t -> Time.SystemTime.t = fun t ->
  let tm = {
    Kernel.Time.tm_sec = t.second;
    tm_min = t.minute;
    tm_hour = t.hour;
    tm_mday = t.day;
    tm_mon = t.month - 1;
    tm_year = t.year - 1_900;
    tm_wday = 0;
    tm_yday = 0;
    tm_isdst = false;
  }
  in
  let unix_time, _ = Kernel.Time.mktime tm in
  let micros, _ = t.microseconds in
  (* Convert to nanoseconds *)
  let secs_nanos = Int64.mul (Int64.of_float unix_time) 1_000_000_000L in
  let micros_nanos = Int64.mul (Int64.of_int micros) 1_000L in
  let total_nanos = Int64.add secs_nanos micros_nanos in
  Time.SystemTime.from_nanos total_nanos

let epoch = Time.SystemTime.epoch |> from_system_time

(** Convert timezone-aware datetime to naive datetime *)
let to_naive: t -> naive = fun t ->
  let microsecond, _ = t.microseconds in
  {
    year = t.year;
    month = t.month;
    day = t.day;
    hour = t.hour;
    minute = t.minute;
    second = t.second;
    microsecond;
  }

(** Convert naive datetime to timezone-aware datetime *)
let from_naive: naive -> tz:Tz.t -> t = fun naive ~tz ->
  match tz with
  | Tz.Etc_UTC ->
      {
        microseconds = (naive.microsecond, 6);
        second = naive.second;
        minute = naive.minute;
        hour = naive.hour;
        day = naive.day;
        month = naive.month;
        year = naive.year;
        time_zone = Tz.Etc_UTC;
        utc_offset = 0;
        std_offset = 0;
      }
  | Tz.Local ->
      (* For local time, we need to determine the UTC offset *)
      (* Create a Unix tm struct and let mktime figure out the offset *)
      let tm = {
        Kernel.Time.tm_sec = naive.second;
        tm_min = naive.minute;
        tm_hour = naive.hour;
        tm_mday = naive.day;
        tm_mon = naive.month - 1;
        tm_year = naive.year - 1_900;
        tm_wday = 0;
        tm_yday = 0;
        tm_isdst = false;
      }
      in
      let unix_time, normalized_tm = Kernel.Time.mktime tm in
      (* Get UTC offset by comparing with gmtime *)
      let utc_tm = Kernel.Time.gmtime unix_time in
      let local_seconds = normalized_tm.tm_hour * 3_600 + normalized_tm.tm_min * 60 + normalized_tm.tm_sec in
      let utc_seconds = utc_tm.tm_hour * 3_600 + utc_tm.tm_min * 60 + utc_tm.tm_sec in
      let utc_offset = local_seconds - utc_seconds in
      {
        microseconds = (naive.microsecond, 6);
        second = naive.second;
        minute = naive.minute;
        hour = naive.hour;
        day = naive.day;
        month = naive.month;
        year = naive.year;
        time_zone = Tz.Local;
        utc_offset;
        std_offset = 0;
      }

let to_iso8601 = fun t ->
  let micros, _ = t.microseconds in
  let millis = micros / 1_000 in
  let tz_suffix =
    match t.time_zone with
    | Tz.Etc_UTC -> "Z"
    | Tz.Local ->
        if t.utc_offset = 0 then
          "Z"
        else
          let hours = abs t.utc_offset / 3_600 in
          let mins = abs t.utc_offset mod 3_600 / 60 in
          let sign =
            if t.utc_offset >= 0 then
              "+"
            else
              "-"
          in
          sign ^ pad2 hours ^ ":" ^ pad2 mins
  in
  pad4 t.year
  ^ "-"
  ^ pad2 t.month
  ^ "-"
  ^ pad2 t.day
  ^ "T"
  ^ pad2 t.hour
  ^ ":"
  ^ pad2 t.minute
  ^ ":"
  ^ pad2 t.second
  ^ "."
  ^ pad3 millis
  ^ tz_suffix

let equal = fun t1 t2 ->
  let st1 = to_system_time t1 |> Time.SystemTime.nanos in
  let st2 = to_system_time t2 |> Time.SystemTime.nanos in
  st1 = st2

type error =
  | Invalid_format of string
  | Invalid_date of string
  | Invalid_time of string
  | Invalid_timezone of string

(* ISO 8601 Parser
 *
 * This parser has full parity with Elixir's DateTime.from_iso8601/2.
 * 
 * Design Philosophy:
 * - Small, composable parser functions in a submodule
 * - Use Option and_then operators let-star to avoid nested matches
 * - Each parser handles one concern (date, time, timezone, etc.)
 * - Main parse function orchestrates using Result for validation
 *)

module Parser = struct
  (* Option and_then operator for clean parser composition *)

  let ( let* ) = Option.and_then

  (* Parse 2 digits starting at position *)

  let parse_2digits = fun s pos ->
    if pos + 2 > String.length s then
      None
    else
      Int.parse (String.sub s pos 2)

  (* Parse 4 digits starting at position *)

  let parse_4digits = fun s pos ->
    if pos + 4 > String.length s then
      None
    else
      Int.parse (String.sub s pos 4)

  (* Check if format is extended (has separators) *)

  let is_extended_format = fun s pos -> pos + 4 < String.length s && s.[pos + 4] = '-'

  (* Parse date component: YYYY-MM-DD or YYYYMMDD *)

  let parse_date = fun s pos is_extended ->
    if is_extended then
      let* year = parse_4digits s pos in
      if pos + 4 >= String.length s || s.[pos + 4] != '-' then
        None
      else
        let* month = parse_2digits s (pos + 5) in
        if pos + 7 >= String.length s || s.[pos + 7] != '-' then
          None
        else
          let* day = parse_2digits s (pos + 8) in
          Some (year, month, day, pos + 10)
    else
      (* YYYYMMDD *)
      let* year = parse_4digits s pos in
      let* month = parse_2digits s (pos + 4) in
      let* day = parse_2digits s (pos + 6) in
      Some (year, month, day, pos + 8)

  (* Parse time component: HH:MM:SS or HHMMSS *)

  let parse_time = fun s pos is_extended ->
    if is_extended then
      let* hour = parse_2digits s pos in
      if pos + 2 >= String.length s || s.[pos + 2] != ':' then
        None
      else
        let* minute = parse_2digits s (pos + 3) in
        if pos + 5 >= String.length s || s.[pos + 5] != ':' then
          None
        else
          let* second = parse_2digits s (pos + 6) in
          Some (hour, minute, second, pos + 8)
    else
      (* HHMMSS *)
      let* hour = parse_2digits s pos in
      let* minute = parse_2digits s (pos + 2) in
      let* second = parse_2digits s (pos + 4) in
      Some (hour, minute, second, pos + 6)

  (* Parse microseconds (fractional seconds) *)

  let parse_microseconds = fun s pos ->
    let len = String.length s in
    if pos >= len || (s.[pos] != '.' && s.[pos] != ',') then
      ((0, 0), pos)
    else
      let start = pos + 1 in
      let rec find_end p =
        if p >= len || s.[p] < '0' || s.[p] > '9' then
          p
        else
          find_end (p + 1)
      in
      let end_pos = find_end start in
      if end_pos = start then
        raise (Failure "Empty microseconds after decimal separator")
      else
        let micro_str = String.sub s start (end_pos - start) in
        match Int.parse micro_str with
        | None ->
            raise (Failure "Invalid microseconds after decimal separator")
        | Some micros ->
            let precision = String.length micro_str in
            let micros = micros * (int_of_float (10.0 ** float (6 - precision))) in
            ((micros, 6), end_pos)

  (* Parse timezone offset *)

  let parse_timezone = fun s pos ->
    let len = String.length s in
    if pos >= len then
      (Tz.Etc_UTC, 0, pos)
    else if s.[pos] = 'Z' then
      (Tz.Etc_UTC, 0, pos + 1)
    else if s.[pos] = '+' || s.[pos] = '-' then
      let sign =
        if s.[pos] = '+' then
          1
        else
          (-1)
      in
      let has_colon = pos + 3 < len && s.[pos + 3] = ':' in
      let tz_result =
        if has_colon then
          let* h = parse_2digits s (pos + 1) in
          let* m = parse_2digits s (pos + 4) in
          Some (h, m, pos + 6)
        else
          (* ±HHMM *)
          let* h = parse_2digits s (pos + 1) in
          let* m = parse_2digits s (pos + 3) in
          Some (h, m, pos + 5)
      in
      match tz_result with
      | Some (h, m, end_pos) when h >= 0 && h <= 23 && m >= 0 && m <= 59 ->
          let offset = sign * ((h * 3_600) + (m * 60)) in
          (Tz.Local, offset, end_pos)
      | Some (h, _, _) when h < 0 || h > 23 ->
          raise (Failure ("Invalid timezone hour: " ^ string_of_int h))
      | Some (_, m, _) when m < 0 || m > 59 ->
          raise (Failure ("Invalid timezone minute: " ^ string_of_int m))
      | _ ->
          raise (Failure "Invalid timezone format")
    else
      raise (Failure "Invalid timezone format")

  (* Validate date components *)

  let validate_date = fun year month day ->
    if month < 1 || month > 12 then
      Error (Invalid_date ("Invalid month: " ^ string_of_int month))
    else if day < 1 || day > 31 then
      Error (Invalid_date ("Invalid day: " ^ string_of_int day))
    else
      let max_days =
        match month with
        | 2 ->
            let abs_year = abs year in
            if (abs_year mod 4 = 0 && abs_year mod 100 != 0) || abs_year mod 400 = 0 then
              29
            else
              28
        | 4
        | 6
        | 9
        | 11 ->
            30
        | _ ->
            31
      in
      if day > max_days then
        Error (Invalid_date ("Invalid day " ^ string_of_int day ^ " for month " ^ string_of_int month))
      else
        Ok ()

  (* Validate time components *)

  let validate_time = fun hour minute second ->
    if hour < 0 || hour > 23 then
      Error (Invalid_time ("Invalid hour: " ^ string_of_int hour))
    else if minute < 0 || minute > 59 then
      Error (Invalid_time ("Invalid minute: " ^ string_of_int minute))
    else if second < 0 || second > 59 then
      Error (Invalid_time ("Invalid second: " ^ string_of_int second))
    else
      Ok ()
end

let parse = fun s ->
  try
    let len = String.length s in
    if len < 16 then
      Error (Invalid_format "String too short for ISO 8601 datetime")
    else
      (* Parse year sign (optional + or -) *)
      let year_sign, pos =
        match s.[0] with
        | '-' -> ((-1), 1)
        | '+' -> (1, 1)
        | _ -> (1, 0)
      in
      (* Detect format (extended has separators, basic doesn't) *)
      let is_extended = Parser.is_extended_format s pos in
      (* Parse date: YYYY-MM-DD or YYYYMMDD *)
      let year, month, day, pos =
        match Parser.parse_date s pos is_extended with
        | None -> raise (Failure "Invalid date format")
        | Some (y, m, d, p) -> (year_sign * y, m, d, p)
      in
      (* Validate date *)
      (
        match Parser.validate_date year month day with
        | Error e -> Error e
        | Ok () ->
            (* Parse datetime separator (T or space) *)
            if pos >= len then
              Error (Invalid_format "Missing time component")
            else if s.[pos] != 'T' && s.[pos] != ' ' then
              Error (Invalid_format "Expected 'T' or ' ' separator")
            else
              let pos = pos + 1 in
              (* Parse time: HH:MM:SS or HHMMSS *)
              let hour, minute, second, pos =
                match Parser.parse_time s pos is_extended with
                | None -> raise (Failure "Invalid time format")
                | Some (h, m, s, p) -> (h, m, s, p)
              in
              (* Validate time *)
              match Parser.validate_time hour minute second with
              | Error e -> Error e
              | Ok () ->
                  (* Parse optional microseconds *)
                  let microseconds, pos = Parser.parse_microseconds s pos in
                  (* Parse timezone *)
                  let time_zone, utc_offset, _pos = Parser.parse_timezone s pos in
                  (* Validate timezone offset *)
                  if time_zone = Tz.Local && (utc_offset < (-86_400) || utc_offset > 86_400) then
                    Error (Invalid_timezone "Timezone offset too large")
                  else
                    Ok {
                      year;
                      month;
                      day;
                      hour;
                      minute;
                      second;
                      microseconds;
                      time_zone;
                      utc_offset;
                      std_offset = 0;
                    }
      )
  with
  | Failure msg ->
      if String.starts_with ~prefix:"Invalid timezone" msg then
        Error (Invalid_timezone msg)
      else
        Error (Invalid_format msg)
  | Invalid_argument msg -> Error (Invalid_format msg)

let epoch = Time.SystemTime.epoch |> from_system_time
