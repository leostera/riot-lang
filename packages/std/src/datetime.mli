(** Date and time utilities *)

module Tz : sig
  type t = 
    | Etc_UTC
    | Local
    
  val to_string : t -> string
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

val now : unit -> t
(** Get current time in local timezone *)

val now_utc : unit -> t
(** Get current time in UTC *)

val from_unix_time : float -> t
(** Create datetime from Unix timestamp *)

val to_unix : t -> float
(** Convert to Unix timestamp (seconds since epoch with fractional part) *)

val to_iso8601 : t -> string
(** Convert to ISO 8601 format string, e.g. "2025-08-27T21:07:31.426Z" *)

