(** Low-level time operations for Kernel *)

type tm = {
  tm_sec : int;
  tm_min : int;
  tm_hour : int;
  tm_mday : int;
  tm_mon : int;
  tm_year : int;
  tm_wday : int;
  tm_yday : int;
  tm_isdst : bool;
}

external gettimeofday : unit -> float = "caml_kernel_gettimeofday"
external localtime : float -> tm = "caml_kernel_localtime"
external gmtime : float -> tm = "caml_kernel_gmtime"
external mktime : tm -> float * tm = "caml_kernel_mktime"
