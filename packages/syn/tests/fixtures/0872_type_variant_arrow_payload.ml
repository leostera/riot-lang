type action =
  Callback of (int -> unit)
  | Value of int
