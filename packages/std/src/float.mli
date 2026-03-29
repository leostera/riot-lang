include module type of Kernel.Float

(** [to_string ?precision f] converts a float to a string with the specified precision.
    
    The precision parameter controls the number of decimal places (default: 6).
    Values are rounded to the specified precision.
    
    Example:
    {[
      Float.to_string ~precision:2 3.14159  (* "3.14" *)
      Float.to_string ~precision:1 1.96     (* "2.0" *)
      Float.to_string 42.0                  (* "42.0" or similar *)
    ]}
*)
val to_string : ?precision:int -> float -> string
