(** Result type for error handling *)

type ('a, 'e) t = ('a, 'e) Stdlib.result = Ok of 'a | Error of 'e

(** {1 Constructors} *)

val ok : 'a -> ('a, 'e) t
(** Create an Ok value *)

val err : 'e -> ('a, 'e) t
(** Create an Error value *)

(** {1 Querying} *)

val is_ok : ('a, 'e) t -> bool
(** Returns true if the result is Ok *)

val is_error : ('a, 'e) t -> bool
(** Returns true if the result is Error (alias for is_err) *)

val is_err : ('a, 'e) t -> bool
(** Returns true if the result is Error *)

val is_ok_and : ('a -> bool) -> ('a, 'e) t -> bool
(** Returns true if the result is Ok and the value matches the predicate *)

val is_err_and : ('e -> bool) -> ('a, 'e) t -> bool
(** Returns true if the result is Error and the error matches the predicate *)

(** {1 Transforming} *)

val map : ('a -> 'b) -> ('a, 'e) t -> ('b, 'e) t
(** Maps an Ok value, leaving Error untouched *)

val map_error : ('e -> 'f) -> ('a, 'e) t -> ('a, 'f) t
(** Maps an Error value, leaving Ok untouched (alias for map_err) *)

val map_err : ('e -> 'f) -> ('a, 'e) t -> ('a, 'f) t
(** Maps an Error value, leaving Ok untouched *)

val map_or : default:'b -> ('a -> 'b) -> ('a, 'e) t -> 'b
(** Returns the result of applying function to Ok value, or default if Error *)

val map_or_else : default:('e -> 'b) -> ('a -> 'b) -> ('a, 'e) t -> 'b
(** Returns the result of applying function to Ok value, or computes default
    from Error *)

(** {1 Chaining} *)

val and_then : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
(** Chains another operation if Ok, short-circuits on Error *)

val or_ : ('a, 'e) t -> ('a, 'e) t -> ('a, 'e) t
(** Returns the first result if Ok, otherwise returns the second result *)

val or_else : ('a, 'e) t -> ('e -> ('a, 'f) t) -> ('a, 'f) t
(** Chains another operation if Error, passes through Ok *)

(** {1 Extracting values} *)

val unwrap : ('a, 'e) t -> 'a
(** Get the Ok value or panic *)

val unwrap_or : default:'a -> ('a, 'e) t -> 'a
(** Get the Ok value or return default *)

val unwrap_or_else : fn:(unit -> 'a) -> ('a, 'e) t -> 'a
(** Get the Ok value or compute it with a function *)

val unwrap_err : ('a, 'e) t -> 'e
(** Get the Error value or panic *)

val expect : msg:string -> ('a, 'e) t -> 'a
(** Get the Ok value or panic with a custom message *)

val expect_err : msg:string -> ('a, 'e) t -> 'e
(** Get the Error value or panic with a custom message *)

val ok_value : ('a, 'e) t -> 'a option
(** Convert Ok to Some, Error to None *)

val err_value : ('a, 'e) t -> 'e option
(** Convert Error to Some, Ok to None *)

(** {1 Inspecting} *)

val inspect : ('a -> unit) -> ('a, 'e) t -> ('a, 'e) t
(** Calls function on Ok value if present, returns unchanged result *)

val inspect_err : ('e -> unit) -> ('a, 'e) t -> ('a, 'e) t
(** Calls function on Error value if present, returns unchanged result *)

(** {1 Iterating} *)

val iter : ('a -> unit) -> ('a, 'e) t -> unit
(** Calls function on Ok value if present *)

val iter_error : ('e -> unit) -> ('a, 'e) t -> unit
(** Calls function on Error value if present *)

(** {1 Converting} *)

val to_option : ('a, 'e) t -> 'a option
(** Convert Ok to Some, Error to None *)

val of_option : error:'e -> 'a option -> ('a, 'e) t
(** Convert Some to Ok, None to Error *)

val transpose : ('a option, 'e) t -> ('a, 'e) t option
(** Transpose Result of Option to Option of Result *)

(** {1 Flattening} *)

val flatten : (('a, 'e) t, 'e) t -> ('a, 'e) t
(** Flatten nested Results *)

(** {1 Collecting} *)

val all : ('a, 'e) t list -> ('a list, 'e) t
(** Convert list of Results to Result of list, short-circuits on first Error *)

val both : ('a, 'e) t -> ('b, 'e) t -> ('a * 'b, 'e) t
(** Combine two Results into a Result of a tuple *)

(** {1 Misc} *)

val fold : ok:('a -> 'c) -> error:('e -> 'c) -> ('a, 'e) t -> 'c
(** Fold over the result *)
