(** Option type utilities *)

type 'a t = 'a option = None | Some of 'a

(** {1 Constructors} *)

val some : 'a -> 'a t
(** Create a Some value *)

val none : 'a t
(** The None value *)

(** {1 Querying} *)

val is_some : 'a t -> bool
(** Returns true if the option is Some *)

val is_none : 'a t -> bool
(** Returns true if the option is None *)

val is_some_and : ('a -> bool) -> 'a t -> bool
(** Returns true if the option is Some and the value matches the predicate *)

val is_none_or : ('a -> bool) -> 'a t -> bool
(** Returns true if the option is None or the value matches the predicate *)

(** {1 Transforming} *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** Maps a Some value, leaving None untouched *)

val map_or : default:'b -> ('a -> 'b) -> 'a t -> 'b
(** Returns the result of applying function to Some value, or default if None *)

val map_or_default : default:(unit -> 'b) -> ('a -> 'b) -> 'a t -> 'b
(** Returns the result of applying function to Some value, or computes default if None *)

val map_or_else : default:(unit -> 'b) -> ('a -> 'b) -> 'a t -> 'b
(** Returns the result of applying function to Some value, or computes default if None *)

(** {1 Chaining} *)

val and_ : 'a t -> 'b t -> 'b t
(** Returns None if the first option is None, otherwise returns the second option *)

val and_then : 'a t -> ('a -> 'b t) -> 'b t
(** Chains another operation if Some, short-circuits on None *)

val or_ : 'a t -> 'a t -> 'a t
(** Returns the first option if Some, otherwise returns the second option *)

val or_else : 'a t -> (unit -> 'a t) -> 'a t
(** Returns the option if Some, otherwise calls a function *)

val xor : 'a t -> 'a t -> 'a t
(** Returns Some if exactly one of the options is Some, otherwise None *)

(** {1 Extracting values} *)

val unwrap : 'a t -> 'a
(** Get the Some value or panic *)

val unwrap_or : default:'a -> 'a t -> 'a
(** Get the Some value or return default *)

val unwrap_or_else : fn:(unit -> 'a) -> 'a t -> 'a
(** Get the Some value or compute it with a function *)

val expect : msg:string -> 'a t -> 'a
(** Get the Some value or panic with a custom message *)

val unwrap_none : 'a t -> unit
(** Panic if the option is Some *)

(** {1 Inspecting} *)

val inspect : ('a -> unit) -> 'a t -> 'a t
(** Calls function on Some value if present, returns unchanged option *)

(** {1 Iterating} *)

val iter : ('a -> unit) -> 'a t -> unit
(** Calls function on Some value if present *)

(** {1 Converting} *)

val ok_or : error:'e -> 'a t -> ('a, 'e) Result.t
(** Convert Some to Ok, None to Error *)

val ok_or_else : error:(unit -> 'e) -> 'a t -> ('a, 'e) Result.t
(** Convert Some to Ok, None to Error computed from function *)

val to_result : error:'e -> 'a t -> ('a, 'e) Result.t
(** Alias for ok_or *)

val to_list : 'a t -> 'a list
(** Convert Some to single-element list, None to empty list *)

val transpose : ('a, 'e) Result.t t -> ('a t, 'e) Result.t
(** Transpose Option of Result to Result of Option *)

(** {1 Filtering} *)

val filter : ('a -> bool) -> 'a t -> 'a t
(** Returns Some if the value matches predicate, otherwise None *)

(** {1 Flattening} *)

val flatten : 'a t t -> 'a t
(** Flatten nested Options *)

(** {1 Zipping} *)

val zip : 'a t -> 'b t -> ('a * 'b) t
(** Combine two options into an option of a tuple *)

val zip_with : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
(** Combine two options with a function *)

val unzip : ('a * 'b) t -> 'a t * 'b t
(** Unzip an option of a tuple into a tuple of options *)

(** {1 Collecting} *)

val all : 'a t list -> 'a list t
(** Convert list of Options to Option of list, returns None if any is None *)
