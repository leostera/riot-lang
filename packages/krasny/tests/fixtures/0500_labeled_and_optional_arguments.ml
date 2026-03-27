(* TODO(@leostera): we need to add more examples here for:
   - [x] defaults on `let foo ... = ..` functions
   - [x] pattern matching on named fields
   - [x] pattern matching and renaming captured fields/variables
   - [x] `~arg:name` patterns
   - [x] `?(arg = foo as name)` patterns
   *)

let labeled_arg_simple = configure ~timeout:30
let labeled_arg_shorthand = configure ~timeout
let labeled_arg_multiple = configure ~timeout:30 ~retries:3
let labeled_arg_with_unlabeled = connect address ~timeout:30 ~retries:3
let labeled_arg_parenthesized = configure ~style:(Style.Grow)

let labeled_param_simple =
  fun ~timeout -> timeout

let labeled_param_multiple =
  fun ~timeout ~retries -> timeout + retries

let optional_arg_explicit = configure ?timeout:(Some 30) ()
let optional_arg_shorthand = configure ?timeout ()
let optional_arg_none = configure ?timeout:None ()
let optional_arg_some = configure ?timeout:(Some value) ()
let optional_arg_parenthesized = configure ?timeout:(value) ()

let optional_param =
  fun ?timeout () -> timeout

let optional_param_default =
  fun ?(timeout = 30) () -> timeout

let optional_param_multiple =
  fun ?timeout ?(retries = 3) ~handler () -> handler retries timeout

let named_arg_pattern ~timeout:seconds =
  seconds + 1

let labeled_record_pattern ~point:{ x; y } =
  x + y

let labeled_record_pattern_renamed ~point:{ x = x_coord; y = y_coord } =
  x_coord + y_coord

let configure_defaults ?(timeout = 30) ?(retries = 3) ~handler () =
  handler timeout retries

let optional_alias ?timeout:chosen_timeout () =
  Option.value ~default:default_timeout chosen_timeout
