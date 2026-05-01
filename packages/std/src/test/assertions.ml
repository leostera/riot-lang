open Global

let assert_equal = fun ~expected ~actual ->
  if expected != actual then
    panic
      ("Expected "
      ^ (Exception.to_string (Failure "expected"))
      ^ " but got "
      ^ (Exception.to_string (Failure "actual")))

let assert_ok = fun __tmp1 ->
  match __tmp1 with
  | Ok _ -> ()
  | Error _ -> panic "Expected Ok but got Error"

let assert_error = fun __tmp1 ->
  match __tmp1 with
  | Ok _ -> panic "Expected Error but got Ok"
  | Error _ -> ()

let assert_true = fun b ->
  if not b then
    panic "Expected true but got false"

let assert_false = fun b ->
  if b then
    panic "Expected false but got true"
