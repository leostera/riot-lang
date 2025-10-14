open Global

let assert_equal ~expected ~actual =
  if expected <> actual then
    raise
      (Failure
         (format "Expected %s but got %s"
            (Exception.to_string (Failure "expected"))
            (Exception.to_string (Failure "actual"))))

let assert_ok = function
  | Ok _ -> ()
  | Error _ -> raise (Failure "Expected Ok but got Error")

let assert_error = function
  | Ok _ -> raise (Failure "Expected Error but got Ok")
  | Error _ -> ()

let assert_true b = if not b then raise (Failure "Expected true but got false")
let assert_false b = if b then raise (Failure "Expected false but got true")
