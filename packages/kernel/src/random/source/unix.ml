type error =
  | System of System_error.t

module FFI = struct
  external fill_bytes: bytes -> (unit, int) Result.t = "kernel_new_random_source_fill"
end

let error_to_string = function
  | System error -> System_error.to_string error

let fill_bytes = fun bytes ->
  Result.map_err
    (FFI.fill_bytes bytes)
    ~fn:(fun code -> System (System_error.from_code code))
