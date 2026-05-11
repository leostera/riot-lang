open Std

module Config = Sqlite__Config
module Driver = Sqlite__Driver

module Error = struct
  type t = Driver.error

  let to_string = Driver.error_to_string

  let serializer = Driver.error_serializer
end

module Testing = Sqlite__Testing
