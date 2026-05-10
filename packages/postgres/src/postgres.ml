open Std

module Error = struct
  type t = Postgres_driver.error

  let to_string = Postgres_driver.error_to_string

  let serializer = Postgres_driver.error_serializer
end

module Internal = struct
  module Protocol = Protocol
end

module Config = Postgres_config
module Driver = Postgres_driver
