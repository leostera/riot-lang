type token =
  | Token

let keep value = value

module Inner = struct
  type outer = token

  let value = Token
end

module Nested = struct
  type alias = token

  let keep = keep
  let value : alias = keep Inner.value
end
