type token =
  | Token

let keep value = value

module Inner = struct
  type outer = token

  let value = Token
end
