module White_reference = struct
  let d65 = 1
end

module Inner = struct
  let plus = fun x -> x + White_reference.d65

  let plus_twice = fun x -> plus (plus x)
end

let result = Inner.plus_twice 1
