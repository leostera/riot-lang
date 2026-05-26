type 'a box = Box of 'a

let unbox (Box value) = value
