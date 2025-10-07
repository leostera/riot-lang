let list = [ 1; 2; 3 ]
let array = [| 1; 2; 3 |]
let tuple = (1, 2, 3)
let record = { x = 1; y = 2 }

let fn x =
  let y = x + 1 in
  y * 2
