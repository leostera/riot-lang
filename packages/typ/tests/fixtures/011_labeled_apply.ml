let choose = fun left ~right ->
  left + right

let pick = choose 1 ~right:2

let pick_existing = fun right ->
  choose 1 ~right
