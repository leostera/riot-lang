let compose f g x = f (g x)
let inc x = x + 1
let double x = x * 2
let inc_then_double = compose double inc
