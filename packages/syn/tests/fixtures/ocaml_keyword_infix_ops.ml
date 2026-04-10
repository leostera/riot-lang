let modulo value = value mod 2

let mask value = value land 255

let merge left right = left lor right

let toggle value = value lxor (-1)

let shift_left value = value lsl 1

let shift_right_logical value = value lsr 1

let shift_right value = value asr 1
