open Std
open Std.Collections

type ansi =
[
  `ansi of int
]

type rgb =
[
  `rgb of int * int * int
]

type lrgb =
[
  `lrgb of float * float * float
]

type xyz =
[
  `xyz of float * float * float
]

type luv =
[
  `luv of float * float * float
]

type uv =
[
  `uv of float * float
]

type color = [
  ansi
  | rgb
  | xyz
  | luv
  | uv
]

let to_string = fun value ->
  match value with
  | `ansi i -> "ANSI(" ^ Int.to_string i ^ ")"
  | `rgb (r, g, b) -> "RGB(" ^ Int.to_string r ^ "," ^ Int.to_string g ^ "," ^ Int.to_string b ^ ")"
  | `lrgb (r, g, b) -> "LinearRGB("
  ^ Float.to_string r
  ^ ","
  ^ Float.to_string g
  ^ ","
  ^ Float.to_string b
  ^ ")"
  | `xyz (x, y, z) -> "XYZ("
  ^ Float.to_string x
  ^ ","
  ^ Float.to_string y
  ^ ","
  ^ Float.to_string z
  ^ ")"
  | `luv (l, u, v) -> "LUV("
  ^ Float.to_string l
  ^ ","
  ^ Float.to_string u
  ^ ","
  ^ Float.to_string v
  ^ ")"
  | `uv (u, v) -> "UV(" ^ Float.to_string u ^ "," ^ Float.to_string v ^ ")"

module ANSI = struct
  let to_rgb = fun (`ansi i) ->
    let i = Int.(min (max 0 i) (Array.length Ansi_table.to_rgb - 1)) in
    Array.get_unchecked Ansi_table.to_rgb ~at:i
end

module White_reference = struct
  let d65 = `xyz (0.950_47, 1.000_00, 1.088_83)
end

module Linear_RGB = struct
  let linearize = fun v ->
    if v < 0.040_45 then
      v /. 12.92
    else
      Float.pow ((v +. 0.055) /. 1.055) 2.4

  let linearize = fun (`rgb (r, g, b)) ->
    `lrgb (
      r |> Float.from_int |> linearize,
      g |> Float.from_int |> linearize,
      b |> Float.from_int |> linearize
    )

  let delinearize = fun v ->
    if v <= 0.003_130_8 then
      12.92 *. v
    else
      (1.055 *. Float.pow v (1.0 /. 2.4)) -. 0.055

  let delinearize = fun (`lrgb (r, g, b)) ->
    `rgb (
      r |> delinearize |> Float.to_int,
      g |> delinearize |> Float.to_int,
      b |> delinearize |> Float.to_int
    )

  let to_xyz = fun (`lrgb (r, g, b)) ->
    let x = (0.412_390_799_265_959_48 *. r)
    +. (0.357_584_339_383_877_96 *. g)
    +. (0.180_480_788_401_834_29 *. b) in
    let y = (0.212_639_005_871_510_36 *. r)
    +. (0.715_168_678_767_755_93 *. g)
    +. (0.072_192_315_360_733_715 *. b) in
    let z = (0.019_330_818_715_591_851 *. r)
    +. (0.119_194_779_794_625_99 *. g)
    +. (0.950_532_152_249_660_58 *. b) in
    `xyz (x, y, z)
end

module XYZ = struct
  let to_linear_rgb = fun (`xyz (x, y, z)) ->
    let r = (3.240_969_941_904_521_4 *. x)
    -. (1.537_383_177_570_093_5 *. y)
    -. (0.498_610_760_293_003_28 *. z) in
    let g = ((-0.969_243_636_280_879_83) *. x)
    +. (1.875_967_501_507_720_7 *. y)
    +. (0.041_555_057_407_175_613 *. z) in
    let b = (0.055_630_079_696_993_609 *. x) -. (0.203_976_958_888_976_57 *. y)
    +. (1.056_971_514_242_878_6 *. z) in
    `lrgb (r, g, b)

  let to_uv = fun (`xyz (x, y, z)) ->
    let denom = x +. (15.0 *. y) +. (3.0 *. z) in
    if denom = 0.0 then
      `uv (0.0, 0.0)
    else
      let u = 4.0 *. x /. denom in
      let v = 9.0 *. y /. denom in
      `uv (u, v)

  let to_luv_with_ref = fun (`xyz (_, y, _) as xyz) ~wref:(`xyz (_, wref1, _) as wref) ->
    let l =
      if y /. wref1 <= 6.0 /. 29.0 *. 6.0 /. 29.0 *. 6.0 /. 29.0 then
        y /. wref1 *. (29.0 /. 3.0 *. 29.0 /. 3.0 *. 29.0 /. 3.0) /. 100.0
      else
        (1.16 *. Float.cbrt (y /. wref1)) -. 0.16
    in
    let (`uv (ubis, vbis)) = to_uv xyz in
    let (`uv (un, vn)) = to_uv wref in
    let u = 13.0 *. l *. (ubis -. un) in
    let v = 13.0 *. l *. (vbis -. vn) in
    `luv (l, u, v)

  let to_luv = fun xyz -> to_luv_with_ref xyz ~wref:White_reference.d65
end

module LUV = struct
  let to_xyz_with_ref = fun (`luv (l, u, v)) ~wref:(`xyz (_, wref1, _) as wref) ->
    let y =
      if l <= 0.08 then
        wref1 *. l *. 100.0 *. 3.0 /. 29.0 *. 3.0 /. 29.0 *. 3.0 /. 29.0
      else
        wref1 *. Float.pow ((l +. 0.16) /. 1.16) 3.
    in
    let (`uv (un, vn)) = XYZ.to_uv wref in
    if l != 0.0 then
      let ubis = (u /. (13.0 *. l)) +. un in
      let vbis = (v /. (13.0 *. l)) +. vn in
      let x = y *. 9.0 *. ubis /. (4.0 *. vbis) in
      let z = y *. (12.0 -. (3.0 *. ubis) -. (20.0 *. vbis)) /. (4.0 *. vbis) in
      `xyz (x, y, z)
    else
      `xyz (0.0, 0.0, 0.0)

  let to_xyz = fun luv -> to_xyz_with_ref luv ~wref:White_reference.d65

  let blend = fun (`luv (l1, u1, v1)) (`luv (l2, u2, v2)) ~mix ->
    let mix = Float.(min (max 0. mix) 1.) in
    let l = l1 +. (mix *. (l2 -. l1)) in
    let u = u1 +. (mix *. (u2 -. u1)) in
    let v = v1 +. (mix *. (v2 -. v1)) in
    `luv (l, u, v)
end

module RGB = struct
  let blend = fun c1 c2 ~mix ->
    let mix = Float.(min (max 0. mix) 1.) in
    let luv1 = c1 |> Linear_RGB.linearize |> Linear_RGB.to_xyz |> XYZ.to_luv in
    let luv2 = c2 |> Linear_RGB.linearize |> Linear_RGB.to_xyz |> XYZ.to_luv in
    LUV.blend luv1 luv2 ~mix |> LUV.to_xyz |> XYZ.to_linear_rgb |> Linear_RGB.delinearize
end
