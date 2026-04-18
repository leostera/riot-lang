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
  | lrgb
  | xyz
  | luv
  | uv
]

let rgb_channel_min = 0

let rgb_channel_max = 255

let rgb_channel_max_f = 255.0

let srgb_forward_threshold = 0.040_45

let srgb_inverse_threshold = 0.003_130_8

let srgb_linear_scale = 12.92

let srgb_gamma_offset = 0.055

let srgb_gamma_scale = 1.055

let cie_epsilon = 216.0 /. 24389.0

let cie_kappa = 24389.0 /. 27.0

let d65_x = 0.950_47

let d65_y = 1.000_00

let d65_z = 1.088_83

let d65_u = 0.197_839_824_821_407_77

let d65_v = 0.468_336_302_932_409_7

let d65_white = `xyz (d65_x, d65_y, d65_z)

let d65_white_uv = `uv (d65_u, d65_v)

let clamp_int = fun ~min:low ~max:high value ->
  Int.min high (Int.max low value)

let clamp_rgb = fun (`rgb (r, g, b)) ->
  (
    clamp_int ~min:rgb_channel_min ~max:rgb_channel_max r,
    clamp_int ~min:rgb_channel_min ~max:rgb_channel_max g,
    clamp_int ~min:rgb_channel_min ~max:rgb_channel_max b
  )

let clamp_float = fun ~min:low ~max:high value ->
  if Float.is_nan value then
    low
  else if value < low then
    low
  else if value > high then
    high
  else
    value

let validate_white_reference = fun (`xyz (x, y, z) as wref) ->
  let denom = x +. (15.0 *. y) +. (3.0 *. z) in
  if not (Float.is_finite x && Float.is_finite y && Float.is_finite z) then
    raise (Invalid_argument "Colors: white reference must be finite")
  else if y <= 0.0 then
    raise (Invalid_argument "Colors: white reference Y must be positive")
  else if denom <= 0.0 then
    raise (Invalid_argument "Colors: white reference must define valid UV chromaticity")
  else
    wref

let uv_of_xyz = fun x y z ->
  let denom = x +. (15.0 *. y) +. (3.0 *. z) in
  if Float.equal denom 0.0 then
    `uv (0.0, 0.0)
  else
    `uv (4.0 *. x /. denom, 9.0 *. y /. denom)

let rgb_distance = fun (left_r, left_g, left_b) (right_r, right_g, right_b) ->
  let diff_r = left_r - right_r in
  let diff_g = left_g - right_g in
  let diff_b = left_b - right_b in
  (diff_r * diff_r) + (diff_g * diff_g) + (diff_b * diff_b)

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
    let i = clamp_int ~min:0 ~max:(Array.length Ansi_table.to_rgb - 1) i in
    Array.get_unchecked Ansi_table.to_rgb ~at:i

  let nearest = fun rgb ->
    let source = clamp_rgb rgb in
    let rec loop index best_index best_distance =
      if index >= Array.length Ansi_table.to_rgb then
        `ansi best_index
      else
        let (`rgb (red, green, blue)) = Array.get_unchecked Ansi_table.to_rgb ~at:index in
        let distance = rgb_distance source (red, green, blue) in
        if distance < best_distance then
          loop (index + 1) index distance
        else
          loop (index + 1) best_index best_distance
    in
    let (`rgb (red, green, blue)) = Array.get_unchecked Ansi_table.to_rgb ~at:0 in
    loop 1 0 (rgb_distance source (red, green, blue))
end

module White_reference = struct
  let d65 = d65_white
end

module Linear_RGB = struct
  let linearize_channel = fun channel ->
    let normalized = Float.from_int (clamp_int ~min:rgb_channel_min ~max:rgb_channel_max channel)
    /. rgb_channel_max_f in
    if normalized <= srgb_forward_threshold then
      normalized /. srgb_linear_scale
    else
      Float.pow ((normalized +. srgb_gamma_offset) /. srgb_gamma_scale) 2.4

  let linearize = fun (`rgb (r, g, b)) ->
    `lrgb (linearize_channel r, linearize_channel g, linearize_channel b)

  let delinearize_channel = fun channel ->
    let clamped = clamp_float ~min:0.0 ~max:1.0 channel in
    let encoded =
      if clamped <= srgb_inverse_threshold then
        srgb_linear_scale *. clamped
      else
        (srgb_gamma_scale *. Float.pow clamped (1.0 /. 2.4)) -. srgb_gamma_offset
    in
    encoded *. rgb_channel_max_f
    |> Float.round
    |> Float.to_int
    |> clamp_int ~min:rgb_channel_min ~max:rgb_channel_max

  let delinearize = fun (`lrgb (r, g, b)) ->
    `rgb (delinearize_channel r, delinearize_channel g, delinearize_channel b)

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

  let to_rgb = fun xyz -> to_linear_rgb xyz |> Linear_RGB.delinearize

  let to_uv = fun (`xyz (x, y, z)) -> uv_of_xyz x y z

  let to_luv_with_ref = fun (`xyz (_, y, _) as xyz) ~wref ->
    let (`xyz (_, wref_y, _) as wref) = validate_white_reference wref in
    let y_ratio = y /. wref_y in
    let l =
      if y_ratio <= cie_epsilon then
        (cie_kappa *. y_ratio) /. 100.0
      else
        (1.16 *. Float.cbrt y_ratio) -. 0.16
    in
    let (`uv (ubis, vbis)) = to_uv xyz in
    let (`uv (un, vn)) =
      if wref = d65_white then
        d65_white_uv
      else
        to_uv wref
    in
    let u = 13.0 *. l *. (ubis -. un) in
    let v = 13.0 *. l *. (vbis -. vn) in
    `luv (l, u, v)

  let to_luv = fun xyz -> to_luv_with_ref xyz ~wref:White_reference.d65
end

module LUV = struct
  let to_xyz_with_ref = fun (`luv (l, u, v)) ~wref ->
    let (`xyz (_, wref_y, _) as wref) = validate_white_reference wref in
    let y =
      if l <= 0.08 then
        wref_y *. l *. 100.0 /. cie_kappa
      else
        let cube_root = (l +. 0.16) /. 1.16 in
        wref_y *. cube_root *. cube_root *. cube_root
    in
    let (`uv (un, vn)) =
      if wref = d65_white then
        d65_white_uv
      else
        XYZ.to_uv wref
    in
    if Float.equal l 0.0 then
      `xyz (0.0, 0.0, 0.0)
    else
      let ubis = (u /. (13.0 *. l)) +. un in
      let vbis = (v /. (13.0 *. l)) +. vn in
      let x = y *. 9.0 *. ubis /. (4.0 *. vbis) in
      let z = y *. (12.0 -. (3.0 *. ubis) -. (20.0 *. vbis)) /. (4.0 *. vbis) in
      `xyz (x, y, z)

  let to_xyz = fun luv -> to_xyz_with_ref luv ~wref:White_reference.d65

  let to_rgb = fun luv -> to_xyz luv |> XYZ.to_rgb

  let blend = fun (`luv (l1, u1, v1)) (`luv (l2, u2, v2)) ~mix ->
    let mix = clamp_float ~min:0.0 ~max:1.0 mix in
    let l = l1 +. (mix *. (l2 -. l1)) in
    let u = u1 +. (mix *. (u2 -. u1)) in
    let v = v1 +. (mix *. (v2 -. v1)) in
    `luv (l, u, v)
end

module RGB = struct
  let to_linear_rgb = fun rgb -> Linear_RGB.linearize rgb

  let to_xyz = fun rgb -> to_linear_rgb rgb |> Linear_RGB.to_xyz

  let to_luv = fun rgb -> to_xyz rgb |> XYZ.to_luv

  let blend = fun c1 c2 ~mix ->
    let mix = clamp_float ~min:0.0 ~max:1.0 mix in
    let luv1 = to_luv c1 in
    let luv2 = to_luv c2 in
    LUV.blend luv1 luv2 ~mix |> LUV.to_rgb
end
