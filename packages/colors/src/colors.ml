open Std
open Std.Collections

type ansi = [`ansi of int]

type rgb = [`rgb of int * int * int]

type lrgb = [`lrgb of float * float * float]

type xyz = [`xyz of float * float * float]

type luv = [`luv of float * float * float]

type uv = [`uv of float * float]

type color = [ansi | rgb | lrgb | xyz | luv | uv]

module Internal = struct
  (* Byte-domain sRGB helpers. *)

  module Byte_rgb = struct
    let min_channel = 0

    let max_channel = 255

    let max_channel_f = 255.0

    let clamp_channel = fun value -> Int.min max_channel (Int.max min_channel value)

    let clamp = fun (`rgb (red, green, blue)) -> (
      clamp_channel red,
      clamp_channel green,
      clamp_channel blue
    )

    let normalize = fun rgb ->
      let (red, green, blue) = clamp rgb in
      `rgb (red, green, blue)

    let distance_squared = fun
      (left_red, left_green, left_blue) (right_red, right_green, right_blue) ->
      let diff_red = left_red - right_red in
      let diff_green = left_green - right_green in
      let diff_blue = left_blue - right_blue in
      (diff_red * diff_red) + (diff_green * diff_green) + (diff_blue * diff_blue)
  end

  (* Shared float-domain helpers for clamping and interpolation. *)

  module Float_domain = struct
    let clamp = fun ~min:low ~max:high value ->
      if Float.is_nan value then
        low
      else if value < low then
        low
      else if value > high then
        high
      else
        value

    let lerp = fun left right mix -> left +. (mix *. (right -. left))
  end

  (* Core XYZ conversion and chromaticity math. *)

  module XYZ_space = struct
    let chromaticity = fun x y z ->
      let denom = x +. (15.0 *. y) +. (3.0 *. z) in
      if Float.equal denom 0.0 then
        `uv (0.0, 0.0)
      else
        `uv (4.0 *. x /. denom, 9.0 *. y /. denom)

    let linear_rgb_to_xyz = fun (`lrgb (red, green, blue)) ->
      let x =
        (0.412_390_799_265_959_48 *. red)
        +. (0.357_584_339_383_877_96 *. green)
        +. (0.180_480_788_401_834_29 *. blue)
      in
      let y =
        (0.212_639_005_871_510_36 *. red)
        +. (0.715_168_678_767_755_93 *. green)
        +. (0.072_192_315_360_733_715 *. blue)
      in
      let z =
        (0.019_330_818_715_591_851 *. red)
        +. (0.119_194_779_794_625_99 *. green)
        +. (0.950_532_152_249_660_58 *. blue)
      in
      `xyz (x, y, z)

    let xyz_to_linear_rgb = fun (`xyz (x, y, z)) ->
      let red =
        (3.240_969_941_904_521_4 *. x)
        -. (1.537_383_177_570_093_5 *. y)
        -. (0.498_610_760_293_003_28 *. z)
      in
      let green =
        ((-0.969_243_636_280_879_83) *. x)
        +. (1.875_967_501_507_720_7 *. y)
        +. (0.041_555_057_407_175_613 *. z)
      in
      let blue =
        (0.055_630_079_696_993_609 *. x) -. (0.203_976_958_888_976_57 *. y)
        +. (1.056_971_514_242_878_6 *. z)
      in
      `lrgb (red, green, blue)
  end

  (* Named white references and validation for custom references. *)

  module White_points = struct
    type named_reference = {
      xyz: xyz;
      uv: uv;
    }

    let make = fun ~x ~y ~z ~u ~v ->
      {
        xyz = `xyz (x, y, z);
        uv = `uv (u, v);
      }

    let d50 =
      make
        ~x:0.964_22
        ~y:1.000_00
        ~z:0.825_21
        ~u:0.209_160_052_820_386_27
        ~v:0.488_073_384_544_885_14

    let d55 =
      make
        ~x:0.956_82
        ~y:1.000_00
        ~z:0.921_49
        ~u:0.204_434_630_305_924_43
        ~v:0.480_736_103_121_099_05

    let d65 =
      make
        ~x:0.950_47
        ~y:1.000_00
        ~z:1.088_83
        ~u:0.197_839_824_821_407_77
        ~v:0.468_336_302_932_409_7

    let d75 =
      make
        ~x:0.949_72
        ~y:1.000_00
        ~z:1.226_38
        ~u:0.193_535_437_106_383_16
        ~v:0.458_508_543_033_064_6

    let equal_energy =
      make
        ~x:1.000_00
        ~y:1.000_00
        ~z:1.000_00
        ~u:0.210_526_315_789_473_67
        ~v:0.473_684_210_526_315_76

    let known = [ d50; d55; d65; d75; equal_energy; ]

    let rec find_known_uv = fun reference named ->
      match named with
      | [] -> None
      | { xyz; uv } :: rest ->
          if xyz = reference then
            Some uv
          else
            find_known_uv reference rest

    let validate = fun (`xyz (x, y, z) as reference) ->
      let denom = x +. (15.0 *. y) +. (3.0 *. z) in
      if not (Float.is_finite x && Float.is_finite y && Float.is_finite z) then
        raise (Invalid_argument "Colors: white reference must be finite")
      else if y <= 0.0 then
        raise (Invalid_argument "Colors: white reference Y must be positive")
      else if denom <= 0.0 then
        raise (Invalid_argument "Colors: white reference must define valid UV chromaticity")
      else
        reference

    let uv_of_reference = fun reference ->
      match find_known_uv reference known with
      | Some uv -> uv
      | None ->
          match reference with
          | `xyz (x, y, z) -> XYZ_space.chromaticity x y z
  end

  (* Standard sRGB transfer curve helpers. *)

  module Transfer_curve = struct
    let forward_threshold = 0.040_45

    let inverse_threshold = 0.003_130_8

    let linear_scale = 12.92

    let gamma_offset = 0.055

    let gamma_scale = 1.055

    let decode_channel = fun channel ->
      let normalized = Float.from_int (Byte_rgb.clamp_channel channel) /. Byte_rgb.max_channel_f in
      if normalized <= forward_threshold then
        normalized /. linear_scale
      else
        Float.pow ((normalized +. gamma_offset) /. gamma_scale) 2.4

    let encode_channel = fun channel ->
      let clamped = Float_domain.clamp ~min:0.0 ~max:1.0 channel in
      let encoded =
        if clamped <= inverse_threshold then
          linear_scale *. clamped
        else
          (gamma_scale *. Float.pow clamped (1.0 /. 2.4)) -. gamma_offset
      in
      encoded *. Byte_rgb.max_channel_f
      |> Float.round
      |> Float.to_int
      |> Byte_rgb.clamp_channel
  end

  (* Normalized CIE LUV lightness helpers. *)

  module LUV_space = struct
    let epsilon = 216.0 /. 24_389.0

    let kappa = 24_389.0 /. 27.0

    let normalized_lightness_of_y_ratio = fun y_ratio ->
      if y_ratio <= epsilon then
        (kappa *. y_ratio) /. 100.0
      else
        (1.16 *. Float.cbrt y_ratio) -. 0.16

    let y_of_normalized_lightness = fun ~white_y lightness ->
      if lightness <= 0.08 then
        white_y *. lightness *. 100.0 /. kappa
      else
        let cube_root = (lightness +. 0.16) /. 1.16 in
        white_y *. cube_root *. cube_root *. cube_root
  end

  (* `#rrggbb` RGB codec helpers. *)

  module Hex_rgb = struct
    let digit_char = fun value ->
      if value < 10 then
        Char.from_int_unchecked (Char.to_int '0' + value)
      else
        Char.from_int_unchecked (Char.to_int 'a' + (value - 10))

    let digit_value = fun digit ->
      match Char.lowercase_ascii digit with
      | '0' .. '9' as value -> Ok (Char.to_int value - Char.to_int '0')
      | 'a' .. 'f' as value -> Ok (10 + Char.to_int value - Char.to_int 'a')
      | _ -> Error ("invalid hex digit: " ^ String.make ~len:1 ~char:digit)

    let parse_byte = fun value ~offset ->
      match (
        digit_value (String.get_unchecked value ~at:offset),
        digit_value (String.get_unchecked value ~at:(offset + 1))
      ) with
      | (Ok high, Ok low) -> Ok ((high * 16) + low)
      | (Error message, _) -> Error message
      | (_, Error message) -> Error message

    let from_string = fun value ->
      let trimmed = String.trim value in
      let normalized =
        if String.starts_with ~prefix:"#" trimmed then
          String.sub trimmed ~offset:1 ~len:(String.length trimmed - 1)
        else
          trimmed
      in
      if String.length normalized != 6 then
        Error "expected a 6-digit RGB hex string"
      else
        match (
          parse_byte normalized ~offset:0,
          parse_byte normalized ~offset:2,
          parse_byte normalized ~offset:4
        ) with
        | (Ok red, Ok green, Ok blue) -> Ok (`rgb (red, green, blue))
        | (Error message, _, _) -> Error message
        | (_, Error message, _) -> Error message
        | (_, _, Error message) -> Error message

    let to_string = fun rgb ->
      let (red, green, blue) = Byte_rgb.clamp rgb in
      let byte_to_hex value =
        let high = value / 16 in
        let low = value mod 16 in
        String.make ~len:1 ~char:(digit_char high) ^ String.make ~len:1 ~char:(digit_char low)
      in
      "#" ^ byte_to_hex red ^ byte_to_hex green ^ byte_to_hex blue
  end

  (* Inclusive gradients and clamped interpolation policy. *)

  module Interpolation = struct
    let clamp_mix = fun mix -> Float_domain.clamp ~min:0.0 ~max:1.0 mix

    let inclusive_gradient = fun ~steps ~start ~finish ~blend_unclamped ->
      if steps <= 0 then
        [||]
      else if steps = 1 then
        [|start|]
      else
        Array.init
          ~count:steps
          ~fn:(fun index ->
            if index = 0 then
              start
            else if index = steps - 1 then
              finish
            else
              let mix = Float.from_int index /. Float.from_int (steps - 1) in
              blend_unclamped start finish ~mix)
  end

  (* Debug formatting shared by the public API. *)

  module Format = struct
    let color = fun value ->
      match value with
      | `ansi index -> "ANSI(" ^ Int.to_string index ^ ")"
      | `rgb (red, green, blue) ->
          "RGB(" ^ Int.to_string red ^ "," ^ Int.to_string green ^ "," ^ Int.to_string blue ^ ")"
      | `lrgb (red, green, blue) ->
          "LinearRGB("
          ^ Float.to_string red
          ^ ","
          ^ Float.to_string green
          ^ ","
          ^ Float.to_string blue
          ^ ")"
      | `xyz (x, y, z) ->
          "XYZ(" ^ Float.to_string x ^ "," ^ Float.to_string y ^ "," ^ Float.to_string z ^ ")"
      | `luv (lightness, u, v) ->
          "LUV("
          ^ Float.to_string lightness
          ^ ","
          ^ Float.to_string u
          ^ ","
          ^ Float.to_string v
          ^ ")"
      | `uv (u, v) -> "UV(" ^ Float.to_string u ^ "," ^ Float.to_string v ^ ")"
  end
end

let to_string = Internal.Format.color

module ANSI = struct
  let max_index = Array.length Ansi_table.to_rgb - 1

  let to_rgb = fun (`ansi index) ->
    let clamped = Int.min max_index (Int.max 0 index) in
    Array.get_unchecked Ansi_table.to_rgb ~at:clamped

  let nearest = fun rgb ->
    let source = Internal.Byte_rgb.clamp rgb in
    let rec scan index best_index best_distance =
      if index >= Array.length Ansi_table.to_rgb then
        `ansi best_index
      else
        let (`rgb (red, green, blue)) = Array.get_unchecked Ansi_table.to_rgb ~at:index in
        let distance = Internal.Byte_rgb.distance_squared source (red, green, blue) in
        if distance < best_distance then
          scan (index + 1) index distance
        else
          scan (index + 1) best_index best_distance
    in
    let (`rgb (red, green, blue)) = Array.get_unchecked Ansi_table.to_rgb ~at:0 in
    scan
      1
      0
      (Internal.Byte_rgb.distance_squared source (red, green, blue))
end

module White_reference = struct
  let d50 = Internal.White_points.d50.xyz

  let d55 = Internal.White_points.d55.xyz

  let d65 = Internal.White_points.d65.xyz

  let d75 = Internal.White_points.d75.xyz

  let equal_energy = Internal.White_points.equal_energy.xyz
end

module Linear_RGB = struct
  let linearize = fun (`rgb (red, green, blue)) ->
    `lrgb (
      Internal.Transfer_curve.decode_channel red,
      Internal.Transfer_curve.decode_channel green,
      Internal.Transfer_curve.decode_channel blue
    )

  let delinearize = fun (`lrgb (red, green, blue)) ->
    `rgb (
      Internal.Transfer_curve.encode_channel red,
      Internal.Transfer_curve.encode_channel green,
      Internal.Transfer_curve.encode_channel blue
    )

  let to_xyz = Internal.XYZ_space.linear_rgb_to_xyz
end

module XYZ = struct
  let to_linear_rgb = Internal.XYZ_space.xyz_to_linear_rgb

  let to_rgb = fun xyz ->
    to_linear_rgb xyz
    |> Linear_RGB.delinearize

  let to_uv = fun (`xyz (x, y, z)) -> Internal.XYZ_space.chromaticity x y z

  let to_luv_with_ref = fun (`xyz (_, y, _) as xyz) ~wref ->
    let (`xyz (_, white_y, _) as reference) = Internal.White_points.validate wref in
    let lightness = Internal.LUV_space.normalized_lightness_of_y_ratio (y /. white_y) in
    let (`uv (u_prime, v_prime)) = to_uv xyz in
    let (`uv (white_u, white_v)) = Internal.White_points.uv_of_reference reference in
    let u = 13.0 *. lightness *. (u_prime -. white_u) in
    let v = 13.0 *. lightness *. (v_prime -. white_v) in
    `luv (lightness, u, v)

  let to_luv = fun xyz -> to_luv_with_ref xyz ~wref:White_reference.d65
end

module LUV = struct
  let distance = fun
    (`luv (left_lightness, left_u, left_v)) (`luv (right_lightness, right_u, right_v)) ->
    let diff_lightness = right_lightness -. left_lightness in
    let diff_u = right_u -. left_u in
    let diff_v = right_v -. left_v in
    Float.sqrt ((diff_lightness *. diff_lightness) +. (diff_u *. diff_u) +. (diff_v *. diff_v))

  let to_xyz_with_ref = fun (`luv (lightness, u, v)) ~wref ->
    let (`xyz (_, white_y, _) as reference) = Internal.White_points.validate wref in
    let y = Internal.LUV_space.y_of_normalized_lightness ~white_y lightness in
    let (`uv (white_u, white_v)) = Internal.White_points.uv_of_reference reference in
    if Float.equal lightness 0.0 then
      `xyz (0.0, 0.0, 0.0)
    else
      let u_prime = (u /. (13.0 *. lightness)) +. white_u in
      let v_prime = (v /. (13.0 *. lightness)) +. white_v in
      let x = y *. 9.0 *. u_prime /. (4.0 *. v_prime) in
      let z = y *. (12.0 -. (3.0 *. u_prime) -. (20.0 *. v_prime)) /. (4.0 *. v_prime) in
      `xyz (x, y, z)

  let to_xyz = fun luv -> to_xyz_with_ref luv ~wref:White_reference.d65

  let to_rgb = fun luv ->
    to_xyz luv
    |> XYZ.to_rgb

  let blend_unclamped = fun
    (`luv (left_lightness, left_u, left_v) as left)
    (`luv (right_lightness, right_u, right_v) as right)
    ~mix ->
    if Float.equal mix 0.0 then
      left
    else if Float.equal mix 1.0 then
      right
    else
      `luv (
        Internal.Float_domain.lerp left_lightness right_lightness mix,
        Internal.Float_domain.lerp left_u right_u mix,
        Internal.Float_domain.lerp left_v right_v mix
      )

  let blend = fun left right ~mix ->
    blend_unclamped
      left
      right
      ~mix:(Internal.Interpolation.clamp_mix mix)

  let gradient = fun start finish ~steps ->
    Internal.Interpolation.inclusive_gradient
      ~steps
      ~start
      ~finish
      ~blend_unclamped
end

module RGB = struct
  let to_linear_rgb = Linear_RGB.linearize

  let to_xyz = fun rgb ->
    to_linear_rgb rgb
    |> Linear_RGB.to_xyz

  let to_luv = fun rgb ->
    to_xyz rgb
    |> XYZ.to_luv

  let distance_luv = fun left right -> LUV.distance (to_luv left) (to_luv right)

  let relative_luminance = fun rgb ->
    match to_xyz rgb with
    | `xyz (_, y, _) -> y

  let contrast_ratio = fun left right ->
    let left_luminance = relative_luminance left in
    let right_luminance = relative_luminance right in
    if left_luminance >= right_luminance then
      (left_luminance +. 0.05) /. (right_luminance +. 0.05)
    else
      (right_luminance +. 0.05) /. (left_luminance +. 0.05)

  let from_hex = Internal.Hex_rgb.from_string

  let to_hex = Internal.Hex_rgb.to_string

  let blend_unclamped = fun left right ~mix ->
    let left = Internal.Byte_rgb.normalize left in
    let right = Internal.Byte_rgb.normalize right in
    if left = right then
      left
    else if Float.equal mix 0.0 then
      left
    else if Float.equal mix 1.0 then
      right
    else
      let left_luv = to_luv left in
      let right_luv = to_luv right in
      LUV.blend_unclamped left_luv right_luv ~mix
      |> LUV.to_rgb

  let blend = fun left right ~mix ->
    blend_unclamped
      left
      right
      ~mix:(Internal.Interpolation.clamp_mix mix)

  let gradient = fun start finish ~steps ->
    let start = Internal.Byte_rgb.normalize start in
    let finish = Internal.Byte_rgb.normalize finish in
    Internal.Interpolation.inclusive_gradient ~steps ~start ~finish ~blend_unclamped
end
