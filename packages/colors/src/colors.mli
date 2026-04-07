(** Convert colors between ANSI, RGB, XYZ, and LUV, and blend colors in a
    perceptually uniform space.

    Reach for this package when you need:

    - terminal colors from ANSI palette indices
    - conversions between display and CIE color spaces
    - color interpolation that looks visually even to humans

    The most common high-level entrypoints are:

    - [ANSI.to_rgb] for terminal palette values
    - [RGB.blend] for perceptual blending between RGB colors
*)

(** ANSI 256-color palette entry. *)
type ansi =
[
  | `ansi of int
]

(** Standard RGB color with integer channels in the range `0..255`. *)
type rgb =
[
  | `rgb of int * int * int
]

(** Linear RGB color used for numeric color-space calculations. *)
type lrgb =
[
  | `lrgb of float * float * float
]

(** CIE 1931 XYZ color. This representation is device-independent. *)
type xyz =
[
  | `xyz of float * float * float
]

(** CIE LUV color. This space is designed to be perceptually uniform. *)
type luv =
[
  | `luv of float * float * float
]

(** Chromaticity coordinates derived from XYZ. *)
type uv =
[
  | `uv of float * float
]

(** Any supported color representation. *)
type color = [
  | ansi
  | rgb
  | xyz
  | luv
  | uv
]

(** Format a color for debugging and logs.

    Example:
    ```ocaml
    Colors.to_string (`rgb (255, 128, 0)) = "RGB(255,128,0)"
    ```
*)
val to_string: color -> string

module ANSI: sig
  (** Convert an ANSI palette entry to RGB.

      Use this when you need a concrete RGB value for a terminal color.

      Example:
      ```ocaml
      ANSI.to_rgb (`ansi 9) = `rgb (255, 0, 0)
      ```
  *)
  val to_rgb: ansi -> rgb
end

module White_reference: sig
  (** Standard D65 white point.

      This is the default daylight white reference used by the package for the
      common XYZ/LUV conversions.
  *)
  val d65: xyz
end

module Linear_RGB: sig
  (** Remove the sRGB gamma curve and convert an RGB color to linear RGB.

      Use this before matrix-based color-space conversions such as RGB to XYZ.

      Example:
      ```ocaml
      let linear = Linear_RGB.linearize (`rgb (128, 128, 128))
      (* returns something like `lrgb (0.2158, 0.2158, 0.2158) *)
      ```
  *)
  val linearize: rgb -> lrgb

  (** Re-apply the sRGB gamma curve and convert linear RGB back to RGB.

      This is the inverse of [linearize].
  *)
  val delinearize: lrgb -> rgb

  (** Convert linear RGB to XYZ.

      Use this when moving from display-oriented RGB values into a
      device-independent CIE color space.
  *)
  val to_xyz: lrgb -> xyz
end

module XYZ: sig
  (** Convert XYZ to linear RGB.

      This is the inverse of [Linear_RGB.to_xyz].
  *)
  val to_linear_rgb: xyz -> lrgb

  (** Convert XYZ to chromaticity coordinates.

      Use this when you only care about chromaticity and not luminance.
  *)
  val to_uv: xyz -> uv

  (** Convert XYZ to LUV with an explicit white reference.

      Use this when your working white point is not the default daylight
      reference.
  *)
  val to_luv_with_ref:
    xyz ->
    (** White reference to use for the conversion. *)
    wref:xyz ->
    luv

  (** Convert XYZ to LUV using [White_reference.d65]. *)
  val to_luv: xyz -> luv
end

module LUV: sig
  (** Convert LUV back to XYZ with an explicit white reference.

      The white reference should match the one used in the forward conversion.
  *)
  val to_xyz_with_ref:
    luv ->
    (** White reference to use for the conversion. *)
    wref:xyz ->
    xyz

  (** Convert LUV to XYZ using [White_reference.d65]. *)
  val to_xyz: luv -> xyz

  (** Blend two LUV colors.

      Use this when you are already working in LUV and want interpolation that
      changes at an even perceptual rate.

      [`mix`] controls the interpolation:

      - `0.0` returns the first color
      - `0.5` returns the midpoint
      - `1.0` returns the second color

      Example:
      ```ocaml
      let start = `luv (0.32, -0.09, -1.13) in
      let finish = `luv (0.97, 0.07, 0.68) in

      let midpoint = LUV.blend start finish ~mix:0.5
      ```
  *)
  val blend:
    luv ->
    luv ->
    (** Blend ratio between the two colors. *)
    mix:float ->
    luv
end

module RGB: sig
  (** Blend two RGB colors in perceptually uniform LUV space.

      This is the high-level blend function most callers want. It converts RGB
      into LUV, blends there, and converts the result back to RGB.

      Use this instead of averaging RGB channels directly when you want a
      visually smooth transition.

      Example:
      ```ocaml
      let blue = `rgb (0, 0, 255) in
      let yellow = `rgb (255, 255, 0) in

      let midpoint = RGB.blend blue yellow ~mix:0.5
      ```
  *)
  val blend:
    rgb ->
    rgb ->
    (** Blend ratio between the two colors. *)
    mix:float ->
    rgb
end
