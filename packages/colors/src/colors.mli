(**
   Convert colors between ANSI, RGB, XYZ, and LUV, and blend colors in a
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
type ansi = [ | `ansi of int]
(** Standard RGB color with integer channels in the range `0..255`. *)
type rgb = [ | `rgb of int * int * int]
(** Linear RGB color used for numeric color-space calculations. *)
type lrgb = [ | `lrgb of float * float * float]
(** CIE 1931 XYZ color. This representation is device-independent. *)
type xyz = [ | `xyz of float * float * float]
(**
   Normalized CIE LUV color.

   This package stores lightness in the range `0.0..1.0` rather than the
   conventional `0..100`, and scales `u`/`v` accordingly.
*)
type luv = [ | `luv of float * float * float]
(** Chromaticity coordinates derived from XYZ. *)
type uv = [ | `uv of float * float]
(** Any supported color representation. *)
type color = [ | ansi | rgb | lrgb | xyz | luv | uv]

(**
   Format a color for debugging and logs.

   Example:
   ```ocaml
   Colors.to_string (`rgb (255, 128, 0)) = "RGB(255,128,0)"
   ```
*)
val to_string: color -> string

(**
   ANSI palette helpers.

   Use this module when you are starting from terminal palette indices and need
   to move into RGB-based conversions.
*)
module ANSI: sig
  (**
     Convert an ANSI palette entry to RGB.

     Use this when you need a concrete RGB value for a terminal color.

     Indices outside `0..255` are clamped to the nearest valid palette entry.

     Example:
     ```ocaml
     ANSI.to_rgb (`ansi 9) = `rgb (255, 0, 0)
     ```
  *)
  val to_rgb: ansi -> rgb

  (**
     Find the nearest ANSI palette entry for an RGB color.

     RGB channels are clamped to `0..255` before matching. Distances are
     measured in RGB space, and ties resolve to the lowest palette index.

     Example:
     ```ocaml
     ANSI.nearest (`rgb (250, 10, 10)) = `ansi 9
     ```
  *)
  val nearest: rgb -> ansi
end

(** White-point definitions used by XYZ and LUV conversions. *)
module White_reference: sig
  (** Standard D50 white point. Useful for print-oriented workflows. *)
  val d50: xyz

  (** Standard D55 white point. *)
  val d55: xyz

  (**
     Standard D65 white point.

     This is the default daylight white reference used by the package for the
     common XYZ/LUV conversions.
  *)
  val d65: xyz

  (** Standard D75 white point. *)
  val d75: xyz

  (** Equal-energy white point. *)
  val equal_energy: xyz
end

(**
   Helpers for working in linear RGB space.

   Linear RGB is the calculation-friendly form of RGB with the display gamma
   curve removed. Use it as the bridge between display RGB values and CIE
   color spaces such as XYZ.
*)
module Linear_RGB: sig
  (**
     Remove the sRGB gamma curve and convert an RGB color to linear RGB.

     Use this before matrix-based color-space conversions such as RGB to XYZ.

     RGB channels are interpreted in the byte domain `0..255`, normalized to
     `0.0..1.0`, and then converted with the standard sRGB transfer curve.

     Example:
     ```ocaml
     let linear = Linear_RGB.linearize (`rgb (128, 128, 128))
     (* returns something like `lrgb (0.2158, 0.2158, 0.2158) *)
     ```
  *)
  val linearize: rgb -> lrgb

  (**
     Re-apply the sRGB gamma curve and convert linear RGB back to RGB.

     This is the inverse of [linearize]. Linear RGB channels are clamped to
     `0.0..1.0`, then rounded to the nearest byte in `0..255`.
  *)
  val delinearize: lrgb -> rgb

  (**
     Convert linear RGB to XYZ.

     Use this when moving from display-oriented RGB values into a
     device-independent CIE color space.
  *)
  val to_xyz: lrgb -> xyz
end

(**
   Helpers for working in the CIE 1931 XYZ color space.

   XYZ is device-independent and is the main bridge between RGB-style color
   values and perceptually uniform spaces such as LUV.
*)
module XYZ: sig
  (**
     Convert XYZ to linear RGB.

     This is the inverse of [Linear_RGB.to_xyz].
  *)
  val to_linear_rgb: xyz -> lrgb

  (**
     Convert XYZ directly to display RGB.

     This composes [to_linear_rgb] with [Linear_RGB.delinearize].
  *)
  val to_rgb: xyz -> rgb

  (**
     Convert XYZ to chromaticity coordinates.

     Use this when you only care about chromaticity and not luminance. The
     zero XYZ value maps to the sentinel [`uv (0.0, 0.0)].
  *)
  val to_uv: xyz -> uv

  (**
     Convert XYZ to LUV with an explicit white reference.

     Use this when your working white point is not the default daylight
     reference. Invalid white references raise [Invalid_argument].
  *)
  val to_luv_with_ref: xyz -> wref:xyz -> luv

  (** Convert XYZ to LUV using [White_reference.d65]. *)
  val to_luv: xyz -> luv
end

(**
   Helpers for working in the CIE LUV color space.

   LUV is useful when you want distances and interpolation to track perceived
   color change more closely than raw RGB values do.
*)
module LUV: sig
  (** Measure Euclidean distance in normalized LUV space. *)
  val distance: luv -> luv -> float

  (**
     Convert LUV back to XYZ with an explicit white reference.

     The white reference should match the one used in the forward conversion.
     Invalid white references raise [Invalid_argument].
  *)
  val to_xyz_with_ref: luv -> wref:xyz -> xyz

  (** Convert LUV to XYZ using [White_reference.d65]. *)
  val to_xyz: luv -> xyz

  (** Convert LUV directly to display RGB. *)
  val to_rgb: luv -> rgb

  (**
     Blend two LUV colors without clamping [`mix`].

     Use this when you want interpolation outside the `[0.0, 1.0]` segment.
     [`mix = 0.0`] returns the first color and [`mix = 1.0`] returns the
     second color exactly.
  *)
  val blend_unclamped: luv -> luv -> mix:float -> luv

  (**
     Blend two LUV colors.

     Use this when you are already working in LUV and want interpolation that
     changes at an even perceptual rate.

     [`mix`] controls the interpolation:

     - `0.0` returns the first color
     - `0.5` returns the midpoint
     - `1.0` returns the second color

     [`mix`] is clamped to `0.0..1.0`.

     Example:
     ```ocaml
     let start = `luv (0.32, -0.09, -1.13) in
     let finish = `luv (0.97, 0.07, 0.68) in

     let midpoint = LUV.blend start finish ~mix:0.5
     ```
  *)
  val blend: luv -> luv -> mix:float -> luv

  (**
     Build an inclusive LUV gradient.

     - [`steps <= 0`] returns an empty array
     - [`steps = 1`] returns an array containing only the first color
     - otherwise the first and last entries match the endpoints exactly
  *)
  val gradient: luv -> luv -> steps:int -> luv array
end

(**
   High-level RGB helpers.

   Use this module when your application naturally works with ordinary RGB
   colors but you still want conversions and blending that respect perceptual
   color differences.
*)
module RGB: sig
  (** Convert RGB to linear RGB. *)
  val to_linear_rgb: rgb -> lrgb

  (** Convert RGB directly to XYZ. *)
  val to_xyz: rgb -> xyz

  (** Convert RGB directly to normalized LUV. *)
  val to_luv: rgb -> luv

  (** Measure perceptual distance by converting both colors to normalized LUV. *)
  val distance_luv: rgb -> rgb -> float

  (**
     Compute relative luminance from display RGB.

     This uses the standard sRGB transfer curve and luminance coefficients.
  *)
  val relative_luminance: rgb -> float

  (** Compute WCAG contrast ratio between two RGB colors. *)
  val contrast_ratio: rgb -> rgb -> float

  (**
     Parse a 6-digit RGB hex string.

     Accepts either `#RRGGBB` or `RRGGBB`, ignores ASCII case, and returns
     [Error _] for invalid lengths or digits.
  *)
  val from_hex: string -> (rgb, string) Std.result

  (**
     Render RGB as a canonical lowercase hex string.

     Channels are clamped to `0..255` before rendering, and the result uses
     the `#rrggbb` form.
  *)
  val to_hex: rgb -> string

  (**
     Blend two RGB colors through LUV without clamping [`mix`].

     Endpoint colors are clamped to byte-domain RGB first, and exact
     [`mix = 0.0`] and [`mix = 1.0`] return the normalized endpoints.
  *)
  val blend_unclamped: rgb -> rgb -> mix:float -> rgb

  (**
     Blend two RGB colors in perceptually uniform LUV space.

     This is the high-level blend function most callers want. It converts RGB
     into LUV, blends there, and converts the result back to RGB.

     Use this instead of averaging RGB channels directly when you want a
     visually smooth transition. [`mix`] is clamped to `0.0..1.0`.

     Example:
     ```ocaml
     let blue = `rgb (0, 0, 255) in
     let yellow = `rgb (255, 255, 0) in

     let midpoint = RGB.blend blue yellow ~mix:0.5
     ```
  *)
  val blend: rgb -> rgb -> mix:float -> rgb

  (**
     Build an inclusive RGB gradient using perceptual LUV interpolation.

     - [`steps <= 0`] returns an empty array
     - [`steps = 1`] returns an array containing the normalized first color
     - otherwise the first and last entries match the normalized endpoints
       exactly
  *)
  val gradient: rgb -> rgb -> steps:int -> rgb array
end
