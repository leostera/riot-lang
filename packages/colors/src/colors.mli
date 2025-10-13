(** Color space conversions and perceptually uniform color blending.

    This library provides conversions between multiple color spaces and
    perceptually uniform color blending in the LUV color space. It's based
    on the go-colorful library and implements CIE color space mathematics.

    ## Color Spaces

    This library supports six color representations:

    - **ANSI** - 256-color terminal palette (0-255)
    - **RGB** - Standard RGB with integer values (0-255 per channel)
    - **Linear RGB** - Gamma-corrected RGB with float values for calculations
    - **XYZ** - CIE 1931 XYZ color space (device-independent)
    - **LUV** - CIE LUV perceptually uniform color space
    - **UV** - Chromaticity coordinates from XYZ

    ## Example: Basic Color Conversion

    ```ocaml
    open Std
    open Colors

    let () =
      (* Start with ANSI color *)
      let ansi_red = `ansi 9 in
      
      (* Convert to RGB *)
      let rgb = ANSI.to_rgb ansi_red in
      println (to_string rgb);
      (* Output: RGB(255,0,0) *)
      
      (* Convert through color spaces *)
      let lrgb = Linear_RGB.linearize rgb in
      let xyz = Linear_RGB.to_xyz lrgb in
      let luv = XYZ.to_luv xyz in
      
      println (to_string luv);
      (* Output: LUV(0.5323,1.7512,0.3742) *)
    ```

    ## Example: Perceptually Uniform Color Blending

    The killer feature: blend colors the way humans perceive them,
    not the way computers calculate them.

    ```ocaml
    let () =
      (* Naive RGB blending looks wrong *)
      let blue = `rgb (0, 0, 255) in
      let yellow = `rgb (255, 255, 0) in
      
      (* This gives you gray - not what you expect! *)
      let (`rgb (r, g, b)) = 
        `rgb ((0 + 255) / 2, (0 + 255) / 2, (255 + 0) / 2) in
      println (format "Naive blend: RGB(%d,%d,%d)" r g b);
      (* Output: RGB(127,127,127) - Gray! *)
      
      (* Perceptually uniform blending in LUV space *)
      let blend = RGB.blend blue yellow ~mix:0.5 in
      println (to_string blend);
      (* Output: RGB(0,242,54) - Green! Matches human perception *)
    ```

    ## Example: Smooth Color Gradients

    ```ocaml
    let create_gradient start_color end_color steps =
      List.init steps (fun i ->
        let mix = Float.of_int i /. Float.of_int (steps - 1) in
        RGB.blend start_color end_color ~mix
      )

    let () =
      let gradient = create_gradient 
        (`rgb (255, 0, 0))    (* Red *)
        (`rgb (0, 0, 255))    (* Blue *)
        10 in
      
      List.iter (fun color ->
        println (to_string color)
      ) gradient
    ```

    ## Example: Working with Different White Points

    ```ocaml
    (* Use custom white reference for specific lighting conditions *)
    let custom_white = `xyz (1.0, 1.0, 1.0) in
    
    let color = `rgb (200, 150, 100) in
    let lrgb = Linear_RGB.linearize color in
    let xyz = Linear_RGB.to_xyz lrgb in
    
    (* Convert to LUV with custom white point *)
    let luv = XYZ.to_luv_with_ref xyz ~wref:custom_white in
    
    (* Convert back with same reference *)
    let xyz' = LUV.to_xyz_with_ref luv ~wref:custom_white in
    let lrgb' = XYZ.to_linear_rgb xyz' in
    let color' = Linear_RGB.delinearize lrgb' in
    
    println (to_string color')
    ```

    ## Color Space Conversions

    The conversion pipeline:

    ```
    ANSI (256 colors)
      ↓ (lookup table)
    RGB (0-255)
      ↓ (gamma correction: 2.4)
    Linear RGB (0.0-1.0)
      ↓ (matrix transformation)
    XYZ (device-independent)
      ↓ (with white point reference)
    LUV (perceptually uniform)
    ```

    Each step is reversible, allowing round-trip conversions.

    ## Understanding Perceptual Uniformity

    In RGB space, equal numeric distances don't correspond to equal
    perceived color differences. For example:
    - RGB(0,0,0) to RGB(50,50,50) looks like a bigger change than
    - RGB(200,200,200) to RGB(250,250,250)

    LUV color space is **perceptually uniform** - equal numeric distances
    correspond to equal perceived color differences. This makes it ideal
    for color blending, interpolation, and calculating color similarity.

    ## References

    This implementation is based on:
    - [go-colorful library](https://github.com/lucasb-eyer/go-colorful)
    - [CIE LUV color space](https://en.wikipedia.org/wiki/CIELUV)
    - [CIE 1931 XYZ](https://en.wikipedia.org/wiki/CIE_1931_color_space)
*)

type ansi = [ `ansi of int ]
(** ANSI 256-color palette entry (0-255) *)

type rgb = [ `rgb of int * int * int ]
(** RGB color with integer values (0-255 per channel) *)

type lrgb = [ `lrgb of float * float * float ]
(** Linear RGB (gamma-corrected) with float values for calculations *)

type xyz = [ `xyz of float * float * float ]
(** CIE 1931 XYZ color space (device-independent representation) *)

type luv = [ `luv of float * float * float ]
(** CIE LUV perceptually uniform color space *)

type uv = [ `uv of float * float ]
(** Chromaticity coordinates derived from XYZ *)

type color = [ ansi | rgb | xyz | luv | uv ]
(** Union of all color types *)

val to_string : color -> string
(** Convert any color type to a string representation.
    
    Examples:
    - `ansi 9` → "ANSI(9)"
    - `rgb (255, 128, 0)` → "RGB(255,128,0)"
    - `luv (0.5323, 1.7512, 0.3742)` → "LUV(0.5323,1.7512,0.3742)"
*)

module ANSI : sig
  val to_rgb : ansi -> rgb
  (** Convert an ANSI 256-color palette entry to RGB.
      
      Uses a lookup table mapping ANSI color indices to their RGB equivalents.
      Values outside 0-255 are clamped to valid range.
      
      Example:
      ```ocaml
      let red = ANSI.to_rgb (`ansi 9) in
      (* Returns: `rgb (255, 0, 0) *)
      ```
  *)
end

module White_reference : sig
  val d65 : xyz
  (** Standard D65 white point (daylight illuminant at 6504K).
      
      This is the default white reference used for most conversions.
      Corresponds to average daylight with correlated color temperature of 6504K.
      
      Value: (0.95047, 1.00000, 1.08883)
      
      Reference: [Standard illuminants](https://en.wikipedia.org/wiki/Standard_illuminant)
  *)
end

module Linear_RGB : sig
  val linearize : rgb -> lrgb
  (** Convert standard RGB to linear RGB by removing gamma correction.
      
      Applies the inverse of sRGB gamma curve:
      - For values ≤ 0.04045: linear = value / 12.92
      - For values > 0.04045: linear = ((value + 0.055) / 1.055)^2.4
      
      Example:
      ```ocaml
      let linear = Linear_RGB.linearize (`rgb (128, 128, 128)) in
      (* Returns: `lrgb (0.2158, 0.2158, 0.2158) *)
      ```
  *)

  val delinearize : lrgb -> rgb
  (** Convert linear RGB back to standard RGB with gamma correction.
      
      Applies sRGB gamma curve:
      - For values ≤ 0.0031308: srgb = value * 12.92
      - For values > 0.0031308: srgb = 1.055 * value^(1/2.4) - 0.055
      
      This is the inverse of `linearize`.
  *)

  val to_xyz : lrgb -> xyz
  (** Convert linear RGB to CIE XYZ color space.
      
      Uses the sRGB to XYZ transformation matrix (D65 illuminant).
      XYZ is device-independent and serves as the bridge between
      RGB and perceptually uniform color spaces.
  *)
end

module XYZ : sig
  val to_linear_rgb : xyz -> lrgb
  (** Convert CIE XYZ to linear RGB.
      
      Uses the XYZ to sRGB transformation matrix (D65 illuminant).
      This is the inverse of `Linear_RGB.to_xyz`.
      
      Note: Some XYZ colors may be outside the RGB gamut, resulting
      in clamped or out-of-range RGB values.
  *)

  val to_uv : xyz -> uv
  (** Convert XYZ to chromaticity coordinates.
      
      Extracts the chromaticity (color information without luminance):
      - u = 4X / (X + 15Y + 3Z)
      - v = 9Y / (X + 15Y + 3Z)
      
      Returns (0.0, 0.0) if denominator is zero.
  *)

  val to_luv_with_ref : xyz -> wref:xyz -> luv
  (** Convert XYZ to LUV using a custom white reference.
      
      The white reference defines what "white" means in the viewing conditions.
      Different illuminants (D65, D50, etc.) produce different LUV values.
      
      Use this when you need to match specific lighting conditions.
  *)

  val to_luv : xyz -> luv
  (** Convert XYZ to LUV using the D65 white reference.
      
      This is the most common conversion for typical display conditions.
      Equivalent to `to_luv_with_ref xyz ~wref:White_reference.d65`.
  *)
end

module LUV : sig
  val to_xyz_with_ref : luv -> wref:xyz -> xyz
  (** Convert LUV back to XYZ using a custom white reference.
      
      The white reference must match the one used in the forward conversion.
      This is the inverse of `XYZ.to_luv_with_ref`.
  *)

  val to_xyz : luv -> xyz
  (** Convert LUV to XYZ using the D65 white reference.
      
      This is the inverse of `XYZ.to_luv`.
  *)

  val blend : luv -> luv -> mix:float -> luv
  (** Blend two colors in LUV space.
      
      `mix` controls the blend ratio:
      - 0.0 returns `color1`
      - 0.5 returns the midpoint
      - 1.0 returns `color2`
      
      Values outside [0.0, 1.0] are clamped.
      
      Blending in LUV space produces perceptually uniform results,
      meaning the perceived color difference changes linearly with `mix`.
      
      Example:
      ```ocaml
      let blue = `luv (0.32, -0.09, -1.13) in
      let yellow = `luv (0.97, 0.07, 0.68) in
      let green = LUV.blend blue yellow ~mix:0.5 in
      (* Produces perceptually accurate blue-green *)
      ```
  *)
end

module RGB : sig
  val blend : rgb -> rgb -> mix:float -> rgb
  (** Blend two RGB colors in perceptually uniform LUV space.
      
      This is the high-level blending function you should use for RGB colors.
      It automatically:
      1. Converts RGB → Linear RGB → XYZ → LUV
      2. Blends in LUV space
      3. Converts back LUV → XYZ → Linear RGB → RGB
      
      `mix` controls the blend ratio:
      - 0.0 returns `color1`
      - 0.5 returns the perceptual midpoint
      - 1.0 returns `color2`
      
      Example:
      ```ocaml
      (* Naive RGB blend: (0+255)/2, (0+255)/2, (255+0)/2 = gray *)
      (* Perceptual LUV blend: produces green as humans perceive it *)
      let blend = RGB.blend 
        (`rgb (0, 0, 255))    (* Blue *)
        (`rgb (255, 255, 0))  (* Yellow *)
        ~mix:0.5 in
      (* Returns: `rgb (0, 242, 54) - Green! *)
      ```
  *)
end
