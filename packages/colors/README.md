# Colors

Advanced color science library for OCaml providing color space conversions and perceptually uniform color blending.

## Features

- **Multiple Color Spaces**: ANSI, RGB, Linear RGB, XYZ, LUV, UV
- **Perceptually Uniform Blending**: Blend colors the way humans perceive them
- **Color Space Conversions**: Seamless conversions between all supported spaces
- **ANSI Support**: 256-color terminal palette with RGB conversion
- **Scientific Foundation**: Based on CIE color space mathematics

## Quick Start

```ocaml
open Std
open Colors

(* Convert ANSI to RGB *)
let red = ANSI.to_rgb (`ansi 9)
(* Returns: `rgb (255, 0, 0) *)

(* Perceptually uniform color blending *)
let blend = RGB.blend 
  (`rgb (0, 0, 255))    (* Blue *)
  (`rgb (255, 255, 0))  (* Yellow *)
  ~mix:0.5
(* Returns the package's LUV-based midpoint as an RGB value *)
```

A runnable example is included:

```sh
riot run -p colors blend_demo
```

## Why Perceptual Color Blending Matters

### The Problem with Naive RGB Blending

```ocaml
(* Naive approach: average RGB values *)
let blue = (0, 0, 255) in
let yellow = (255, 255, 0) in
let naive_blend = ((0+255)/2, (0+255)/2, (255+0)/2)
(* Result: (127, 127, 127) - Gray! This looks wrong! *)
```

### The Solution: LUV Color Space

```ocaml
(* Perceptually uniform blending in LUV space *)
let blend = RGB.blend (`rgb (0, 0, 255)) (`rgb (255, 255, 0)) ~mix:0.5
(* Result: a midpoint computed by converting through LUV space *)
```

The difference: LUV is **perceptually uniform**. Equal numeric distances in LUV correspond to equal perceived color differences by humans.

## Color Space Pipeline

```
ANSI (256 colors)
  ↓ lookup table
RGB (0-255)
  ↓ gamma correction (2.4)
Linear RGB (0.0-1.0)
  ↓ matrix transformation
XYZ (device-independent)
  ↓ with white point reference (D65)
LUV (perceptually uniform)
```

Each step is reversible for round-trip conversions.

## Usage Examples

### Creating Smooth Gradients

```ocaml
let create_gradient start_color end_color steps =
  List.init steps (fun i ->
    let mix = Float.of_int i /. Float.of_int (steps - 1) in
    RGB.blend start_color end_color ~mix
  )

let gradient = create_gradient 
  (`rgb (255, 0, 0))    (* Red *)
  (`rgb (0, 0, 255))    (* Blue *)
  10
```

### Manual Color Space Conversions

```ocaml
(* Full conversion pipeline *)
let rgb = `rgb (200, 150, 100) in
let lrgb = Linear_RGB.linearize rgb in
let xyz = Linear_RGB.to_xyz lrgb in
let luv = XYZ.to_luv xyz in

println (to_string luv)
(* Output: LUV(0.6532,0.1234,0.3456) *)

(* Convert back *)
let xyz' = LUV.to_xyz luv in
let lrgb' = XYZ.to_linear_rgb xyz' in
let rgb' = Linear_RGB.delinearize lrgb' in
(* rgb' ≈ rgb (within floating-point precision) *)
```

### Working with Terminal Colors

```ocaml
(* Convert ANSI colors to RGB for manipulation *)
let ansi_colors = List.init 16 (fun i -> `ansi i) in
let rgb_colors = List.map ANSI.to_rgb ansi_colors in

(* Blend two terminal colors *)
let ansi_red = ANSI.to_rgb (`ansi 9) in
let ansi_blue = ANSI.to_rgb (`ansi 12) in
let blend = RGB.blend ansi_red ansi_blue ~mix:0.5
```

### Custom White Point References

```ocaml
(* Use custom white point for specific lighting *)
let custom_white = `xyz (1.0, 1.0, 1.0) in

let color = `rgb (200, 150, 100) in
let lrgb = Linear_RGB.linearize color in
let xyz = Linear_RGB.to_xyz lrgb in

(* Convert with custom white point *)
let luv = XYZ.to_luv_with_ref xyz ~wref:custom_white in
let xyz' = LUV.to_xyz_with_ref luv ~wref:custom_white in
```

## Color Space Details

### RGB (Standard RGB)
- Integer values: 0-255 per channel
- Gamma-encoded for display
- Common but not perceptually uniform

### Linear RGB
- Float values: 0.0-1.0
- Gamma correction removed
- Required for accurate color math

### XYZ (CIE 1931)
- Device-independent representation
- Bridge between RGB and perceptual spaces
- Based on human cone cell responses

### LUV (CIE LUV)
- Perceptually uniform color space
- Equal numeric distances = equal perceived differences
- Ideal for blending and interpolation
- L* = lightness, u* and v* = chromaticity

### ANSI
- 256-color terminal palette
- Indices 0-15: standard colors
- Indices 16-231: 6×6×6 RGB cube
- Indices 232-255: grayscale

## Mathematical Foundation

This library implements standard CIE color space transformations:

1. **sRGB Gamma Correction**
   - Forward: v ≤ 0.04045 ? v/12.92 : ((v+0.055)/1.055)^2.4
   - Inverse: v ≤ 0.0031308 ? 12.92v : 1.055v^(1/2.4) - 0.055

2. **RGB to XYZ Matrix** (D65 illuminant)
   ```
   [X]   [0.4124 0.3576 0.1805]   [R]
   [Y] = [0.2126 0.7152 0.0722] × [G]
   [Z]   [0.0193 0.1192 0.9505]   [B]
   ```

3. **XYZ to LUV**
   - Uses D65 white point reference by default
   - L* calculation with cube root for perceptual uniformity
   - u*, v* from chromaticity coordinates

## References

- Based on [go-colorful](https://github.com/lucasb-eyer/go-colorful)
- [CIE LUV Color Space](https://en.wikipedia.org/wiki/CIELUV)
- [CIE 1931 XYZ](https://en.wikipedia.org/wiki/CIE_1931_color_space)
- [sRGB Standard](https://en.wikipedia.org/wiki/SRGB)

## When to Use This Library

- **UI Gradients**: Create smooth, perceptually uniform color transitions
- **Color Manipulation**: Blend colors naturally
- **Terminal Applications**: Convert between ANSI and RGB
- **Color Science**: Accurate device-independent color representation
- **Accessibility**: Calculate perceptual color differences

## When NOT to Use This Library

- Simple RGB color storage (use basic tuples)
- HSL/HSV color space (not implemented here)
- Color palette generation (use dedicated tools)
- Performance-critical inner loops (conversions involve floating-point math)
