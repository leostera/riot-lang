# Colors

`colors` covers the small set of color operations this repo actually needs:

- ANSI 256-color palette lookup and nearest-color matching
- byte-domain sRGB to linear RGB / XYZ / normalized LUV conversion
- perceptual RGB blending and gradients
- small RGB utilities such as hex codecs, luminance, contrast, and distance

It is a compact package with a narrow scope. The API is organized around a few
modules rather than a large type hierarchy.

## API Map

- `ANSI`: terminal palette lookup and nearest ANSI entry
- `RGB`: high-level helpers for ordinary display colors
- `Linear_RGB`: transfer-curve removal and matrix conversion bridge
- `XYZ`: device-independent color-space conversions
- `LUV`: normalized perceptual color-space operations
- `White_reference`: named white points for XYZ and LUV conversions
- `Colors.to_string`: debug formatting for any public color variant

## Quick Start

```ocaml
open Std
open Colors

let blue = `rgb (0, 0, 255)
let yellow = `rgb (255, 255, 0)

let midpoint = RGB.blend blue yellow ~mix:0.5
let contrast = RGB.contrast_ratio (`rgb (0, 0, 0)) (`rgb (255, 255, 255))

println (to_string ((midpoint:> color)))
println (Float.to_string contrast)
```

Runnable example:

```sh
riot run -p colors blend_demo
```

## Color Model

The package uses these public representations:

- `ansi`: ANSI palette index
- `rgb`: standard byte-domain sRGB, channels in `0..255`
- `lrgb`: unit-domain linear RGB, channels in `0.0..1.0`
- `xyz`: CIE 1931 XYZ
- `luv`: normalized CIE LUV
- `uv`: chromaticity coordinates derived from XYZ

The main conversion pipeline is:

```text
ANSI -> RGB -> Linear_RGB -> XYZ -> LUV
```

Integer RGB roundtrips are approximate because the last step quantizes back to
bytes. ANSI lookup is one-way unless you explicitly choose the nearest palette
entry with `ANSI.nearest`.

## Normalized LUV

This package exposes **normalized** LUV rather than conventional CIELUV units:

- `l` is `0.0..1.0` instead of `0..100`
- `u` and `v` are scaled to match that normalized lightness

That keeps the public representation compact and works well for interpolation
and distance, but it is not a drop-in interchange format for APIs expecting
standard `L*`, `u*`, and `v*` units.

## Common Tasks

### Terminal Colors

```ocaml
let red = ANSI.to_rgb (`ansi 9)
let nearest = ANSI.nearest (`rgb (250, 10, 10))
```

### Hex RGB

```ocaml
let accent = RGB.from_hex "#ff8000"
let css = RGB.to_hex (`rgb (255, 128, 0))
```

`RGB.from_hex` accepts `#RRGGBB` and `RRGGBB`, case-insensitively.

### Explicit Conversions

```ocaml
let rgb = `rgb (200, 150, 100)
let linear = RGB.to_linear_rgb rgb
let xyz = RGB.to_xyz rgb
let luv = RGB.to_luv rgb

let rgb_from_xyz = XYZ.to_rgb xyz
let rgb_from_luv = LUV.to_rgb luv
```

### Perceptual Blending

```ocaml
let start = `rgb (255, 0, 0)
let finish = `rgb (0, 0, 255)

let midpoint = RGB.blend start finish ~mix:0.5
let gradient = RGB.gradient start finish ~steps:10
```

`RGB.blend` converts through normalized LUV so midpoints follow perceived color
change more closely than naive per-channel RGB averaging.

### Metrics

```ocaml
let luminance = RGB.relative_luminance (`rgb (32, 32, 32))
let contrast = RGB.contrast_ratio (`rgb (32, 32, 32)) (`rgb (255, 255, 255))
let distance = RGB.distance_luv (`rgb (255, 0, 0)) (`rgb (0, 0, 255))
```

### White References

```ocaml
let xyz = `xyz (0.4, 0.5, 0.2)
let luv = XYZ.to_luv_with_ref xyz ~wref:White_reference.d50
let xyz' = LUV.to_xyz_with_ref luv ~wref:White_reference.d50
```

Named references:

- `White_reference.d50`
- `White_reference.d55`
- `White_reference.d65`
- `White_reference.d75`
- `White_reference.equal_energy`

Custom white references must be finite, have positive `Y`, and define valid UV
chromaticity.

## Semantics and Edge Cases

- ANSI indices are clamped to `0..255`
- `ANSI.nearest` clamps RGB channels and breaks ties toward the lowest index
- RGB channels are always treated as byte-domain sRGB
- `Linear_RGB.delinearize` clamps to `0.0..1.0`, rounds, then clamps to bytes
- `RGB.blend` and `LUV.blend` clamp `mix` to `0.0..1.0`
- `RGB.blend_unclamped` and `LUV.blend_unclamped` preserve extrapolation
- `RGB.gradient` and `LUV.gradient` are inclusive
- `steps <= 0` returns an empty array
- `steps = 1` returns an array containing the first endpoint

## Design Notes

- Table lookup is kept for `ANSI.to_rgb`; benchmarks show it is faster than
  recomputing the palette mapping.
- Channel-domain work is intentionally explicit: byte RGB, unit linear RGB,
  then XYZ/LUV math.
- The implementation is small enough to audit directly, and the test suite
  leans on exhaustive checks for ANSI indices and 8-bit channel behavior.

## Validation

```sh
timeout 30 riot build colors --json
timeout 30 riot test -p colors --json
timeout 30 riot run -p colors blend_demo
timeout 30 riot bench -p colors --json
```
