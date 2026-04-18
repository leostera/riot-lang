# colors AGENTS

`colors` owns ANSI palette mapping and conversions between sRGB, linear RGB,
XYZ, UV, and normalized LUV.

## Rules

1. Treat `rgb` as byte-domain sRGB and `lrgb` as unit-domain linear RGB. Normalize before gamma removal; scale, round, and clamp when returning to bytes.
2. Public `luv` uses normalized units: `l` is `0.0..1.0`, and `u`/`v` are scaled to match. Preserve and document that contract.
3. Keep invalid-input behavior explicit and stable: ANSI indices and blend mix are clamped, `ANSI.nearest` clamps RGB channels and breaks ties toward the lowest index, `RGB.of_hex` accepts only 6-digit RGB hex strings, and custom white references must be finite and have positive `Y`.
4. Prefer exhaustive tests over random fuzzing for discrete domains like ANSI indices and 8-bit RGB channels.

## Validate

`timeout 30 riot build colors --json`
`timeout 30 riot test -p colors --json`
`timeout 30 riot run -p colors blend_demo`
