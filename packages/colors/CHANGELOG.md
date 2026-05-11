# Changelog

All notable changes to `colors` are documented here.

## 0.0.26 - 2026-04-28

### Changed

- ANSI palette conversion has more stable edge behavior: out-of-range palette indices clamp to the closest valid entry, duplicate palette colors canonicalize predictably, and nearest-color lookup remains stable for off-palette inputs.
- RGB, linear RGB, XYZ, LUV, and UV conversions now have tighter numeric behavior around roundtrips, finite edge cases, and perceptual blending, which makes terminal color and gradient output more predictable.
