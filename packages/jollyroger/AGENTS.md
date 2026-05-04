# Jolly Roger

Implement shared terminal design-system primitives here.

## Guidance

- Keep the package focused on reusable layout, color, and status rendering helpers.
- Return strings or structured values; command reporters are responsible for printing.
- Prefer plain-text-safe output first. ANSI styling should be optional and layered on top.
- Use semantic names such as `success`, `warning`, `danger`, `muted`, and `status_label` instead of command-specific names.
- Keep dependencies light; this package is intended for Riot CLI surfaces and should only depend on low-level shared packages such as `std` and `tty`.
