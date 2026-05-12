# Loop 4 Benchmark Notes

## Format

`timestamp_ms,strategy,label,iters,ns_per_op,allocations,field_writes,bytes_writes,root_registrations,root_unregistrations,collections,reclaims`

## Baseline captures

Add one row per case/strategy after running the commands in:
- `zort/README.md` benchmark commands
- `zort/spec/benchmark-depth.md` capture checklist

## Append flow

Use:

`zig build bench -- --filter=<substring> --csv=notes/benchmarks.csv`

## Governance

- Keep development captures at `1000` iterations unless a loop explicitly needs longer runs.
- Compare rows only when `strategy`, `label`, and `iters` match.
- Treat a single surprising row as a hint, not a conclusion; append another row before drawing a directional claim.
