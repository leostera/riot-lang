# Riot Profiling Workflow

Use this reference when profiling Riot commands or generated Riot binaries on
macOS.

## Default Rules

- Prefer `--json` for profiled Riot commands. It keeps terminal rendering and
  human-oriented log churn out of the measurement.
- Use `--target-dir` for isolated build profiles instead of mutating the normal
  workspace `_build`.
- Record whether the run is cold, warm, or fully warm:
  - cold: remove the profiling target directory before recording.
  - warm: run once, then record the same command and target directory.
  - fully warm: run repeatedly until no new artifacts are built, then record.
- Capture the exact command, Riot version, macOS/architecture, target directory,
  trace path, and the top measured costs.
- Prefer process-local capture. Use all-process capture only when investigating
  scheduler, filesystem, or child-process behavior that does not appear in a
  single-process trace.

## xctrace Recording

Profile a cold build with Instruments Time Profiler:

```sh
stamp=$(date +%Y%m%d-%H%M%S)
riot_bin=${RIOT_BIN:-$(command -v riot)}
target_dir="/tmp/riot-profile-build-$stamp"
trace_path="/tmp/riot-build-$stamp.trace"
json_log="/tmp/riot-build-$stamp.jsonl"

rm -rf "$target_dir" "$trace_path" "$json_log"

xcrun xctrace record \
  --template 'Time Profiler' \
  --no-prompt \
  --output "$trace_path" \
  --target-stdout "$json_log" \
  --launch -- \
  "$riot_bin" build --all -x all --json --target-dir "$target_dir"
```

Profile a warm build by running the command once before `xctrace` and reusing
the same `target_dir`.

Profile planning separately:

```sh
stamp=$(date +%Y%m%d-%H%M%S)
riot_bin=${RIOT_BIN:-$(command -v riot)}
trace_path="/tmp/riot-plan-$stamp.trace"
json_log="/tmp/riot-plan-$stamp.jsonl"

rm -rf "$trace_path" "$json_log"

xcrun xctrace record \
  --template 'Time Profiler' \
  --no-prompt \
  --output "$trace_path" \
  --target-stdout "$json_log" \
  --launch -- \
  "$riot_bin" plan --all -x all --json
```

Inspect the trace:

```sh
open "$trace_path"
xcrun xctrace export --input "$trace_path" --toc | head
```

## xctrace Call Tables

Export the Time Profiler table to XML:

```sh
profile_xml="/tmp/riot-time-profile-$stamp.xml"

xcrun xctrace export \
  --input "$trace_path" \
  --xpath '/trace-toc/run[@number="1"]/data/table[@schema="time-profile"]' \
  --output "$profile_xml"
```

Print a flat call table sorted by self CPU time:

```sh
python3 .agents/skills/riot-ml/scripts/xctrace_time_profile.py \
  "$profile_xml" \
  --mode table \
  --sort self \
  --limit 40
```

Print an inclusive call tree sorted by total CPU time:

```sh
python3 .agents/skills/riot-ml/scripts/xctrace_time_profile.py \
  "$profile_xml" \
  --mode tree \
  --depth 10 \
  --children 12
```

Add `--show-binary` when system/framework frames need to be separated by
binary.

## Riot JSON Timelines

Use the captured `--json` output to find event gaps:

```sh
jq -r -s -f .agents/skills/riot-ml/scripts/riot_build_gaps.jq "$json_log" \
  | column -t -s $'\t'
```

This is useful when the CPU profile shows little Riot work but the wall clock
still has long silent periods.

If a recording is interrupted, check that no profiled command is still running:

```sh
ps aux | rg 'xctrace|riot .*build|riot .*plan'
```

## What To Capture

- Startup overhead: `riot version --json`
- Planning overhead: `riot plan --all -x all --json`
- Small warm build: `riot build -p kernel --json`
- Whole-workspace warm build: `riot build --all -x all --json`
- Whole-workspace cold build: `riot build --all -x all --json --target-dir <fresh-dir>`

## Reporting

Include:

- command and whether it was cold, warm, or fully warm.
- Riot version and commit when available.
- macOS version and architecture.
- trace, exported XML, and JSON log paths.
- top self-time functions and top inclusive call-tree branches.
- largest Riot JSON event gaps.
- whether terminal output used `--json` or human/TUI mode.

## Script Notes

- `scripts/xctrace_time_profile.py` reads `time-profile` XML exported by
  `xcrun xctrace export`.
- `scripts/riot_build_gaps.jq` reads newline-delimited Riot JSON events with
  `jq -s`.
- Keep raw `.trace` files when possible. XML exports are convenient summaries,
  but Instruments remains the source of truth for deeper inspection.
