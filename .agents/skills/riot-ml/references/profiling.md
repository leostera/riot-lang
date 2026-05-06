# Riot Profiling Workflow

Use this reference when profiling Riot commands or generated Riot binaries.

## Default Rules

- Prefer `--json` for profiled Riot commands. It keeps terminal rendering and
  human-oriented log churn out of the measurement.
- Use `--target-dir` for isolated build profiles instead of mutating the normal
  workspace `_build`.
- Record whether the run is cold, warm, or fully warm:
  - cold: remove the profiling target directory before recording.
  - warm: run once, then record the same command and target directory.
  - fully warm: run repeatedly until no new artifacts are built, then record.
- Capture the exact command, Riot version, OS/architecture, target directory,
  trace path, and the top measured costs.
- Prefer process-local capture. Use all-process capture only when investigating
  scheduler, filesystem, or child-process behavior that does not appear in a
  single-process trace.

## macOS: xctrace

Profile a cold build with Instruments Time Profiler:

```sh
stamp=$(date +%Y%m%d-%H%M%S)
riot_bin=${RIOT_BIN:-$(command -v riot)}
target_dir="/tmp/riot-profile-build-$stamp"
trace_path="/tmp/riot-build-$stamp.trace"

rm -rf "$target_dir" "$trace_path"

xcrun xctrace record \
  --template 'Time Profiler' \
  --no-prompt \
  --output "$trace_path" \
  --target-stdout - \
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

rm -rf "$trace_path"

xcrun xctrace record \
  --template 'Time Profiler' \
  --no-prompt \
  --output "$trace_path" \
  --target-stdout - \
  --launch -- \
  "$riot_bin" plan --all -x all --json
```

Inspect the result:

```sh
open "$trace_path"
xcrun xctrace export --input "$trace_path" --toc | head
```

If a recording is interrupted, check that no profiled command is still running:

```sh
ps aux | rg 'xctrace|riot .*build|riot .*plan'
```

## Linux: perf

Use `perf` for CPU profiles when available:

```sh
stamp=$(date +%Y%m%d-%H%M%S)
riot_bin=${RIOT_BIN:-$(command -v riot)}
target_dir="/tmp/riot-profile-build-$stamp"

rm -rf "$target_dir"

perf record \
  --call-graph dwarf \
  --output "/tmp/riot-build-$stamp.perf.data" \
  -- "$riot_bin" build --all -x all --json --target-dir "$target_dir"

perf report --input "/tmp/riot-build-$stamp.perf.data"
```

For syscall-heavy investigations, use:

```sh
strace -f -c riot build --all -x all --json --target-dir /tmp/riot-profile-build
```

## Windows: WPR/WPA

Use Windows Performance Recorder and Windows Performance Analyzer when
available:

```powershell
$targetDir = "$env:TEMP\riot-profile-build"
Remove-Item -Recurse -Force $targetDir -ErrorAction SilentlyContinue

wpr -start CPU -filemode
riot build --all -x all --json --target-dir $targetDir
wpr -stop "$env:TEMP\riot-build.etl"
```

Open the `.etl` file in Windows Performance Analyzer and report the hot stacks
and wall-clock gaps.

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
- OS, architecture, and toolchain target.
- trace or profile file path.
- top functions, top syscalls, or longest timeline gaps.
- whether terminal output used `--json` or human/TUI mode.
