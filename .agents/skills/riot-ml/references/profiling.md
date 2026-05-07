# Riot Profiling Workflow

Use this reference when profiling generated binaries or Riot-managed projects on
macOS.

## Default Rules

- Do not assume project binaries support `--json`. That flag is specific to
  Riot CLI commands and only applies when the profiled command is `riot` itself.
- Treat trace collection as expensive work. If a long run has finished but
  `xctrace` is still finalizing, do everything reasonable to let it finish or
  export usable data before interrupting it.
- Use `--target-dir` when profiling `riot build` itself. For project binaries,
  build the binary first, then profile the generated executable directly.
- Record whether the run is cold, warm, or fully warm:
  - cold: remove the profiling target directory before recording.
  - warm: run once, then record the same command and target directory.
  - fully warm: run repeatedly until no new artifacts are built, then record.
- Capture the exact command, Riot version, macOS/architecture, target directory,
  trace path, and the top measured costs.
- Prefer process-local capture. Use all-process capture only when investigating
  scheduler, filesystem, or child-process behavior that does not appear in a
  single-process trace.

## xctrace Recording For Project Binaries

Build the binary first, then profile the executable directly:

```sh
stamp=$(date +%Y%m%d-%H%M%S)
app_bin="./_build/debug/$(uname -m)-apple-darwin/out/<package>/<binary>"
trace_path="/tmp/riot-app-$stamp.trace"
app_stdout="/tmp/riot-app-$stamp.out"

rm -rf "$trace_path" "$app_stdout"

xcrun xctrace record \
  --template 'Time Profiler' \
  --no-prompt \
  --output "$trace_path" \
  --target-stdout "$app_stdout" \
  --launch -- \
  "$app_bin" <app-args>
```

Use the actual generated binary path for the project being profiled. Do not add
`--json` unless that binary explicitly implements such a flag.

## xctrace Recording For Riot CLI Commands

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

Only use `--json` in this recipe because the launched program is the Riot CLI.

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

## Preserve Long Recordings

Long cold builds can spend many minutes collecting data. Do not waste that time
by killing the recorder as soon as the target command exits.

When a recording appears stuck after the Riot process finished:

1. Confirm what is still running:

   ```sh
   ps -axo pid,etime,stat,comm,args | rg 'xctrace|riot .*build|riot .*plan'
   ```

2. Check whether the trace bundle is still being written:

   ```sh
   du -sh "$trace_path"
   find "$trace_path" -type f -maxdepth 4 -print0 \
     | xargs -0 ls -lh \
     | sort -k5 -h \
     | tail
   ```

3. Try a non-destructive export before interrupting anything:

   ```sh
   xcrun xctrace export --input "$trace_path" --toc > "/tmp/xctrace-toc-$stamp.txt"
   ```

4. If the table of contents exports, immediately export the Time Profiler table:

   ```sh
   profile_xml="/tmp/riot-time-profile-$stamp.xml"

   xcrun xctrace export \
     --input "$trace_path" \
     --xpath '/trace-toc/run[@number="1"]/data/table[@schema="time-profile"]' \
     --output "$profile_xml"
   ```

5. If `xctrace` is still active but making progress, keep waiting. Trace
   finalization can take a long time for multi-hundred-second builds and large
   bundles.

6. If the recorder is truly wedged, prefer a graceful interrupt over a hard
   kill:

   ```sh
   kill -INT <xctrace-pid>
   ```

   Re-check `--toc` and export again after the process exits. Use `kill -TERM`
   only after trying `SIGINT`; avoid `kill -9` unless the trace has already been
   exported or there is no other option.

Always keep the captured `json_log` even when the Instruments trace cannot be
exported. Riot JSON events still preserve wall-clock gaps, repeated events, and
target/build timing.

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
