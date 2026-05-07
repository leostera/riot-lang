# Profiling Riot Itself

Use this reference when profiling Riot CLI or build-system commands inside this
repository. For generic project binary profiling, use the `riot-ml` profiling
reference.

## Rules

- Use the workspace-built Riot binary or `riot run riot -- ...` for normal
  validation. Do not install experimental binaries globally while profiling.
- Add `--json` when profiling Riot CLI commands that support it. This removes
  human/TUI rendering from the measured command and gives a sidecar event log.
- Use `--target-dir <fresh-dir>` for cold build profiles so the repository
  `_build` can keep the Riot binary being used for the measurement.
- Let `xctrace` finish finalizing. If a long trace appears stuck, try exporting
  the table of contents and Time Profiler table before interrupting it.

## Cold Build Profile

```sh
stamp=$(date +%Y%m%d-%H%M%S)
riot_bin="$PWD/_build/debug/aarch64-apple-darwin/out/riot-cli/riot"
trace_path="/tmp/riot-build-full-$stamp.trace"
json_log="/tmp/riot-build-full-$stamp.jsonl"
target_dir="/tmp/riot-build-full-target-$stamp"

rm -rf "$trace_path" "$json_log" "$target_dir"

xcrun xctrace record \
  --template 'Time Profiler' \
  --no-prompt \
  --output "$trace_path" \
  --target-stdout "$json_log" \
  --launch -- \
  "$riot_bin" build --all -x all --json --target-dir "$target_dir"
```

## Export

```sh
xcrun xctrace export --input "$trace_path" --toc > "/tmp/xctrace-toc-$stamp.txt"

profile_xml="/tmp/riot-time-profile-$stamp.xml"
xcrun xctrace export \
  --input "$trace_path" \
  --xpath '/trace-toc/run[@number="1"]/data/table[@schema="time-profile"]' \
  --output "$profile_xml"
```

Then summarize with the scripts from `riot-ml`:

```sh
python3 .agents/skills/riot-ml/scripts/xctrace_time_profile.py \
  "$profile_xml" \
  --mode table \
  --sort self \
  --limit 40

jq -r -s -f .agents/skills/riot-ml/scripts/riot_build_gaps.jq "$json_log" \
  | column -t -s $'\t'
```
