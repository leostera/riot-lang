#!/bin/sh

set -eu

SCRIPT_DIR=$(CDPATH= cd -- "$(dirname "$0")" && pwd)
ZORT_DIR=$(CDPATH= cd -- "$SCRIPT_DIR/.." && pwd)
OUT_DIR=${OUT_DIR:-"$ZORT_DIR/zig-out/e2e-ml-zort"}
OCAMLOPT=${OCAMLOPT:-"$HOME/.riot/toolchains/5.5.0-riot.3/aarch64-apple-darwin/bin/ocamlopt.opt"}
CC=${CC:-cc}
BENCH_RUNS=${BENCH_RUNS:-25}
if [ "$#" -gt 0 ]; then
  ZORT_CAML_COMPAT_DYLIB=$1
else
  ZORT_CAML_COMPAT_DYLIB=${ZORT_CAML_COMPAT_DYLIB:-${ZORT_COMPAT_DYLIB:-"$ZORT_DIR/zig-out/lib/libzort-caml-compat.dylib"}}
fi

if [ ! -x "$OCAMLOPT" ]; then
  echo "missing ocamlopt.opt at $OCAMLOPT" >&2
  exit 1
fi

if [ ! -f "$ZORT_CAML_COMPAT_DYLIB" ]; then
  echo "missing zort caml compat dylib at $ZORT_CAML_COMPAT_DYLIB" >&2
  exit 1
fi

mkdir -p "$OUT_DIR"

verify_expected_output() {
  actual_file=$1
  expected_file=$2
  label=$3

  if [ ! -f "$expected_file" ]; then
    echo "missing expected $label file at $expected_file" >&2
    exit 1
  fi

  if cmp -s "$actual_file" "$expected_file"; then
    return
  fi

  echo "mismatched $label for $(basename "$actual_file")" >&2
  diff -u "$expected_file" "$actual_file" >&2 || true
  exit 1
}

measure_bench_ns_per_run() {
  exe=$1
  bench_runs=$2
  bench_time_file=$3

  { time -p sh -c '
      exe=$1
      runs=$2
      i=0
      while [ "$i" -lt "$runs" ]; do
        "$exe" >/dev/null
        i=$((i + 1))
      done
    ' sh "$exe" "$bench_runs" >/dev/null; } 2>"$bench_time_file"

  real_seconds=$(awk '/^real / { print $2 }' "$bench_time_file")
  if [ -z "$real_seconds" ]; then
    echo "failed to capture benchmark timing for $exe" >&2
    exit 1
  fi

  awk -v seconds="$real_seconds" -v runs="$bench_runs" 'BEGIN { printf "%.0f\n", (seconds * 1000000000) / runs }'
}

compile_fixture() {
  case_name=$1
  ml_source=$2
  host_stub=$3

  case_dir="$OUT_DIR/$case_name"
  mkdir -p "$case_dir"

  ml_basename=$(basename "$ml_source")
  ml_copy="$case_dir/$ml_basename"
  ml_object="$case_dir/$case_name.o"
  exe="$case_dir/$case_name.zort"
  raw_output="$case_dir/raw_output.txt"
  stdout_file="$case_dir/stdout.txt"
  trace_file="$case_dir/trace.txt"
  bench_time_file="$case_dir/bench.time"
  bench_file="$case_dir/bench_ns_per_run.txt"
  expected_stdout="$SCRIPT_DIR/ml/$case_name.expected.stdout"
  expected_trace="$SCRIPT_DIR/ml/$case_name.expected.trace.txt"

  cp "$SCRIPT_DIR/ml/$ml_source" "$ml_copy"
  "$OCAMLOPT" -nostdlib -nopervasives -output-obj -o "$ml_object" "$ml_copy"

  $CC \
    "$host_stub" \
    "$ml_object" \
    "$ZORT_CAML_COMPAT_DYLIB" \
    -Wl,-rpath,"$(dirname "$ZORT_CAML_COMPAT_DYLIB")" \
    -lm \
    -o "$exe"
}

compile_success_case() {
  case_name=$1
  ml_source=$2
  host_stub=$3

  compile_fixture "$case_name" "$ml_source" "$host_stub"

  case_dir="$OUT_DIR/$case_name"
  raw_output="$case_dir/raw_output.txt"
  stdout_file="$case_dir/stdout.txt"
  trace_file="$case_dir/trace.txt"
  bench_time_file="$case_dir/bench.time"
  bench_file="$case_dir/bench_ns_per_run.txt"
  expected_stdout="$SCRIPT_DIR/ml/$case_name.expected.stdout"
  expected_trace="$SCRIPT_DIR/ml/$case_name.expected.trace.txt"

  "$case_dir/$case_name.zort" >"$raw_output"

  line_count=$(awk 'END { print NR }' "$raw_output")
  if [ "$line_count" -ne 2 ]; then
    echo "expected exactly 2 output lines from $case_name, got $line_count" >&2
    cat "$raw_output" >&2
    exit 1
  fi

  sed -n '1p' "$raw_output" >"$stdout_file"
  sed -n '2p' "$raw_output" >"$trace_file"

  verify_expected_output "$stdout_file" "$expected_stdout" "stdout"
  verify_expected_output "$trace_file" "$expected_trace" "trace"

  bench_ns_per_run=$(measure_bench_ns_per_run "$exe" "$BENCH_RUNS" "$bench_time_file")
  printf '%s\n' "$bench_ns_per_run" >"$bench_file"

  printf "e2e-ml-zort %s ok %s bench_ns_per_run=%s\n" \
    "$case_name" \
    "$(cat "$stdout_file")" \
    "$bench_ns_per_run"
}

compile_fatal_case() {
  case_name=$1
  ml_source=$2
  host_stub=$3

  compile_fixture "$case_name" "$ml_source" "$host_stub"

  case_dir="$OUT_DIR/$case_name"
  exe="$case_dir/$case_name.zort"
  raw_output="$case_dir/raw_output.txt"
  stdout_file="$case_dir/stdout.txt"
  trace_file="$case_dir/trace.txt"
  stderr_file="$case_dir/stderr.txt"
  exitcode_file="$case_dir/exitcode.txt"
  expected_stdout="$SCRIPT_DIR/ml/$case_name.expected.stdout"
  expected_trace="$SCRIPT_DIR/ml/$case_name.expected.trace.txt"
  expected_stderr="$SCRIPT_DIR/ml/$case_name.expected.stderr"
  expected_exitcode="$SCRIPT_DIR/ml/$case_name.expected.exitcode"

  if (
    sh -c 'exec "$1"' sh "$exe" >"$raw_output" 2>"$stderr_file"
  ) 2>/dev/null; then
    echo "expected fatal failure from $case_name" >&2
    exit 1
  else
    exit_code=$?
  fi

  printf '%s\n' "$exit_code" >"$exitcode_file"

  line_count=$(awk 'END { print NR }' "$raw_output")
  if [ "$line_count" -ne 2 ]; then
    echo "expected exactly 2 output lines from $case_name before fatal, got $line_count" >&2
    cat "$raw_output" >&2
    exit 1
  fi

  sed -n '1p' "$raw_output" >"$stdout_file"
  sed -n '2p' "$raw_output" >"$trace_file"

  verify_expected_output "$stdout_file" "$expected_stdout" "stdout"
  verify_expected_output "$trace_file" "$expected_trace" "trace"
  verify_expected_output "$stderr_file" "$expected_stderr" "stderr"
  verify_expected_output "$exitcode_file" "$expected_exitcode" "exit code"

  printf "e2e-ml-zort %s fatal exit=%s %s\n" \
    "$case_name" \
    "$exit_code" \
    "$(cat "$trace_file")"
}

compile_success_case \
  "min_pure_startup" \
  "min_pure_startup.ml" \
  "$SCRIPT_DIR/ml/min_pure_startup_main.c"

compile_success_case \
  "min_global_pair_root_zort" \
  "min_global_pair_root_zort.ml" \
  "$SCRIPT_DIR/ml/min_global_pair_root_zort_main.c"

compile_success_case \
  "min_pure_startup_reentrant" \
  "min_pure_startup.ml" \
  "$SCRIPT_DIR/ml/min_pure_startup_reentrant_main.c"

compile_fatal_case \
  "min_pure_startup_reentrant_extra_shutdown_fatal" \
  "min_pure_startup.ml" \
  "$SCRIPT_DIR/ml/min_pure_startup_reentrant_extra_shutdown_fatal_main.c"

compile_fatal_case \
  "min_pure_startup_after_shutdown_fatal" \
  "min_pure_startup.ml" \
  "$SCRIPT_DIR/ml/min_pure_startup_after_shutdown_fatal_main.c"

compile_fatal_case \
  "min_pure_shutdown_without_startup_fatal" \
  "min_pure_startup.ml" \
  "$SCRIPT_DIR/ml/min_pure_shutdown_without_startup_fatal_main.c"

compile_success_case \
  "min_external_startup" \
  "min_external_startup.ml" \
  "$SCRIPT_DIR/ml/min_external_startup_main.c"
