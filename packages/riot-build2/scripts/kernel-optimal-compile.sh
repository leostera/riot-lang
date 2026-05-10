#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)"
ROOT="$(cd "$SCRIPT_DIR/../../.." && pwd -P)"

KERNEL_SRC="$ROOT/packages/kernel/src"
OCAMLOPT="${OCAMLOPT:-ocamlopt.opt}"
OCAMLOBJINFO="${OCAMLOBJINFO:-ocamlobjinfo}"
MAKE_BIN="${MAKE_BIN:-make}"
MODE="${MODE:-source}"
JOBS="${JOBS:-}"
OUT_DIR="${OUT_DIR:-$ROOT/_bench/kernel-optimal-$(date +%s)-$$}"
STAGED_KERNEL_DIR="${STAGED_KERNEL_DIR:-}"

if [ "$MODE" = "all" ]; then
  BASE_OUT_DIR="$OUT_DIR"
  status=0
  for mode in source module folder single; do
    printf '\n=== MODE=%s ===\n' "$mode"
    if ! MODE="$mode" OUT_DIR="$BASE_OUT_DIR/$mode" "$0"; then
      status=1
    fi
  done
  exit "$status"
fi

case "$MODE" in
  source | module | folder | single)
    ;;
  *)
    printf 'error: MODE must be source, module, folder, single, or all; got %s\n' "$MODE" >&2
    exit 2
    ;;
esac

count_files() {
  find "$1" -type f -name "$2" | wc -l | awk '{ print $1 }'
}

find_riot_tool() {
  local tool="$1"
  local preferred_triple="${2:-}"
  local candidate
  local found=""

  shopt -s nullglob
  if [ -n "$preferred_triple" ]; then
    for candidate in "$HOME"/.riot/toolchains/*/"$preferred_triple"/bin/"$tool"; do
      if [ -x "$candidate" ] && "$candidate" -version >/dev/null 2>&1; then
        found="$candidate"
      fi
    done
  fi

  if [ -n "$found" ]; then
    shopt -u nullglob
    printf '%s\n' "$found"
    return
  fi

  for candidate in "$HOME"/.riot/toolchains/*/*/bin/"$tool"; do
    if [ -x "$candidate" ] && "$candidate" -version >/dev/null 2>&1; then
      found="$candidate"
    fi
  done
  shopt -u nullglob

  printf '%s\n' "$found"
}

resolve_tool() {
  local tool="$1"
  local preferred_triple="${2:-}"
  local resolved

  if resolved="$(command -v "$tool" 2>/dev/null)"; then
    printf '%s\n' "$resolved"
    return
  fi

  find_riot_tool "$tool" "$preferred_triple"
}

find_staged_kernel_dir() {
  local candidate

  shopt -s nullglob
  for candidate in \
    "$ROOT"/_build/riot-build2-tests/kernel-*/debug/*/sandbox/kernel/* \
    "$ROOT"/_build/riot-build2-bench/kernel-*/debug/*/sandbox/kernel/* \
    "$ROOT"/_bench/kernel-*/debug/*/sandbox/kernel/*; do
    if [ -f "$candidate/Kernel.ml" ] && [ -f "$candidate/Kernel__Aliases.ml" ]; then
      printf '%s\n' "$candidate"
    fi
  done | tail -n 1
  shopt -u nullglob
}

available_parallelism() {
  if command -v sysctl >/dev/null 2>&1; then
    sysctl -n hw.ncpu 2>/dev/null && return
  fi

  if command -v nproc >/dev/null 2>&1; then
    nproc && return
  fi

  printf '4\n'
}

if [ -z "$JOBS" ]; then
  JOBS="$(available_parallelism)"
fi

requested_ocamlopt="$OCAMLOPT"
requested_ocamlobjinfo="$OCAMLOBJINFO"

if [ -z "$STAGED_KERNEL_DIR" ]; then
  STAGED_KERNEL_DIR="$(find_staged_kernel_dir)"
fi

if [ -z "$STAGED_KERNEL_DIR" ] || [ ! -d "$STAGED_KERNEL_DIR" ]; then
  cat >&2 <<EOF
error: no staged kernel sandbox was found.

Run a build2 kernel build once, or provide the exact staged source directory:

  STAGED_KERNEL_DIR=/path/to/sandbox/kernel/<hash> $0

The timed section of this script starts after staging has been copied and after
the dependency graph has been read from existing compiler metadata.
EOF
  exit 2
fi

STAGED_TARGET=""
case "$STAGED_KERNEL_DIR" in
  */debug/*/sandbox/kernel/*)
    STAGED_TARGET="${STAGED_KERNEL_DIR#*/debug/}"
    STAGED_TARGET="${STAGED_TARGET%%/sandbox/kernel/*}"
    ;;
esac

OCAMLOPT="$(resolve_tool "$OCAMLOPT" "$STAGED_TARGET")"
OCAMLOBJINFO="$(resolve_tool "$OCAMLOBJINFO" "$STAGED_TARGET")"

[ -n "$OCAMLOPT" ] || {
  printf 'error: %s was not found in PATH or ~/.riot/toolchains\n' "$requested_ocamlopt" >&2
  exit 2
}

[ -n "$OCAMLOBJINFO" ] || {
  printf 'error: %s was not found in PATH or ~/.riot/toolchains\n' "$requested_ocamlobjinfo" >&2
  exit 2
}

command -v python3 >/dev/null 2>&1 || {
  printf 'error: python3 was not found in PATH\n' >&2
  exit 2
}

command -v "$MAKE_BIN" >/dev/null 2>&1 || {
  printf 'error: %s was not found in PATH\n' "$MAKE_BIN" >&2
  exit 2
}

rm -rf "$OUT_DIR"
mkdir -p "$OUT_DIR/work"

WORK="$OUT_DIR/work"
cp "$STAGED_KERNEL_DIR"/*.ml "$WORK"/
cp "$STAGED_KERNEL_DIR"/*.mli "$WORK"/

cat > "$WORK/Makefile" <<'EOF'
OCAMLOPT ?= ocamlopt.opt

COMPILEFLAGS = -bin-annot -w -49 -g -inline 0 -warn-error +1+8+26+33+11 -nopervasives -nostdlib -I .
ALIAS_COMPILEFLAGS = $(COMPILEFLAGS) -no-alias-deps
ARCHIVEFLAGS = -bin-annot -w -49 -I .

.PHONY: all clean

all: Kernel.cmxa

-include build_plan.mk

clean:
	@rm -f *.a *.annot *.cmi *.cmo *.cmx *.cmt *.cmti *.cmxa *.o
	@rm -f *.stamp build_plan.mk graph_stats.env
EOF

kernel_ml="$(count_files "$KERNEL_SRC" '*.ml')"
kernel_mli="$(count_files "$KERNEL_SRC" '*.mli')"
kernel_dirs="$(find "$KERNEL_SRC" -type f \( -name '*.ml' -o -name '*.mli' \) -exec dirname {} \; | sort -u | wc -l | awk '{ print $1 }')"
staged_ml="$(count_files "$WORK" '*.ml')"
staged_mli="$(count_files "$WORK" '*.mli')"
alias_ml="$(count_files "$WORK" '*__Aliases.ml')"
staged_files=$((staged_ml + staged_mli))

printf 'kernel compiler-only benchmark\n'
printf '  mode:               %s\n' "$MODE"
printf '  staged source:      %s\n' "$STAGED_KERNEL_DIR"
printf '  staged target:      %s\n' "${STAGED_TARGET:-unknown}"
printf '  output dir:         %s\n' "$OUT_DIR"
printf '  ocamlopt:           %s\n' "$OCAMLOPT"
printf '  ocamlobjinfo:       %s\n' "$OCAMLOBJINFO"
printf '  jobs:               %s\n' "$JOBS"
printf '  raw kernel dirs:    %s\n' "$kernel_dirs"
printf '  raw kernel files:   %s ml + %s mli = %s\n' "$kernel_ml" "$kernel_mli" "$((kernel_ml + kernel_mli))"
printf '  staged files:       %s ml + %s mli = %s\n' "$staged_ml" "$staged_mli" "$staged_files"
printf '  staged aliases:     %s ml\n' "$alias_ml"
printf '  dependency graph:   existing cmi/cmx metadata (not timed)\n'

python3 - "$STAGED_KERNEL_DIR" "$WORK" "$OCAMLOBJINFO" "$MODE" <<'PY'
import os
import re
import subprocess
import sys

artifact_dir, work_dir, objinfo, mode = sys.argv[1:]

ml_units = {name[:-3] for name in os.listdir(work_dir) if name.endswith(".ml")}
mli_units = {name[:-4] for name in os.listdir(work_dir) if name.endswith(".mli")}
local_units = ml_units | mli_units
alias_units = {unit for unit in ml_units if unit.endswith("__Aliases")}
concrete_ml_units = ml_units - alias_units
concrete_units = (ml_units | mli_units) - alias_units


def parse_imports(path):
    if not os.path.exists(path):
        raise SystemExit(f"missing compiled artifact for dependency extraction: {path}")

    output = subprocess.check_output([objinfo, path], text=True, stderr=subprocess.DEVNULL)
    section = None
    interfaces = []
    implementations = []

    for raw_line in output.splitlines():
        line = raw_line.rstrip()
        if line == "Interfaces imported:":
            section = "interfaces"
            continue
        if line == "Implementations imported:":
            section = "implementations"
            continue
        if not line.startswith("\t"):
            section = None
            continue
        if section is None:
            continue

        parts = line.split()
        if not parts:
            continue

        unit = parts[-1]
        if unit not in local_units:
            continue
        if section == "interfaces":
            interfaces.append(unit)
        else:
            implementations.append(unit)

    return interfaces, implementations


cmi_interfaces = {}
cmx_interfaces = {}
cmx_implementations = {}

for unit in sorted(mli_units):
    interfaces, _ = parse_imports(os.path.join(artifact_dir, unit + ".cmi"))
    cmi_interfaces[unit] = interfaces

for unit in sorted(ml_units):
    interfaces, implementations = parse_imports(os.path.join(artifact_dir, unit + ".cmx"))
    cmx_interfaces[unit] = interfaces
    cmx_implementations[unit] = implementations


def unique(items):
    seen = set()
    result = []
    for item in items:
        if item in seen:
            continue
        seen.add(item)
        result.append(item)
    return result


def topological_order(nodes, deps_by_node, label):
    visiting = set()
    visited = set()
    ordered = []

    def visit(node):
        if node in visited:
            return
        if node in visiting:
            raise SystemExit(f"cycle in {label} dependency graph at {node}")

        visiting.add(node)
        for dep in deps_by_node.get(node, []):
            if dep in nodes:
                visit(dep)
        visiting.remove(node)
        visited.add(node)
        ordered.append(node)

    for node in sorted(nodes):
        visit(node)

    return ordered


def make_rule(lines, target, deps, commands):
    deps = unique(deps)
    if deps:
        lines.append(f"{target}: {' '.join(deps)}")
    else:
        lines.append(f"{target}:")

    for command in commands:
        lines.append(f"\t@{command}")

    lines.append("")


def unit_sources(unit):
    sources = []
    if unit in mli_units:
        sources.append(unit + ".mli")
    if unit in ml_units:
        sources.append(unit + ".ml")
    return sources


def source_cmi_process_target(unit):
    if unit in mli_units:
        return unit + ".cmi"
    if unit in ml_units:
        return unit + ".cmx"
    return None


def module_process_target(unit):
    if unit in ml_units:
        return unit + ".cmx"
    if unit in mli_units:
        return unit + ".cmi"
    return None


def local_imports_for_unit(unit):
    imports = []
    imports.extend(cmi_interfaces.get(unit, []))
    imports.extend(cmx_interfaces.get(unit, []))
    imports.extend(cmx_implementations.get(unit, []))
    return unique(
        imported
        for imported in imports
        if imported != unit and imported in local_units
    )


def archive_order():
    deps = {unit: [] for unit in alias_units}
    for unit in concrete_ml_units:
        deps[unit] = unique(
            imported
            for imported in (cmx_interfaces.get(unit, []) + cmx_implementations.get(unit, []))
            if imported != unit and imported in ml_units
        )
    return topological_order(ml_units, deps, "cmx implementation")


def archive_objects():
    return [unit + ".cmx" for unit in archive_order()]


def write_stats(process_deps, compile_processes):
    process_nodes = set(process_deps)
    normalized_process_deps = {}

    for target, deps in process_deps.items():
        normalized_process_deps[target] = unique(
            dep
            for dep in deps
            if dep in process_nodes and dep != target
        )

    level_memo = {}
    visiting = set()
    stack = []

    def level(target):
        if target in level_memo:
            return level_memo[target]
        if target in visiting:
            cycle_start = stack.index(target) if target in stack else 0
            cycle = stack[cycle_start:] + [target]
            raise SystemExit("cycle in process dependency graph: " + " -> ".join(cycle))

        visiting.add(target)
        stack.append(target)
        deps = normalized_process_deps.get(target, [])
        if not deps:
            result = 0
        else:
            result = max(level(dep) for dep in deps) + 1
        stack.pop()
        visiting.remove(target)
        level_memo[target] = result
        return result

    level_counts = {}
    for target in sorted(process_nodes):
        target_level = level(target)
        level_counts[target_level] = level_counts.get(target_level, 0) + 1

    levels = max(level_counts) + 1 if level_counts else 0
    widest = max(level_counts.values()) if level_counts else 0
    wave_sizes = ",".join(str(level_counts.get(idx, 0)) for idx in range(levels))

    with open(os.path.join(work_dir, "graph_stats.env"), "w", encoding="utf-8") as file:
        file.write(f"compile_processes={compile_processes}\n")
        file.write("archive_processes=1\n")
        file.write(f"total_processes={compile_processes + 1}\n")
        file.write(f"levels={levels}\n")
        file.write(f"widest={widest}\n")
        file.write(f"wave_sizes={wave_sizes}\n")


def write_build_plan(lines, process_deps, compile_processes):
    with open(os.path.join(work_dir, "build_plan.mk"), "w", encoding="utf-8") as file:
        file.write("\n".join(lines))
        file.write("\n")
    write_stats(process_deps, compile_processes)


def source_mode():
    lines = [f"ARCHIVE_OBJECTS := {' '.join(archive_objects())}", ""]
    process_deps = {}

    for unit in sorted(alias_units):
        target = unit + ".cmx"
        process_deps[target] = []
        make_rule(lines, target, [unit + ".ml"], ["$(OCAMLOPT) $(ALIAS_COMPILEFLAGS) -c $<"])
        make_rule(lines, unit + ".cmi", [target], [":"])

    for unit in sorted(mli_units):
        target = unit + ".cmi"
        imported_units = [
            imported
            for imported in cmi_interfaces.get(unit, [])
            if imported != unit and imported in local_units
        ]
        make_deps = [imported + ".cmi" for imported in imported_units]
        process_deps[target] = unique(
            dep
            for dep in (source_cmi_process_target(imported) for imported in imported_units)
            if dep is not None
        )
        make_rule(lines, target, [unit + ".mli"] + make_deps, ["$(OCAMLOPT) $(COMPILEFLAGS) -c $<"])

    for unit in sorted(concrete_ml_units):
        target = unit + ".cmx"
        make_deps = []
        process_target_deps = []

        if unit in mli_units:
            make_deps.append(unit + ".cmi")
            process_target_deps.append(unit + ".cmi")

        iface_units = [
            imported
            for imported in cmx_interfaces.get(unit, [])
            if imported != unit and imported in local_units
        ]
        impl_units = [
            imported
            for imported in cmx_implementations.get(unit, [])
            if imported != unit and imported in ml_units
        ]

        make_deps.extend(imported + ".cmi" for imported in iface_units)
        make_deps.extend(imported + ".cmx" for imported in impl_units)
        process_target_deps.extend(
            dep
            for dep in (source_cmi_process_target(imported) for imported in iface_units)
            if dep is not None
        )
        process_target_deps.extend(imported + ".cmx" for imported in impl_units)
        process_deps[target] = unique(process_target_deps)

        make_rule(lines, target, [unit + ".ml"] + make_deps, ["$(OCAMLOPT) $(COMPILEFLAGS) -c $<"])
        if unit not in mli_units:
            make_rule(lines, unit + ".cmi", [target], [":"])

    make_rule(lines, "Kernel.cmxa", archive_objects(), ["$(OCAMLOPT) -a $(ARCHIVEFLAGS) -o $@ $(ARCHIVE_OBJECTS)"])
    write_build_plan(lines, process_deps, len(process_deps))


def module_mode():
    lines = [f"ARCHIVE_OBJECTS := {' '.join(archive_objects())}", ""]
    process_deps = {}

    for unit in sorted(alias_units):
        target = unit + ".cmx"
        process_deps[target] = []
        make_rule(lines, target, [unit + ".ml"], ["$(OCAMLOPT) $(ALIAS_COMPILEFLAGS) -c $<"])
        make_rule(lines, unit + ".cmi", [target], [":"])

    for unit in sorted(concrete_units):
        target = module_process_target(unit)
        if target is None:
            continue

        imports = local_imports_for_unit(unit)
        deps = unique(
            dep
            for dep in (module_process_target(imported) for imported in imports)
            if dep is not None and dep != target
        )
        process_deps[target] = deps
        sources = unit_sources(unit)

        if unit in ml_units:
            make_rule(lines, target, sources + deps, [f"$(OCAMLOPT) $(COMPILEFLAGS) -c {' '.join(sources)}"])
            make_rule(lines, unit + ".cmi", [target], [":"])
        else:
            make_rule(lines, target, sources + deps, ["$(OCAMLOPT) $(COMPILEFLAGS) -c $<"])

    make_rule(lines, "Kernel.cmxa", archive_objects(), ["$(OCAMLOPT) -a $(ARCHIVEFLAGS) -o $@ $(ARCHIVE_OBJECTS)"])
    write_build_plan(lines, process_deps, len(process_deps))


def single_mode():
    process_deps = {}
    for unit in concrete_units | alias_units:
        target = module_process_target(unit)
        if target is None:
            continue
        imports = [] if unit in alias_units else local_imports_for_unit(unit)
        process_deps[target] = unique(
            dep
            for dep in (module_process_target(imported) for imported in imports)
            if dep is not None and dep != target
        )

    ordered_targets = topological_order(set(process_deps), process_deps, "single command")
    ordered_units = [target.rsplit(".", 1)[0] for target in ordered_targets]
    ordered_sources = []
    for unit in ordered_units:
        ordered_sources.extend(unit_sources(unit))

    lines = [
        f"ARCHIVE_OBJECTS := {' '.join(archive_objects())}",
        f"SINGLE_SOURCES := {' '.join(ordered_sources)}",
        "",
    ]
    make_rule(lines, "single.stamp", ordered_sources, [
        "$(OCAMLOPT) $(ALIAS_COMPILEFLAGS) -c $(SINGLE_SOURCES)",
        "touch $@",
    ])
    make_rule(lines, "Kernel.cmxa", ["single.stamp"], ["$(OCAMLOPT) -a $(ARCHIVEFLAGS) -o $@ $(ARCHIVE_OBJECTS)"])
    write_build_plan(lines, {"single.stamp": []}, 1)


def source_origin(source_name):
    path = os.path.join(work_dir, source_name)
    marker = re.compile(r'^# 1 "([^"]+)"')
    with open(path, "r", encoding="utf-8", errors="replace") as file:
        for _ in range(8):
            line = file.readline()
            if not line:
                break
            match = marker.match(line)
            if match:
                source_path = match.group(1)
                needle = "/packages/kernel/src/"
                if needle in source_path:
                    rel = source_path.split(needle, 1)[1]
                    dirname = os.path.dirname(rel)
                    return dirname if dirname else "."
                dirname = os.path.dirname(source_path)
                return dirname if dirname else "."
    return "."


def folder_mode():
    def safe_group_name(group):
        if group == ".":
            return "root"
        return re.sub(r"[^A-Za-z0-9_]+", "_", group).strip("_")

    unit_group = {}
    groups = {}
    root_units = []

    for unit in concrete_units:
        sources = unit_sources(unit)
        group = source_origin(sources[0]) if sources else "."
        unit_group[unit] = group
        if group != ".":
            groups.setdefault(group, []).append(unit)

    group_names = sorted(groups)
    parent_groups = set()
    for group in group_names:
        current = os.path.dirname(group)
        while current not in ("", "."):
            parent_groups.add(current)
            current = os.path.dirname(current)

    batched_groups = sorted(group for group in group_names if group not in parent_groups)
    group_target = {group: f"folder_{safe_group_name(group)}.stamp" for group in batched_groups}
    individual_units = []
    for unit in concrete_units:
        group = unit_group[unit]
        if group == "." or group not in group_target:
            individual_units.append(unit)

    individual_target = {
        unit: module_process_target(unit)
        for unit in individual_units
        if module_process_target(unit) is not None
    }
    lines = [f"ARCHIVE_OBJECTS := {' '.join(archive_objects())}", ""]
    process_deps = {}

    alias_sources = [unit + ".ml" for unit in sorted(alias_units)]
    make_rule(lines, "aliases.stamp", alias_sources, [
        "$(OCAMLOPT) $(ALIAS_COMPILEFLAGS) -c " + " ".join(alias_sources),
        "touch $@",
    ])
    process_deps["aliases.stamp"] = []

    def target_for_import(imported):
        if imported in alias_units:
            return "aliases.stamp"
        if imported in individual_target:
            return individual_target[imported]
        group = unit_group.get(imported)
        if group is None or group not in group_target:
            return None
        return group_target[group]

    for unit in sorted(individual_target):
        target = individual_target[unit]
        sources = unit_sources(unit)
        deps = ["aliases.stamp"]
        deps.extend(
            dep
            for dep in (target_for_import(imported) for imported in local_imports_for_unit(unit))
            if dep is not None and dep != target
        )
        deps = unique(deps)
        process_deps[target] = deps

        if unit in ml_units:
            make_rule(lines, target, sources + deps, [f"$(OCAMLOPT) $(COMPILEFLAGS) -c {' '.join(sources)}"])
            make_rule(lines, unit + ".cmi", [target], [":"])
        else:
            make_rule(lines, target, sources + deps, ["$(OCAMLOPT) $(COMPILEFLAGS) -c $<"])

    for idx, group in enumerate(batched_groups):
        target = group_target[group]
        units = set(groups[group])
        unit_deps = {}
        for unit in units:
            unit_deps[unit] = [
                imported
                for imported in local_imports_for_unit(unit)
                if imported in units
            ]

        ordered_units = topological_order(units, unit_deps, f"folder {group}")
        sources = []
        for unit in ordered_units:
            sources.extend(unit_sources(unit))

        deps = ["aliases.stamp"]
        for unit in ordered_units:
            for imported in local_imports_for_unit(unit):
                dep = target_for_import(imported)
                if dep is not None and dep != target:
                    deps.append(dep)

        deps = unique(deps)
        process_deps[target] = deps
        variable = f"FOLDER_{idx}_SOURCES"
        lines.append(f"{variable} := {' '.join(sources)}")
        make_rule(lines, target, sources + deps, [
            f"$(OCAMLOPT) $(COMPILEFLAGS) -c $({variable})",
            "touch $@",
        ])

    make_rule(lines, "Kernel.cmxa", ["aliases.stamp"] + list(individual_target.values()) + [group_target[group] for group in batched_groups], [
        "$(OCAMLOPT) -a $(ARCHIVEFLAGS) -o $@ $(ARCHIVE_OBJECTS)",
    ])
    write_build_plan(lines, process_deps, len(process_deps))


if mode == "source":
    source_mode()
elif mode == "module":
    module_mode()
elif mode == "single":
    single_mode()
elif mode == "folder":
    folder_mode()
else:
    raise SystemExit(f"unknown mode: {mode}")
PY

graph_compile_processes="$(awk -F= '$1 == "compile_processes" { print $2 }' "$WORK/graph_stats.env")"
graph_total_processes="$(awk -F= '$1 == "total_processes" { print $2 }' "$WORK/graph_stats.env")"
graph_levels="$(awk -F= '$1 == "levels" { print $2 }' "$WORK/graph_stats.env")"
graph_widest="$(awk -F= '$1 == "widest" { print $2 }' "$WORK/graph_stats.env")"
graph_wave_sizes="$(awk -F= '$1 == "wave_sizes" { print $2 }' "$WORK/graph_stats.env")"
printf '  timed processes:    %s compile + 1 archive = %s\n' "$graph_compile_processes" "$graph_total_processes"
printf '  compile waves:      %s levels, widest %s processes\n' "$graph_levels" "$graph_widest"
printf '  wave sizes:         %s\n' "$graph_wave_sizes"

printf '\ncompiler wall time:\n'
if [ -x /usr/bin/time ]; then
  /usr/bin/time -p "$MAKE_BIN" -s -C "$WORK" -j "$JOBS" OCAMLOPT="$OCAMLOPT" all
else
  time "$MAKE_BIN" -s -C "$WORK" -j "$JOBS" OCAMLOPT="$OCAMLOPT" all
fi

if [ ! -f "$WORK/Kernel.cmxa" ]; then
  printf 'error: Kernel.cmxa was not produced\n' >&2
  exit 1
fi

printf '\noutputs:\n'
printf '  %s\n' "$WORK/Kernel.cmxa"
printf '  %s\n' "$WORK/Kernel.a"
