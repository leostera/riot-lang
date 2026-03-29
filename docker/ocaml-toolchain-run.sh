#!/bin/bash

set -euo pipefail

TARGET="${1:-}"
CLEAN_BUILD="${2:-0}"
SOURCE_DIR="/src/vendor/ocaml"
WORK_DIR="/work/vendor/ocaml"
OUTPUT_DIR="/out"

die() {
  echo "error: $*" >&2
  exit 1
}

[ -n "$TARGET" ] || die "target is required"
[ -d "$SOURCE_DIR" ] || die "vendored OCaml source not mounted at $SOURCE_DIR"

mkdir -p /work "$OUTPUT_DIR"

if [ "$CLEAN_BUILD" != "0" ]; then
  rm -rf "$WORK_DIR"
fi

mkdir -p "$WORK_DIR"

# Sync tracked and untracked source files into the cached worktree without
# copying ignored build outputs. That keeps the Linux cache incremental while
# avoiding Mach-O and other host-specific artefacts from the source checkout.
git -C "$SOURCE_DIR" ls-files -z --cached --others --exclude-standard | \
  rsync -a --from0 --files-from=- "$SOURCE_DIR"/ "$WORK_DIR"/

cd "$WORK_DIR"
bash ./cross/build.sh "$TARGET"
bash ./cross/package.sh "$TARGET" "$OUTPUT_DIR"

tarball="$(find "$OUTPUT_DIR" -maxdepth 1 -type f -name 'ocaml-*.tar.gz' | head -n 1)"
[ -n "$tarball" ] || die "package step did not produce a tarball for $TARGET"

(cd "$OUTPUT_DIR" && sha256sum "$(basename "$tarball")" > "$(basename "$tarball").sha256")

bash ./cross/test-relocatable.sh "$TARGET"
