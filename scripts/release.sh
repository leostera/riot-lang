#!/bin/bash

set -euo pipefail

SCRIPT_DIR="$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)"
REPO_ROOT="$(CDPATH= cd -- "$SCRIPT_DIR/.." && pwd)"
RELEASE_REMOTE="${RELEASE_REMOTE:-origin}"
RELEASE_BRANCH="${RELEASE_BRANCH:-main}"
DRY_RUN=0

usage() {
  cat <<'EOF'
Usage: ./scripts/release.sh [--dry-run]

Preflight a Riot release, then delegate the release prep and binary publish flow
to `codex exec`.

This script:
1. Requires a clean git worktree with no in-flight git operations.
2. Requires the current branch to be up to date with origin/main.
3. Asks Codex to:
   - read the current version from packages/riot-cli/riot.toml
   - compute the next patch version
   - bump all real workspace riot.toml versions by one patch
   - update CHANGELOG.md from the last reachable semver tag to HEAD
   - commit the release prep
   - create the new semver tag
   - run ./scripts/release/riot.sh all

Environment:
  RELEASE_REMOTE   default: origin
  RELEASE_BRANCH   default: main

Examples:
  ./scripts/release.sh
  ./scripts/release.sh --dry-run
EOF
}

die() {
  echo "error: $*" >&2
  exit 1
}

run_cmd() {
  echo "+ $*"
  "$@"
}

require_clean_worktree() {
  git update-index -q --refresh

  if ! git diff --quiet --ignore-submodules --; then
    die "worktree has unstaged changes"
  fi

  if ! git diff --cached --quiet --ignore-submodules --; then
    die "worktree has staged but uncommitted changes"
  fi

  if [ -n "$(git ls-files --others --exclude-standard)" ]; then
    die "worktree has untracked files"
  fi
}

require_no_git_operation_in_progress() {
  local git_dir
  git_dir="$(git rev-parse --git-dir)"

  local markers=(
    "MERGE_HEAD"
    "CHERRY_PICK_HEAD"
    "REVERT_HEAD"
    "REBASE_HEAD"
    "BISECT_LOG"
    "rebase-apply"
    "rebase-merge"
    "sequencer"
  )

  local marker
  for marker in "${markers[@]}"; do
    if [ -e "$git_dir/$marker" ]; then
      die "git operation in progress ($marker)"
    fi
  done
}

require_branch_main() {
  local current_branch
  current_branch="$(git branch --show-current)"

  if [ "$current_branch" != "$RELEASE_BRANCH" ]; then
    die "expected to run from $RELEASE_BRANCH, found $current_branch"
  fi
}

require_up_to_date_with_remote_main() {
  local remote_ref="$RELEASE_REMOTE/$RELEASE_BRANCH"
  local head remote_head merge_base

  run_cmd git fetch --tags "$RELEASE_REMOTE" "$RELEASE_BRANCH"

  head="$(git rev-parse HEAD)"
  remote_head="$(git rev-parse "$remote_ref")"
  merge_base="$(git merge-base HEAD "$remote_ref")"

  if [ "$head" = "$remote_head" ]; then
    return 0
  fi

  if [ "$head" = "$merge_base" ]; then
    die "local branch is behind $remote_ref"
  fi

  if [ "$remote_head" = "$merge_base" ]; then
    die "local branch is ahead of $remote_ref; push or reconcile before releasing"
  fi

  die "local branch has diverged from $remote_ref"
}

last_reachable_semver_tag() {
  git describe --tags --abbrev=0 --match '[0-9]*.[0-9]*.[0-9]*'
}

build_codex_prompt() {
  local last_tag="$1"

  cat <<EOF
You are preparing a Riot release in $REPO_ROOT.

Last reachable release tag: $last_tag
Changelog diff range: $last_tag..HEAD

Do exactly these steps:

1. Read the current release version from \`packages/riot-cli/riot.toml\`.

2. Compute the next patch version.
   - Example: \`0.0.6 -> 0.0.7\`
   - Refuse to proceed if a git tag with that exact version already exists.

3. Update every real workspace and service \`riot.toml\` manifest version from the current version to the next patch version.
   - Leave fixture manifests under tests/workspace fixtures pinned as they are.

4. Update \`CHANGELOG.md\` with a new entry for the next patch version based on the diff range \`$last_tag..HEAD\`.
   - Keep it concise and user-facing.
   - Group by meaningful release themes instead of dumping raw commit subjects.

5. Commit the release prep changes with the conventional commit message:
   - \`chore(release): prepare <next-version>\`

6. Create an annotated git tag:
   - name: \`<next-version>\`
   - message: \`<next-version>\`

7. Run:
   - \`./scripts/release/riot.sh all\`

8. In your final response, report:
   - the current version you found
   - the next version you released
   - the commit SHA
   - the tag created
   - whether \`./scripts/release/riot.sh all\` completed successfully

Important constraints:
- Work only in this repository.
- Do not touch fixture versions that are intentionally not part of the workspace release.
- Stop and report clearly if any step fails.
EOF
}

while [ $# -gt 0 ]; do
  case "$1" in
    --dry-run)
      DRY_RUN=1
      shift
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      usage >&2
      die "unexpected argument: $1"
      ;;
  esac
done

cd "$REPO_ROOT"

command -v git >/dev/null 2>&1 || die "git is required"
command -v codex >/dev/null 2>&1 || die "codex is required"
[ -f "$REPO_ROOT/packages/riot-cli/riot.toml" ] || die "manifest not found at packages/riot-cli/riot.toml"
[ -x "$REPO_ROOT/scripts/release/riot.sh" ] || die "scripts/release/riot.sh is missing or not executable"

require_no_git_operation_in_progress
require_clean_worktree
require_branch_main
require_up_to_date_with_remote_main

LAST_TAG="$(last_reachable_semver_tag)" || die "could not find a reachable semver tag"
PROMPT="$(build_codex_prompt "$LAST_TAG")"

echo "==> Last tag: $LAST_TAG"

if [ "$DRY_RUN" = "1" ]; then
  echo
  echo "==> Codex prompt"
  echo "----------------------------------------"
  printf '%s\n' "$PROMPT"
  exit 0
fi

printf '%s\n' "$PROMPT" | codex exec \
  --dangerously-bypass-approvals-and-sandbox \
  --color never \
  -C "$REPO_ROOT" \
  -
