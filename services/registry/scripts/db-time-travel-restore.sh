#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<EOF
Usage:
  $0 --timestamp <RFC3339-or-epoch-seconds>
  $0 --bookmark <bookmark>
EOF
  exit 1
}

if [[ $# -ne 2 ]]; then
  usage
fi

cd "$(dirname "$0")/.."

DB_NAME="${REGISTRY_D1_DB_NAME:-riot-registry}"
MODE="$1"
VALUE="$2"

case "$MODE" in
  --timestamp)
    echo "Restoring '$DB_NAME' to timestamp '$VALUE'"
    bunx wrangler d1 time-travel restore "$DB_NAME" --timestamp "$VALUE"
    ;;
  --bookmark)
    echo "Restoring '$DB_NAME' to bookmark '$VALUE'"
    bunx wrangler d1 time-travel restore "$DB_NAME" --bookmark "$VALUE"
    ;;
  *)
    usage
    ;;
esac

echo "Rollback restore request sent to Cloudflare. Check status in the dashboard if needed."
