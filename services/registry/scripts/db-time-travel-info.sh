#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/.."

DB_NAME="${REGISTRY_D1_DB_NAME:-riot-registry}"
TIMESTAMP="${1:-}"

if [[ -n "$TIMESTAMP" ]]; then
  echo "Fetching time-travel snapshot info for '$DB_NAME' at '$TIMESTAMP'"
  bunx wrangler d1 time-travel info "$DB_NAME" --timestamp "$TIMESTAMP" --json
else
  echo "Fetching latest time-travel snapshot info for '$DB_NAME'"
  bunx wrangler d1 time-travel info "$DB_NAME" --json
fi
