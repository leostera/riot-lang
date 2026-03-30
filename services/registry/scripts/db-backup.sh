#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/.."

DB_NAME="${REGISTRY_D1_DB_NAME:-riot-registry}"
BACKUP_DIR="${REGISTRY_D1_BACKUP_DIR:-./backups}"
TIMESTAMP="${REGISTRY_D1_BACKUP_TS:-$(date -u +%Y%m%dT%H%M%SZ)}"
OUTPUT_PATH="${BACKUP_DIR}/registry-d1-${DB_NAME}-${TIMESTAMP}.sql"

mkdir -p "$BACKUP_DIR"

echo "Dumping D1 database '$DB_NAME' to '$OUTPUT_PATH'"
bunx wrangler d1 export "$DB_NAME" --remote --config wrangler.toml --output "$OUTPUT_PATH"
echo "Backup completed: $OUTPUT_PATH"
