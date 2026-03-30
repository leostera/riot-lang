#!/usr/bin/env bash
set -euo pipefail

if [[ $# -ne 1 ]]; then
  echo "Usage: $0 <backup.sql>"
  echo "Pass a SQL export file that contains CREATE TABLE statements and inserts."
  exit 1
fi

cd "$(dirname "$0")/.."

DB_NAME="${REGISTRY_D1_DB_NAME:-riot-registry}"
BACKUP_FILE="$1"

if [[ ! -f "$BACKUP_FILE" ]]; then
  echo "Backup file not found: $BACKUP_FILE"
  exit 1
fi

echo "Applying backup '$BACKUP_FILE' to database '$DB_NAME'"
read -r -p "This will run the backup SQL directly against the live database. Type 'restore' to continue: " confirmation
if [[ "$confirmation" != "restore" ]]; then
  echo "Aborted."
  exit 1
fi

bunx wrangler d1 execute "$DB_NAME" --remote --config wrangler.toml --file "$BACKUP_FILE"
echo "Restore command finished."
