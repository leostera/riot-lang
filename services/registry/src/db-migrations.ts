import { v7 as uuidv7, validate as validateUuid, version as uuidVersion } from "uuid";

type Migration = {
  version: number;
  migrate: (db: D1Database) => Promise<void>;
};

const MIGRATION_STATE_TABLE = "registry_migrations";

const METADATA_MIGRATIONS: Migration[] = [
  {
    version: 1,
    migrate: async (db: D1Database): Promise<void> => {
      await db.exec(`
        CREATE TABLE IF NOT EXISTS users (
          user_id TEXT PRIMARY KEY,
          github_id INTEGER NOT NULL UNIQUE,
          github_login TEXT NOT NULL UNIQUE,
          github_login_lower TEXT NOT NULL UNIQUE,
          github_name TEXT,
          github_avatar_url TEXT,
          github_email TEXT,
          github_email_verified INTEGER NOT NULL DEFAULT 0,
          created_at TEXT NOT NULL,
          updated_at TEXT NOT NULL
        );
        CREATE TABLE IF NOT EXISTS user_logins (
          github_login_lower TEXT PRIMARY KEY,
          github_login TEXT NOT NULL,
          user_id TEXT NOT NULL,
          updated_at TEXT NOT NULL
        );
        CREATE TABLE IF NOT EXISTS oauth_states (
          state_id TEXT PRIMARY KEY,
          return_to TEXT NOT NULL,
          created_at TEXT NOT NULL
        );
        CREATE TABLE IF NOT EXISTS sessions (
          session_id TEXT PRIMARY KEY,
          user_id TEXT NOT NULL,
          github_login TEXT NOT NULL,
          created_at TEXT NOT NULL,
          expires_at TEXT NOT NULL
        );
        CREATE TABLE IF NOT EXISTS api_tokens (
          token_id TEXT PRIMARY KEY,
          user_id TEXT NOT NULL,
          github_login TEXT NOT NULL,
          name TEXT NOT NULL,
          secret_hash TEXT NOT NULL UNIQUE,
          capabilities_json TEXT NOT NULL,
          created_at TEXT NOT NULL,
          last_used_at TEXT,
          revoked_at TEXT
        );
        CREATE TABLE IF NOT EXISTS api_token_lookups (
          secret_hash TEXT PRIMARY KEY,
          token_id TEXT NOT NULL,
          user_id TEXT NOT NULL,
          github_login TEXT NOT NULL,
          capabilities_json TEXT NOT NULL,
          revoked_at TEXT
        );
        CREATE TABLE IF NOT EXISTS package_claims (
          package_name TEXT PRIMARY KEY,
          package_locator TEXT NOT NULL,
          source_url TEXT NOT NULL,
          package_subdir TEXT NOT NULL,
          owner_user_id TEXT,
          owner_github_login TEXT,
          claimed_at TEXT NOT NULL,
          updated_at TEXT NOT NULL
        );
        CREATE TABLE IF NOT EXISTS published_releases (
          package_name TEXT NOT NULL,
          package_version TEXT NOT NULL,
          package_locator TEXT NOT NULL,
          source_url TEXT NOT NULL,
          package_subdir TEXT NOT NULL,
          selector TEXT NOT NULL,
          resolved_sha TEXT NOT NULL,
          package_description TEXT,
          package_license TEXT,
          package_homepage TEXT,
          package_repository TEXT,
          package_root_module TEXT,
          package_categories_json TEXT NOT NULL,
          package_keywords_json TEXT NOT NULL,
          dependencies_json TEXT NOT NULL,
          source_archive_key TEXT NOT NULL,
          manifest_key TEXT NOT NULL,
          published_at TEXT NOT NULL,
          PRIMARY KEY (package_name, package_version)
        );
        CREATE TABLE IF NOT EXISTS selector_resolutions (
          package_locator TEXT NOT NULL,
          selector TEXT NOT NULL,
          resolved_sha TEXT NOT NULL,
          frozen INTEGER NOT NULL,
          recorded_at TEXT NOT NULL,
          PRIMARY KEY (package_locator, selector)
        );
        CREATE TABLE IF NOT EXISTS request_logs (
          request_id TEXT PRIMARY KEY,
          request_timestamp TEXT NOT NULL,
          method TEXT NOT NULL,
          path TEXT NOT NULL,
          route TEXT NOT NULL,
          package_locator TEXT,
          selector TEXT,
          resolved_sha TEXT,
          status INTEGER NOT NULL,
          success INTEGER NOT NULL,
          error_category TEXT,
          error_message TEXT,
          user_agent TEXT
        );
        CREATE TABLE IF NOT EXISTS registry_events (
          sequence_id INTEGER PRIMARY KEY AUTOINCREMENT,
          event_id TEXT NOT NULL UNIQUE,
          event_type TEXT NOT NULL,
          package_name TEXT,
          package_version TEXT,
          package_locator TEXT,
          payload_json TEXT NOT NULL,
          created_at TEXT NOT NULL
        );
        CREATE INDEX IF NOT EXISTS idx_sessions_user_id ON sessions(user_id);
        CREATE INDEX IF NOT EXISTS idx_api_tokens_user_id ON api_tokens(user_id);
        CREATE INDEX IF NOT EXISTS idx_api_tokens_secret_hash ON api_tokens(secret_hash);
        CREATE INDEX IF NOT EXISTS idx_claims_owner_login ON package_claims(owner_github_login);
        CREATE INDEX IF NOT EXISTS idx_releases_package_name ON published_releases(package_name);
        CREATE INDEX IF NOT EXISTS idx_selector_resolutions_locator ON selector_resolutions(package_locator);
        CREATE INDEX IF NOT EXISTS idx_request_logs_timestamp ON request_logs(request_timestamp DESC);
        CREATE INDEX IF NOT EXISTS idx_registry_events_sequence_id ON registry_events(sequence_id);
        CREATE INDEX IF NOT EXISTS idx_registry_events_created_at ON registry_events(created_at);
        CREATE INDEX IF NOT EXISTS idx_registry_events_package ON registry_events(package_name, package_version, created_at);
      `);
    },
  },
  {
    version: 2,
    migrate: async (db: D1Database): Promise<void> => {
      await ensureUserColumn(db, "users", "github_email", "TEXT");
      await ensureUserColumn(db, "users", "github_email_verified", "INTEGER NOT NULL DEFAULT 0");
    },
  },
  {
    version: 3,
    migrate: async (db: D1Database): Promise<void> => {
      const tables = await db
        .prepare(`
          SELECT name
          FROM sqlite_master
          WHERE type = 'table' AND name = 'registry_events'
        `)
        .all<{ name: string }>();

      if ((tables.results?.length ?? 0) === 0) {
        await db
          .prepare(
            `CREATE TABLE registry_events (
              sequence_id INTEGER PRIMARY KEY AUTOINCREMENT,
              event_id TEXT NOT NULL UNIQUE,
              event_type TEXT NOT NULL,
              package_name TEXT,
              package_version TEXT,
              package_locator TEXT,
              payload_json TEXT NOT NULL,
              created_at TEXT NOT NULL
            )`,
          )
          .run();
        await db
          .prepare(
            `CREATE INDEX IF NOT EXISTS idx_registry_events_sequence_id ON registry_events(sequence_id)`,
          )
          .run();
        await db
          .prepare(
            `CREATE INDEX IF NOT EXISTS idx_registry_events_created_at ON registry_events(created_at)`,
          )
          .run();
        await db
          .prepare(
            `CREATE INDEX IF NOT EXISTS idx_registry_events_package ON registry_events(package_name, package_version, created_at)`,
          )
          .run();
        return;
      }

      const columns = await db.prepare(`PRAGMA table_info(registry_events);`).all<{ name: string }>();
      const hasSequenceId = (columns.results ?? []).some((column) => column.name === "sequence_id");
      if (hasSequenceId) {
        await db.exec(`
          CREATE INDEX IF NOT EXISTS idx_registry_events_sequence_id ON registry_events(sequence_id);
        `);
        return;
      }

      await db.exec(`
        ALTER TABLE registry_events RENAME TO registry_events_legacy;

        CREATE TABLE registry_events (
          sequence_id INTEGER PRIMARY KEY AUTOINCREMENT,
          event_id TEXT NOT NULL UNIQUE,
          event_type TEXT NOT NULL,
          package_name TEXT,
          package_version TEXT,
          package_locator TEXT,
          payload_json TEXT NOT NULL,
          created_at TEXT NOT NULL
        );

        INSERT INTO registry_events (
          event_id,
          event_type,
          package_name,
          package_version,
          package_locator,
          payload_json,
          created_at
        )
        SELECT
          event_id,
          event_type,
          package_name,
          package_version,
          package_locator,
          payload_json,
          created_at
        FROM registry_events_legacy
        ORDER BY created_at ASC, rowid ASC;

        DROP TABLE registry_events_legacy;

        CREATE INDEX IF NOT EXISTS idx_registry_events_sequence_id ON registry_events(sequence_id);
        CREATE INDEX IF NOT EXISTS idx_registry_events_created_at ON registry_events(created_at);
        CREATE INDEX IF NOT EXISTS idx_registry_events_package ON registry_events(package_name, package_version, created_at);
      `);
    },
  },
  {
    version: 4,
    migrate: async (db: D1Database): Promise<void> => {
      const rows = await db
        .prepare(`
          SELECT
            event_id,
            created_at
          FROM registry_events
          ORDER BY created_at ASC, rowid ASC
        `)
        .all<{ event_id: string; created_at: string }>();

      let sequence = 0;
      for (const row of rows.results ?? []) {
        const eventId = row.event_id;
        if (isUuidV7(eventId)) {
          continue;
        }

        const createdAt = Date.parse(row.created_at);
        const msecs = Number.isFinite(createdAt) ? createdAt : Date.now();
        const nextEventId = uuidv7({
          msecs,
          seq: sequence,
        });
        sequence += 1;

        await db
          .prepare(`
            UPDATE registry_events
            SET event_id = ?
            WHERE event_id = ?
          `)
          .bind(nextEventId, eventId)
          .run();
      }
    },
  },
  {
    version: 5,
    migrate: async (db: D1Database): Promise<void> => {
      await db.exec(`
        CREATE TABLE IF NOT EXISTS request_logs (
          request_id TEXT PRIMARY KEY,
          request_timestamp TEXT NOT NULL,
          method TEXT NOT NULL,
          path TEXT NOT NULL,
          route TEXT NOT NULL,
          package_locator TEXT,
          selector TEXT,
          resolved_sha TEXT,
          status INTEGER NOT NULL,
          success INTEGER NOT NULL,
          error_category TEXT,
          error_message TEXT,
          user_agent TEXT
        );
        CREATE INDEX IF NOT EXISTS idx_request_logs_timestamp ON request_logs(request_timestamp DESC);
        DROP TABLE IF EXISTS web_views;
      `);
    },
  },
];

const SEARCH_MIGRATIONS: Migration[] = [
  {
    version: 1,
    migrate: async (db: D1Database): Promise<void> => {
      await db.exec(`
        CREATE TABLE IF NOT EXISTS packages (
          package_name TEXT PRIMARY KEY,
          normalized_name TEXT NOT NULL,
          latest_version TEXT NOT NULL,
          description TEXT,
          license TEXT,
          homepage TEXT,
          repository TEXT,
          root_module TEXT,
          canonical_locator TEXT NOT NULL,
          repo_url TEXT NOT NULL,
          repo_owner TEXT NOT NULL,
          repo_name TEXT NOT NULL,
          subdir TEXT NOT NULL,
          release_count INTEGER NOT NULL,
          updated_at TEXT NOT NULL
        )
      `);
      await db.exec(`
        CREATE VIRTUAL TABLE IF NOT EXISTS package_search USING fts5 (
          package_name,
          description,
          repo_owner,
          repo_name,
          subdir,
          repository,
          tokenize = 'unicode61 remove_diacritics 2'
        )
      `);
    },
  },
];

export async function applyMetadataMigrations(db: D1Database): Promise<void> {
  await runMigrations(db, "registry_metadata", METADATA_MIGRATIONS);
}

export async function applySearchMigrations(db: D1Database): Promise<void> {
  await runMigrations(db, "registry_search", SEARCH_MIGRATIONS);
}

async function runMigrations(
  db: D1Database,
  scope: string,
  migrations: Migration[],
): Promise<void> {
  await ensureMigrationStateTable(db);

  const currentVersion = await getMigrationVersion(db, scope);
  for (const migration of migrations) {
    if (migration.version <= currentVersion) {
      continue;
    }

    await migration.migrate(db);
    await setMigrationVersion(db, scope, migration.version);
  }
}

async function ensureMigrationStateTable(db: D1Database): Promise<void> {
  await db.exec(
    "CREATE TABLE IF NOT EXISTS " +
      `${MIGRATION_STATE_TABLE} (` +
      "scope TEXT PRIMARY KEY, " +
      "version INTEGER NOT NULL" +
      ");",
  );
}

async function getMigrationVersion(db: D1Database, scope: string): Promise<number> {
  const rows = await db
    .prepare(`SELECT version FROM ${MIGRATION_STATE_TABLE} WHERE scope = ?`)
    .bind(scope)
    .all<{ scope: string; version: number }>();

  return rows.results?.[0]?.version ?? 0;
}

async function setMigrationVersion(
  db: D1Database,
  scope: string,
  version: number,
): Promise<void> {
  await db
    .prepare(
      `INSERT INTO ${MIGRATION_STATE_TABLE} (scope, version)
       VALUES (?, ?)
       ON CONFLICT(scope) DO UPDATE SET
         version = excluded.version`,
    )
    .bind(scope, version)
    .run();
}

async function ensureUserColumn(
  db: D1Database,
  table: string,
  column: string,
  definition: string,
): Promise<void> {
  const columns = await db.prepare(`PRAGMA table_info(${table});`).all<{ name: string }>();
  const names = new Set((columns.results ?? []).map((item) => item.name));
  if (names.has(column)) {
    return;
  }

  await db.prepare(`ALTER TABLE ${table} ADD COLUMN ${column} ${definition};`).run();
}

function isUuidV7(value: string): boolean {
  return validateUuid(value) && uuidVersion(value) === 7;
}
