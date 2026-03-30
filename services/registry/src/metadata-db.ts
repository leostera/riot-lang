import type {
  ApiTokenCapability,
  ApiTokenLookupRecord,
  ApiTokenRecord,
  CategoriesIndexDocument,
  CategorySummary,
  OAuthStateRecord,
  OwnerPackagesDocument,
  PackageClaimRecord,
  PackageRelationDependency,
  PackageRelationDependent,
  PackageOverviewDocument,
  WebPackageListItem,
  PackagePublicationManifest,
  PackageRelationsDocument,
  PopularPackagesDocument,
  PublishedReleaseRecord,
  RecentPackagesDocument,
  RegistryEventRecord,
  RequestLogEntry,
  SelectorResolutionRecord,
  SessionRecord,
  UserLoginRecord,
  UserRecord,
} from "./types.ts";
import { applyMetadataMigrations as applyD1Migrations } from "./db-migrations.ts";

export async function applyMetadataMigrations(db: D1Database): Promise<void> {
  await applyD1Migrations(db);
}

export async function writeRequestLog(db: D1Database, entry: RequestLogEntry): Promise<void> {
  await db
    .prepare(
      `INSERT INTO request_logs (
         request_id,
         request_timestamp,
         method,
         path,
         route,
         package_locator,
         selector,
         resolved_sha,
         status,
         success,
         error_category,
         error_message,
         user_agent
       ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)`,
    )
    .bind(
      entry.request_id,
      entry.request_timestamp,
      entry.method,
      entry.path,
      entry.route,
      entry.package_locator ?? null,
      entry.selector ?? null,
      entry.resolved_sha ?? null,
      entry.status,
      entry.success ? 1 : 0,
      entry.error_category ?? null,
      entry.error_message ?? null,
      entry.user_agent ?? null,
    )
    .run();
}

export async function listRequestLogs(
  db: D1Database,
  limit = 100,
): Promise<RequestLogEntry[]> {
  const rows = await db
    .prepare(
      `SELECT
         request_id,
         request_timestamp,
         method,
         path,
         route,
         package_locator,
         selector,
         resolved_sha,
         status,
         success,
         error_category,
         error_message,
         user_agent
       FROM request_logs
       ORDER BY request_timestamp DESC
       LIMIT ?`,
    )
    .bind(limit)
    .all<RequestLogRow>();

  return (rows.results ?? []).map(parseRequestLogRecord);
}

export async function readUserRecord(db: D1Database, userId: string): Promise<UserRecord | null> {
  const rows = await db
    .prepare(
      `SELECT
         user_id,
         github_id,
         github_login,
         github_name,
         github_avatar_url,
         github_email,
         github_email_verified,
         created_at,
         updated_at
       FROM users
       WHERE user_id = ?`,
    )
    .bind(userId)
    .all<UserRow>();

  return rows.results?.[0] ? parseUserRecord(rows.results[0]) : null;
}

export async function writeUserRecord(db: D1Database, record: UserRecord): Promise<void> {
  await db
    .prepare(
      `INSERT INTO users (
         user_id,
         github_id,
         github_login,
         github_login_lower,
         github_name,
         github_avatar_url,
         github_email,
         github_email_verified,
         created_at,
         updated_at
       ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
       ON CONFLICT(user_id) DO UPDATE SET
         github_id = excluded.github_id,
         github_login = excluded.github_login,
         github_login_lower = excluded.github_login_lower,
         github_name = excluded.github_name,
         github_avatar_url = excluded.github_avatar_url,
         github_email = excluded.github_email,
         github_email_verified = excluded.github_email_verified,
         updated_at = excluded.updated_at`,
    )
    .bind(
      record.user_id,
      record.github_id,
      record.github_login,
      record.github_login.toLowerCase(),
      record.github_name ?? null,
      record.github_avatar_url ?? null,
      record.github_email ?? null,
      record.github_email_verified ? 1 : 0,
      record.created_at,
      record.updated_at,
    )
    .run();
}

export async function readUserLoginRecord(
  db: D1Database,
  githubLogin: string,
): Promise<UserLoginRecord | null> {
  const rows = await db
    .prepare(
      `SELECT
         github_login,
         user_id,
         updated_at
       FROM user_logins
       WHERE github_login_lower = ?`,
    )
    .bind(githubLogin.toLowerCase())
    .all<UserLoginRecord>();

  return rows.results?.[0] ?? null;
}

export async function writeUserLoginRecord(
  db: D1Database,
  record: UserLoginRecord,
): Promise<void> {
  await db
    .prepare(
      `INSERT INTO user_logins (
         github_login_lower,
         github_login,
         user_id,
         updated_at
       ) VALUES (?, ?, ?, ?)
       ON CONFLICT(github_login_lower) DO UPDATE SET
         github_login = excluded.github_login,
         user_id = excluded.user_id,
         updated_at = excluded.updated_at`,
    )
    .bind(
      record.github_login.toLowerCase(),
      record.github_login,
      record.user_id,
      record.updated_at,
    )
    .run();
}

export async function readOAuthStateRecord(
  db: D1Database,
  stateId: string,
): Promise<OAuthStateRecord | null> {
  const rows = await db
    .prepare(
      `SELECT
         state_id,
         return_to,
         created_at
       FROM oauth_states
       WHERE state_id = ?`,
    )
    .bind(stateId)
    .all<OAuthStateRecord>();

  return rows.results?.[0] ?? null;
}

export async function writeOAuthStateRecord(
  db: D1Database,
  record: OAuthStateRecord,
): Promise<void> {
  await db
    .prepare(
      `INSERT INTO oauth_states (state_id, return_to, created_at)
       VALUES (?, ?, ?)
       ON CONFLICT(state_id) DO UPDATE SET
         return_to = excluded.return_to,
         created_at = excluded.created_at`,
    )
    .bind(record.state_id, record.return_to, record.created_at)
    .run();
}

export async function deleteOAuthStateRecord(db: D1Database, stateId: string): Promise<void> {
  await db.prepare("DELETE FROM oauth_states WHERE state_id = ?").bind(stateId).run();
}

export async function readSessionRecord(
  db: D1Database,
  sessionId: string,
): Promise<SessionRecord | null> {
  const rows = await db
    .prepare(
      `SELECT
         session_id,
         user_id,
         github_login,
         created_at,
         expires_at
       FROM sessions
       WHERE session_id = ?`,
    )
    .bind(sessionId)
    .all<SessionRecord>();

  return rows.results?.[0] ?? null;
}

export async function writeSessionRecord(
  db: D1Database,
  record: SessionRecord,
): Promise<void> {
  await db
    .prepare(
      `INSERT INTO sessions (
         session_id,
         user_id,
         github_login,
         created_at,
         expires_at
       ) VALUES (?, ?, ?, ?, ?)
       ON CONFLICT(session_id) DO UPDATE SET
         user_id = excluded.user_id,
         github_login = excluded.github_login,
         created_at = excluded.created_at,
         expires_at = excluded.expires_at`,
    )
    .bind(
      record.session_id,
      record.user_id,
      record.github_login,
      record.created_at,
      record.expires_at,
    )
    .run();
}

export async function deleteSessionRecord(db: D1Database, sessionId: string): Promise<void> {
  await db.prepare("DELETE FROM sessions WHERE session_id = ?").bind(sessionId).run();
}

export async function readApiTokenRecord(
  db: D1Database,
  userId: string,
  tokenId: string,
): Promise<ApiTokenRecord | null> {
  const rows = await db
    .prepare(
      `SELECT
         token_id,
         user_id,
         github_login,
         name,
         secret_hash,
         capabilities_json,
         created_at,
         last_used_at,
         revoked_at
       FROM api_tokens
       WHERE user_id = ? AND token_id = ?`,
    )
    .bind(userId, tokenId)
    .all<ApiTokenRow>();

  return rows.results?.[0] ? parseApiTokenRecord(rows.results[0]) : null;
}

export async function writeApiTokenRecord(
  db: D1Database,
  record: ApiTokenRecord,
): Promise<void> {
  await db
    .prepare(
      `INSERT INTO api_tokens (
         token_id,
         user_id,
         github_login,
         name,
         secret_hash,
         capabilities_json,
         created_at,
         last_used_at,
         revoked_at
       ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
       ON CONFLICT(token_id) DO UPDATE SET
         user_id = excluded.user_id,
         github_login = excluded.github_login,
         name = excluded.name,
         secret_hash = excluded.secret_hash,
         capabilities_json = excluded.capabilities_json,
         created_at = excluded.created_at,
         last_used_at = excluded.last_used_at,
         revoked_at = excluded.revoked_at`,
    )
    .bind(
      record.token_id,
      record.user_id,
      record.github_login,
      record.name,
      record.secret_hash,
      JSON.stringify(record.capabilities),
      record.created_at,
      record.last_used_at ?? null,
      record.revoked_at ?? null,
    )
    .run();
}

export async function listApiTokenRecords(
  db: D1Database,
  userId: string,
): Promise<ApiTokenRecord[]> {
  const rows = await db
    .prepare(
      `SELECT
         token_id,
         user_id,
         github_login,
         name,
         secret_hash,
         capabilities_json,
         created_at,
         last_used_at,
         revoked_at
       FROM api_tokens
       WHERE user_id = ?
       ORDER BY created_at DESC`,
    )
    .bind(userId)
    .all<ApiTokenRow>();

  return (rows.results ?? []).map(parseApiTokenRecord);
}

export async function readApiTokenLookupRecord(
  db: D1Database,
  tokenHash: string,
): Promise<ApiTokenLookupRecord | null> {
  const rows = await db
    .prepare(
      `SELECT
         token_id,
         user_id,
         github_login,
         capabilities_json,
         revoked_at
       FROM api_token_lookups
       WHERE secret_hash = ?`,
    )
    .bind(tokenHash)
    .all<ApiTokenLookupRow>();

  return rows.results?.[0] ? parseApiTokenLookupRecord(rows.results[0]) : null;
}

export async function writeApiTokenLookupRecord(
  db: D1Database,
  tokenHash: string,
  record: ApiTokenLookupRecord,
): Promise<void> {
  await db
    .prepare(
      `INSERT INTO api_token_lookups (
         secret_hash,
         token_id,
         user_id,
         github_login,
         capabilities_json,
         revoked_at
       ) VALUES (?, ?, ?, ?, ?, ?)
       ON CONFLICT(secret_hash) DO UPDATE SET
         token_id = excluded.token_id,
         user_id = excluded.user_id,
         github_login = excluded.github_login,
         capabilities_json = excluded.capabilities_json,
         revoked_at = excluded.revoked_at`,
    )
    .bind(
      tokenHash,
      record.token_id,
      record.user_id,
      record.github_login,
      JSON.stringify(record.capabilities),
      record.revoked_at ?? null,
    )
    .run();
}

export async function readSelectorResolution(
  db: D1Database,
  packageLocator: string,
  selector: string,
): Promise<SelectorResolutionRecord | null> {
  const rows = await db
    .prepare(
      `SELECT
         package_locator,
         selector,
         resolved_sha,
         frozen,
         recorded_at
       FROM selector_resolutions
       WHERE package_locator = ? AND selector = ?`,
    )
    .bind(packageLocator, selector)
    .all<SelectorResolutionRow>();

  return rows.results?.[0] ? parseSelectorResolution(rows.results[0]) : null;
}

export async function writeSelectorResolution(
  db: D1Database,
  record: SelectorResolutionRecord,
): Promise<void> {
  await db
    .prepare(
      `INSERT INTO selector_resolutions (
         package_locator,
         selector,
         resolved_sha,
         frozen,
         recorded_at
       ) VALUES (?, ?, ?, ?, ?)
       ON CONFLICT(package_locator, selector) DO UPDATE SET
         resolved_sha = excluded.resolved_sha,
         frozen = excluded.frozen,
         recorded_at = excluded.recorded_at`,
    )
    .bind(
      record.package_locator,
      record.selector,
      record.resolved_sha,
      record.frozen ? 1 : 0,
      record.recorded_at,
    )
    .run();
}

export async function readPackageClaim(
  db: D1Database,
  packageName: string,
): Promise<PackageClaimRecord | null> {
  const rows = await db
    .prepare(
      `SELECT
         package_name,
         package_locator,
         source_url,
         package_subdir,
         owner_user_id,
         owner_github_login,
         claimed_at,
         updated_at
       FROM package_claims
       WHERE package_name = ?`,
    )
    .bind(packageName)
    .all<PackageClaimRow>();

  return rows.results?.[0] ? parsePackageClaimRecord(rows.results[0]) : null;
}

export async function writePackageClaim(
  db: D1Database,
  record: PackageClaimRecord,
): Promise<void> {
  await db
    .prepare(
      `INSERT INTO package_claims (
         package_name,
         package_locator,
         source_url,
         package_subdir,
         owner_user_id,
         owner_github_login,
         claimed_at,
         updated_at
       ) VALUES (?, ?, ?, ?, ?, ?, ?, ?)
       ON CONFLICT(package_name) DO UPDATE SET
         package_locator = excluded.package_locator,
         source_url = excluded.source_url,
         package_subdir = excluded.package_subdir,
         owner_user_id = excluded.owner_user_id,
         owner_github_login = excluded.owner_github_login,
         claimed_at = excluded.claimed_at,
         updated_at = excluded.updated_at`,
    )
    .bind(
      record.package_name,
      record.package_locator,
      record.source_url,
      record.package_subdir,
      record.owner_user_id ?? null,
      record.owner_github_login ?? null,
      record.claimed_at,
      record.updated_at,
    )
    .run();
}

export async function readPublishedRelease(
  db: D1Database,
  packageName: string,
  version: string,
): Promise<PublishedReleaseRecord | null> {
  const rows = await db
    .prepare(
      `SELECT
         package_name,
         package_version,
         package_locator,
         source_url,
         package_subdir,
         selector,
         resolved_sha,
         package_description,
         package_license,
         package_homepage,
         package_repository,
         package_root_module,
         package_categories_json,
         package_keywords_json,
         dependencies_json,
         source_archive_key,
         manifest_key,
         published_at
       FROM published_releases
       WHERE package_name = ? AND package_version = ?`,
    )
    .bind(packageName, version)
    .all<PublishedReleaseRow>();

  return rows.results?.[0] ? parsePublishedReleaseRecord(rows.results[0]) : null;
}

export async function hasPublishedRelease(db: D1Database, packageName: string): Promise<boolean> {
  const rows = await db
    .prepare(
      `SELECT
         package_name
       FROM published_releases
       WHERE package_name = ?
       LIMIT 1`,
    )
    .bind(packageName)
    .all<{ package_name: string }>();

  return (rows.results?.length ?? 0) > 0;
}

export async function writePublishedRelease(
  db: D1Database,
  record: PublishedReleaseRecord,
): Promise<void> {
  await db
    .prepare(
      `INSERT INTO published_releases (
         package_name,
         package_version,
         package_locator,
         source_url,
         package_subdir,
         selector,
         resolved_sha,
         package_description,
         package_license,
         package_homepage,
         package_repository,
         package_root_module,
         package_categories_json,
         package_keywords_json,
         dependencies_json,
         source_archive_key,
         manifest_key,
         published_at
       ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
       ON CONFLICT(package_name, package_version) DO UPDATE SET
         package_locator = excluded.package_locator,
         source_url = excluded.source_url,
         package_subdir = excluded.package_subdir,
         selector = excluded.selector,
         resolved_sha = excluded.resolved_sha,
         package_description = excluded.package_description,
         package_license = excluded.package_license,
         package_homepage = excluded.package_homepage,
         package_repository = excluded.package_repository,
         package_root_module = excluded.package_root_module,
         package_categories_json = excluded.package_categories_json,
         package_keywords_json = excluded.package_keywords_json,
         dependencies_json = excluded.dependencies_json,
         source_archive_key = excluded.source_archive_key,
         manifest_key = excluded.manifest_key,
         published_at = excluded.published_at`,
    )
    .bind(
      record.package_name,
      record.package_version,
      record.package_locator,
      record.source_url,
      record.package_subdir,
      record.selector,
      record.resolved_sha,
      record.package_description ?? null,
      record.package_license ?? null,
      record.package_homepage ?? null,
      record.package_repository ?? null,
      record.package_root_module ?? null,
      JSON.stringify(record.package_categories ?? []),
      JSON.stringify(record.package_keywords ?? []),
      JSON.stringify(record.dependencies),
      record.source_archive_key,
      record.manifest_key,
      record.published_at,
    )
    .run();
}

export function prepareWritePackageClaim(
  db: D1Database,
  record: PackageClaimRecord,
): D1PreparedStatement {
  return db
    .prepare(
      `INSERT INTO package_claims (
         package_name,
         package_locator,
         source_url,
         package_subdir,
         owner_user_id,
         owner_github_login,
         claimed_at,
         updated_at
       ) VALUES (?, ?, ?, ?, ?, ?, ?, ?)
       ON CONFLICT(package_name) DO UPDATE SET
         package_locator = excluded.package_locator,
         source_url = excluded.source_url,
         package_subdir = excluded.package_subdir,
         owner_user_id = excluded.owner_user_id,
         owner_github_login = excluded.owner_github_login,
         claimed_at = excluded.claimed_at,
         updated_at = excluded.updated_at`,
    )
    .bind(
      record.package_name,
      record.package_locator,
      record.source_url,
      record.package_subdir,
      record.owner_user_id ?? null,
      record.owner_github_login ?? null,
      record.claimed_at,
      record.updated_at,
    );
}

export function prepareWritePublishedRelease(
  db: D1Database,
  record: PublishedReleaseRecord,
): D1PreparedStatement {
  return db
    .prepare(
      `INSERT INTO published_releases (
         package_name,
         package_version,
         package_locator,
         source_url,
         package_subdir,
         selector,
         resolved_sha,
         package_description,
         package_license,
         package_homepage,
         package_repository,
         package_root_module,
         package_categories_json,
         package_keywords_json,
         dependencies_json,
         source_archive_key,
         manifest_key,
         published_at
       ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
       ON CONFLICT(package_name, package_version) DO UPDATE SET
         package_locator = excluded.package_locator,
         source_url = excluded.source_url,
         package_subdir = excluded.package_subdir,
         selector = excluded.selector,
         resolved_sha = excluded.resolved_sha,
         package_description = excluded.package_description,
         package_license = excluded.package_license,
         package_homepage = excluded.package_homepage,
         package_repository = excluded.package_repository,
         package_root_module = excluded.package_root_module,
         package_categories_json = excluded.package_categories_json,
         package_keywords_json = excluded.package_keywords_json,
         dependencies_json = excluded.dependencies_json,
         source_archive_key = excluded.source_archive_key,
         manifest_key = excluded.manifest_key,
         published_at = excluded.published_at`,
    )
    .bind(
      record.package_name,
      record.package_version,
      record.package_locator,
      record.source_url,
      record.package_subdir,
      record.selector,
      record.resolved_sha,
      record.package_description ?? null,
      record.package_license ?? null,
      record.package_homepage ?? null,
      record.package_repository ?? null,
      record.package_root_module ?? null,
      JSON.stringify(record.package_categories ?? []),
      JSON.stringify(record.package_keywords ?? []),
      JSON.stringify(record.dependencies),
      record.source_archive_key,
      record.manifest_key,
      record.published_at,
    );
}

export async function writeRegistryEvent(
  db: D1Database,
  record: RegistryEventRecord,
): Promise<void> {
  await prepareWriteRegistryEvent(db, record).run();
}

export function prepareWriteRegistryEvent(
  db: D1Database,
  record: RegistryEventRecord,
): D1PreparedStatement {
  return db
    .prepare(
      `INSERT INTO registry_events (
         event_id,
         event_type,
         package_name,
         package_version,
         package_locator,
         payload_json,
         created_at
       ) VALUES (?, ?, ?, ?, ?, ?, ?)`,
    )
    .bind(
      record.event_id,
      record.event_type,
      record.package_name ?? null,
      record.package_version ?? null,
      record.package_locator ?? null,
      JSON.stringify(record.payload),
      record.created_at,
    );
}

export async function listRegistryEvents(
  db: D1Database,
  limit = 100,
  after?: string,
): Promise<RegistryEventRecord[]> {
  const rows = after === undefined
    ? await db
        .prepare(
          `SELECT
             event_id,
             event_type,
             package_name,
             package_version,
             package_locator,
             payload_json,
             created_at
           FROM registry_events
           ORDER BY event_id DESC
           LIMIT ?`,
        )
        .bind(limit)
        .all<RegistryEventRow>()
    : await db
        .prepare(
          `SELECT
             event_id,
             event_type,
             package_name,
             package_version,
             package_locator,
             payload_json,
             created_at
           FROM registry_events
           WHERE event_id > ?
           ORDER BY event_id ASC
           LIMIT ?`,
        )
        .bind(after, limit)
        .all<RegistryEventRow>();

  return (rows.results ?? []).map(parseRegistryEventRecord);
}

export async function listPackageRegistryEvents(
  db: D1Database,
  packageName: string,
  packageVersion?: string,
  limit = 50,
): Promise<RegistryEventRecord[]> {
  if (packageVersion === undefined) {
    const rows = await db
      .prepare(
        `SELECT
           event_id,
           event_type,
           package_name,
           package_version,
           package_locator,
           payload_json,
           created_at
        FROM registry_events
        WHERE package_name = ?
        ORDER BY event_id DESC
        LIMIT ?`,
      )
      .bind(packageName, limit)
      .all<RegistryEventRow>();

    return (rows.results ?? []).map(parseRegistryEventRecord);
  }

  const rows = await db
    .prepare(
      `SELECT
         event_id,
         event_type,
         package_name,
         package_version,
         package_locator,
         payload_json,
         created_at
       FROM registry_events
       WHERE package_name = ? AND package_version = ?
       ORDER BY event_id DESC
       LIMIT ?`,
    )
    .bind(packageName, packageVersion, limit)
    .all<RegistryEventRow>();

  return (rows.results ?? []).map(parseRegistryEventRecord);
}

export async function readPackageOverviewDocument(
  db: D1Database,
  packageName: string,
): Promise<PackageOverviewDocument | null> {
  const rows = await db
    .prepare(
      `SELECT
         p.package_name,
         p.latest_version,
         p.description,
         p.license,
         p.homepage,
         p.repository,
         p.root_module,
         p.repo_url,
         p.repo_owner,
         p.release_count,
         p.updated_at,
         pr.package_locator,
         pr.package_subdir,
         pr.resolved_sha,
         pr.package_description,
         pr.package_license,
         pr.package_homepage,
         pr.package_repository,
         pr.package_root_module,
         pr.package_categories_json,
         pr.package_keywords_json,
         pr.dependencies_json,
         pr.source_archive_key,
         pr.manifest_key,
         pr.published_at
       FROM packages p
       JOIN published_releases pr
         ON pr.package_name = p.package_name
        AND pr.package_version = p.latest_version
       WHERE p.package_name = ?`,
    )
    .bind(packageName)
    .all<PackageOverviewRow>();

  const row = rows.results?.[0];
  if (row === undefined) {
    return null;
  }

  const claim = await readPackageClaim(db, packageName);
  const ownerGithubLogin = claim?.owner_github_login ?? row.repo_owner;
  const dependentMap = await readPackageDependentMap(db);

  return {
    schema_version: 1,
    package_name: row.package_name,
    latest_version: row.latest_version,
    updated_at: row.updated_at,
    published_at: row.published_at,
    description: row.package_description ?? row.description ?? undefined,
    license: row.package_license ?? row.license ?? undefined,
    homepage: row.package_homepage ?? row.homepage ?? undefined,
    repository: row.package_repository ?? row.repository ?? undefined,
    root_module: row.package_root_module ?? row.root_module ?? undefined,
    canonical_locator: row.package_locator,
    repo_url: row.repo_url,
    subdir: row.package_subdir,
    source_key: row.source_archive_key,
    manifest_key: row.manifest_key,
    sha: row.resolved_sha,
    owner_github_login: ownerGithubLogin,
    owner_github_avatar_url: await resolveOwnerAvatarUrl(
      db,
      {
        owner_user_id: claim?.owner_user_id,
        owner_github_login: claim?.owner_github_login,
      },
      ownerGithubLogin,
    ),
    release_count: row.release_count,
    dependency_count: normalizeDependencies(row.dependencies_json).length,
    dependent_count: (dependentMap.get(packageName) ?? []).length,
    categories: parseJsonArray<string>(row.package_categories_json),
    keywords: parseJsonArray<string>(row.package_keywords_json),
  };
}

export async function readPackageRelationsDocument(
  db: D1Database,
  packageName: string,
): Promise<PackageRelationsDocument | null> {
  const rows = await db
    .prepare(
      `SELECT
         p.package_name,
         p.updated_at,
         pr.dependencies_json
       FROM packages p
       JOIN published_releases pr
         ON pr.package_name = p.package_name
        AND pr.package_version = p.latest_version
       WHERE p.package_name = ?`,
    )
    .bind(packageName)
    .all<PackageRelationsRow>();

  const row = rows.results?.[0];
  if (row === undefined) {
    return null;
  }

  const dependentMap = await readPackageDependentMap(db);
  return {
    schema_version: 1,
    package_name: row.package_name,
    updated_at: row.updated_at,
    dependencies: normalizeDependencies(row.dependencies_json),
    dependents: [...(dependentMap.get(packageName) ?? [])].sort((left, right) =>
      left.package_name.localeCompare(right.package_name)
    ),
  };
}

export async function readRecentPackagesDocument(
  db: D1Database,
): Promise<RecentPackagesDocument | null> {
  const summaries = await listPackageSummaryRows(db);
  if (summaries.length === 0) {
    return null;
  }

  const latestReleases = await readLatestReleaseRows(db);
  const avatarUrls = await resolveOwnerAvatarUrls(db, summaries);

  return {
    schema_version: 1,
    generated_at: new Date().toISOString(),
    packages: summaries
      .slice(0, 12)
      .map((summary) => toWebPackageListItem(summary, latestReleases, avatarUrls)),
  };
}

export async function readPopularPackagesDocument(
  db: D1Database,
): Promise<PopularPackagesDocument | null> {
  const summaries = await listPackageSummaryRows(db);
  if (summaries.length === 0) {
    return null;
  }

  const latestReleases = await readLatestReleaseRows(db);
  const avatarUrls = await resolveOwnerAvatarUrls(db, summaries);
  const dependentMap = await readPackageDependentMap(db);

  return {
    schema_version: 1,
    generated_at: new Date().toISOString(),
    packages: summaries
      .map((summary) => ({
        ...toWebPackageListItem(summary, latestReleases, avatarUrls),
        dependent_count: (dependentMap.get(summary.package_name) ?? []).length,
        release_count: summary.release_count,
      }))
      .sort((left, right) => {
        if (right.dependent_count !== left.dependent_count) {
          return right.dependent_count - left.dependent_count;
        }

        if (right.release_count !== left.release_count) {
          return right.release_count - left.release_count;
        }

        return right.updated_at.localeCompare(left.updated_at);
      })
      .slice(0, 12),
  };
}

export async function readCategoriesIndexDocument(
  db: D1Database,
): Promise<CategoriesIndexDocument | null> {
  const latestReleases = await readLatestReleaseRows(db);
  if (latestReleases.size === 0) {
    return null;
  }

  const packagesByCategory = new Map<string, Set<string>>();
  for (const [packageName, release] of latestReleases.entries()) {
    for (const category of release.categories) {
      const normalized = category.trim();
      if (normalized.length === 0) {
        continue;
      }

      const packageNames = packagesByCategory.get(normalized) ?? new Set<string>();
      packageNames.add(packageName);
      packagesByCategory.set(normalized, packageNames);
    }
  }

  const categories: CategorySummary[] = [...packagesByCategory.entries()]
    .map(([name, packageNames]) => ({
      name,
      slug: toSlug(name),
      package_count: packageNames.size,
      packages: [...packageNames].sort(),
    }))
    .sort((left, right) => {
      if (right.package_count !== left.package_count) {
        return right.package_count - left.package_count;
      }

      return left.name.localeCompare(right.name);
    });

  return {
    schema_version: 1,
    generated_at: new Date().toISOString(),
    categories,
  };
}

export async function readOwnerPackagesDocument(
  db: D1Database,
  ownerGithubLogin: string,
): Promise<OwnerPackagesDocument | null> {
  const rows = await db
    .prepare(
      `SELECT
         p.package_name,
         p.latest_version,
         p.description,
         p.license,
         p.repository,
         p.subdir,
         p.release_count,
         p.updated_at,
         p.repo_url,
         p.repo_owner,
         c.owner_user_id,
         c.owner_github_login
       FROM package_claims c
       JOIN packages p
         ON p.package_name = c.package_name
       WHERE lower(c.owner_github_login) = lower(?)
       ORDER BY p.updated_at DESC`,
    )
    .bind(ownerGithubLogin)
    .all<OwnerPackageRow>();

  const ownerRows = rows.results ?? [];
  if (ownerRows.length === 0) {
    return null;
  }

  const latestReleases = await readLatestReleaseRows(db);
  const avatarUrls = await resolveOwnerAvatarUrls(db, ownerRows);
  const normalizedLogin = ownerGithubLogin.toLowerCase();

  const packages = ownerRows.map((row) => toWebPackageListItem(row, latestReleases, avatarUrls));
  return {
    schema_version: 1,
    generated_at: new Date().toISOString(),
    owner_github_login: ownerRows[0]?.owner_github_login ?? ownerGithubLogin,
    owner_github_avatar_url: avatarUrls.get(normalizedLogin),
    package_count: packages.length,
    latest_update_at: packages[0]?.updated_at,
    packages,
  };
}

async function listPackageSummaryRows(db: D1Database): Promise<PackageSummaryRow[]> {
  const rows = await db
    .prepare(
      `SELECT
         p.package_name,
         p.latest_version,
         p.description,
         p.license,
         p.repository,
         p.subdir,
         p.release_count,
         p.updated_at,
         p.repo_url,
         p.repo_owner,
         c.owner_user_id,
         c.owner_github_login
       FROM packages p
       LEFT JOIN package_claims c
         ON c.package_name = p.package_name
       ORDER BY p.updated_at DESC, p.package_name ASC`,
    )
    .all<PackageSummaryRow>();

  return rows.results ?? [];
}

async function readLatestReleaseRows(
  db: D1Database,
): Promise<Map<string, LatestReleaseMetadata>> {
  const rows = await db
    .prepare(
      `SELECT
         p.package_name,
         pr.package_categories_json,
         pr.package_keywords_json,
         pr.dependencies_json
       FROM packages p
       JOIN published_releases pr
         ON pr.package_name = p.package_name
        AND pr.package_version = p.latest_version`,
    )
    .all<LatestReleaseRow>();

  const latestReleases = new Map<string, LatestReleaseMetadata>();
  for (const row of rows.results ?? []) {
    latestReleases.set(row.package_name, {
      categories: parseJsonArray<string>(row.package_categories_json),
      keywords: parseJsonArray<string>(row.package_keywords_json),
      dependencies: normalizeDependencies(row.dependencies_json),
    });
  }

  return latestReleases;
}

async function readPackageDependentMap(
  db: D1Database,
): Promise<Map<string, PackageRelationDependent[]>> {
  const rows = await db
    .prepare(
      `SELECT
         p.package_name,
         p.latest_version,
         pr.dependencies_json
       FROM packages p
       JOIN published_releases pr
         ON pr.package_name = p.package_name
        AND pr.package_version = p.latest_version`,
    )
    .all<DependentSourceRow>();

  const dependents = new Map<string, PackageRelationDependent[]>();
  for (const row of rows.results ?? []) {
    for (const dependency of normalizeDependencies(row.dependencies_json)) {
      const existing = dependents.get(dependency.package_name) ?? [];
      existing.push({
        package_name: row.package_name,
        latest_version: row.latest_version,
        requirement: dependency.requirement,
      });
      dependents.set(dependency.package_name, existing);
    }
  }

  return dependents;
}

async function resolveOwnerAvatarUrls(
  db: D1Database,
  rows: Array<{
    repo_owner: string;
    owner_user_id?: string | null;
    owner_github_login?: string | null;
  }>,
): Promise<Map<string, string | undefined>> {
  const avatarUrls = new Map<string, string | undefined>();

  for (const row of rows) {
    const ownerGithubLogin = row.owner_github_login ?? row.repo_owner;
    const normalizedLogin = ownerGithubLogin.toLowerCase();
    if (avatarUrls.has(normalizedLogin)) {
      continue;
    }

    avatarUrls.set(
      normalizedLogin,
      await resolveOwnerAvatarUrl(
        db,
        {
          owner_user_id: row.owner_user_id,
          owner_github_login: row.owner_github_login,
        },
        ownerGithubLogin,
      ),
    );
  }

  return avatarUrls;
}

async function resolveOwnerAvatarUrl(
  db: D1Database,
  owner: {
    owner_user_id?: string | null;
    owner_github_login?: string | null;
  },
  fallbackGithubLogin: string,
): Promise<string | undefined> {
  if (owner.owner_user_id !== undefined && owner.owner_user_id !== null) {
    const user = await readUserRecord(db, owner.owner_user_id);
    if (user?.github_avatar_url !== undefined) {
      return user.github_avatar_url;
    }
  }

  const githubLogin = owner.owner_github_login ?? fallbackGithubLogin;
  const loginRecord = await readUserLoginRecord(db, githubLogin);
  if (loginRecord === null) {
    return undefined;
  }

  const user = await readUserRecord(db, loginRecord.user_id);
  return user?.github_avatar_url;
}

function toWebPackageListItem(
  row: {
    package_name: string;
    latest_version: string;
    description?: string | null;
    license?: string | null;
    repository?: string | null;
    subdir: string;
    release_count: number;
    updated_at: string;
    repo_url: string;
    repo_owner: string;
    owner_github_login?: string | null;
  },
  latestReleases: Map<string, LatestReleaseMetadata>,
  avatarUrls: Map<string, string | undefined>,
): WebPackageListItem {
  const ownerGithubLogin = row.owner_github_login ?? row.repo_owner;
  const normalizedOwner = ownerGithubLogin.toLowerCase();

  return {
    package_name: row.package_name,
    latest_version: row.latest_version,
    description: row.description ?? undefined,
    license: row.license ?? undefined,
    owner_github_login: ownerGithubLogin,
    owner_github_avatar_url: avatarUrls.get(normalizedOwner),
    categories: latestReleases.get(row.package_name)?.categories ?? [],
    updated_at: row.updated_at,
    repo_url: row.repo_url,
    repository: row.repository ?? undefined,
    subdir: row.subdir,
    release_count: row.release_count,
    package_path: `/p/${row.package_name}`,
  };
}

function normalizeDependencies(dependenciesJson: string): PackageRelationDependency[] {
  return parseJsonArray<Record<string, unknown>>(dependenciesJson)
    .map((dependency) => {
      const packageName =
        typeof dependency.package === "string"
          ? dependency.package
          : typeof dependency.name === "string"
            ? dependency.name
            : null;

      if (packageName === null || packageName.length === 0) {
        return null;
      }

      const requirement =
        typeof dependency.version === "string"
          ? dependency.version
          : typeof dependency.requirement === "string"
            ? dependency.requirement
            : typeof dependency.raw === "string"
              ? dependency.raw
              : "unspecified";

      return {
        package_name: packageName,
        requirement,
      };
    })
    .filter((dependency): dependency is PackageRelationDependency => dependency !== null);
}

function toSlug(value: string): string {
  return value
    .toLowerCase()
    .trim()
    .replace(/[^a-z0-9]+/g, "-")
    .replace(/^-+|-+$/g, "");
}

interface PackageOverviewRow {
  package_name: string;
  latest_version: string;
  description?: string | null;
  license?: string | null;
  homepage?: string | null;
  repository?: string | null;
  root_module?: string | null;
  repo_url: string;
  repo_owner: string;
  release_count: number;
  updated_at: string;
  package_locator: string;
  package_subdir: string;
  resolved_sha: string;
  package_description?: string | null;
  package_license?: string | null;
  package_homepage?: string | null;
  package_repository?: string | null;
  package_root_module?: string | null;
  package_categories_json: string;
  package_keywords_json: string;
  dependencies_json: string;
  source_archive_key: string;
  manifest_key: string;
  published_at: string;
}

interface PackageRelationsRow {
  package_name: string;
  updated_at: string;
  dependencies_json: string;
}

interface PackageSummaryRow {
  package_name: string;
  latest_version: string;
  description?: string | null;
  license?: string | null;
  repository?: string | null;
  subdir: string;
  release_count: number;
  updated_at: string;
  repo_url: string;
  repo_owner: string;
  owner_user_id?: string | null;
  owner_github_login?: string | null;
}

interface OwnerPackageRow extends PackageSummaryRow {}

interface LatestReleaseRow {
  package_name: string;
  package_categories_json: string;
  package_keywords_json: string;
  dependencies_json: string;
}

interface LatestReleaseMetadata {
  categories: string[];
  keywords: string[];
  dependencies: PackageRelationDependency[];
}

interface DependentSourceRow {
  package_name: string;
  latest_version: string;
  dependencies_json: string;
}

interface ApiTokenRow {
  token_id: string;
  user_id: string;
  github_login: string;
  name: string;
  secret_hash: string;
  capabilities_json: string;
  created_at: string;
  last_used_at?: string | null;
  revoked_at?: string | null;
}

interface UserRow {
  user_id: string;
  github_id: number;
  github_login: string;
  github_name?: string | null;
  github_avatar_url?: string | null;
  github_email?: string | null;
  github_email_verified?: number | boolean | null;
  created_at: string;
  updated_at: string;
}

interface PackageClaimRow {
  package_name: string;
  package_locator: string;
  source_url: string;
  package_subdir: string;
  owner_user_id?: string | null;
  owner_github_login?: string | null;
  claimed_at: string;
  updated_at: string;
}

interface ApiTokenLookupRow {
  token_id: string;
  user_id: string;
  github_login: string;
  capabilities_json: string;
  revoked_at?: string | null;
}

interface SelectorResolutionRow {
  package_locator: string;
  selector: string;
  resolved_sha: string;
  frozen: number;
  recorded_at: string;
}

interface PublishedReleaseRow {
  package_name: string;
  package_version: string;
  package_locator: string;
  source_url: string;
  package_subdir: string;
  selector: string;
  resolved_sha: string;
  package_description?: string | null;
  package_license?: string | null;
  package_homepage?: string | null;
  package_repository?: string | null;
  package_root_module?: string | null;
  package_categories_json: string;
  package_keywords_json: string;
  dependencies_json: string;
  source_archive_key: string;
  manifest_key: string;
  published_at: string;
}

interface RequestLogRow {
  request_id: string;
  request_timestamp: string;
  method: string;
  path: string;
  route: string;
  package_locator?: string | null;
  selector?: string | null;
  resolved_sha?: string | null;
  status: number;
  success: number | boolean;
  error_category?: string | null;
  error_message?: string | null;
  user_agent?: string | null;
}

interface RegistryEventRow {
  event_id: string;
  event_type: RegistryEventRecord["event_type"];
  package_name?: string | null;
  package_version?: string | null;
  package_locator?: string | null;
  payload_json: string;
  created_at: string;
}

function parseRequestLogRecord(row: RequestLogRow): RequestLogEntry {
  return {
    request_id: row.request_id,
    request_timestamp: row.request_timestamp,
    method: row.method,
    path: row.path,
    route: row.route,
    package_locator: row.package_locator ?? undefined,
    selector: row.selector ?? undefined,
    resolved_sha: row.resolved_sha ?? undefined,
    status: row.status,
    success: row.success === true || row.success === 1,
    error_category: row.error_category ?? undefined,
    error_message: row.error_message ?? undefined,
    user_agent: row.user_agent ?? null,
  };
}

function parseApiTokenRecord(row: ApiTokenRow): ApiTokenRecord {
  return {
    token_id: row.token_id,
    user_id: row.user_id,
    github_login: row.github_login,
    name: row.name,
    secret_hash: row.secret_hash,
    capabilities: parseJsonArray<ApiTokenCapability>(row.capabilities_json),
    created_at: row.created_at,
    last_used_at: row.last_used_at ?? undefined,
    revoked_at: row.revoked_at ?? undefined,
  };
}

function parseUserRecord(row: UserRow): UserRecord {
  return {
    user_id: row.user_id,
    github_id: row.github_id,
    github_login: row.github_login,
    github_name: row.github_name ?? undefined,
    github_avatar_url: row.github_avatar_url ?? undefined,
    github_email: row.github_email ?? undefined,
    github_email_verified: row.github_email_verified === 1 || row.github_email_verified === true,
    created_at: row.created_at,
    updated_at: row.updated_at,
  };
}

function parsePackageClaimRecord(row: PackageClaimRow): PackageClaimRecord {
  return {
    package_name: row.package_name,
    package_locator: row.package_locator,
    source_url: row.source_url,
    package_subdir: row.package_subdir,
    owner_user_id: row.owner_user_id ?? undefined,
    owner_github_login: row.owner_github_login ?? undefined,
    claimed_at: row.claimed_at,
    updated_at: row.updated_at,
  };
}

function parseApiTokenLookupRecord(row: ApiTokenLookupRow): ApiTokenLookupRecord {
  return {
    token_id: row.token_id,
    user_id: row.user_id,
    github_login: row.github_login,
    capabilities: parseJsonArray<ApiTokenCapability>(row.capabilities_json),
    revoked_at: row.revoked_at ?? undefined,
  };
}

function parseSelectorResolution(row: SelectorResolutionRow): SelectorResolutionRecord {
  return {
    package_locator: row.package_locator,
    selector: row.selector,
    resolved_sha: row.resolved_sha,
    frozen: row.frozen === 1,
    recorded_at: row.recorded_at,
  };
}

function parsePublishedReleaseRecord(row: PublishedReleaseRow): PublishedReleaseRecord {
  return {
    package_name: row.package_name,
    package_version: row.package_version,
    package_locator: row.package_locator,
    source_url: row.source_url,
    package_subdir: row.package_subdir,
    selector: row.selector,
    resolved_sha: row.resolved_sha,
    package_description: row.package_description ?? undefined,
    package_license: row.package_license ?? undefined,
    package_homepage: row.package_homepage ?? undefined,
    package_repository: row.package_repository ?? undefined,
    package_root_module: row.package_root_module ?? undefined,
    package_categories: parseJsonArray(row.package_categories_json),
    package_keywords: parseJsonArray(row.package_keywords_json),
    dependencies: parseJsonArray<Record<string, unknown>>(row.dependencies_json),
    source_archive_key: row.source_archive_key,
    manifest_key: row.manifest_key,
    published_at: row.published_at,
  };
}

function parseRegistryEventRecord(row: RegistryEventRow): RegistryEventRecord {
  return {
    event_id: row.event_id,
    event_type: row.event_type,
    package_name: row.package_name ?? undefined,
    package_version: row.package_version ?? undefined,
    package_locator: row.package_locator ?? undefined,
    payload: parseJsonObject(row.payload_json),
    created_at: row.created_at,
  };
}

function parseJsonArray<T = string>(value: string): T[] {
  try {
    const parsed = JSON.parse(value) as unknown;
    return Array.isArray(parsed) ? (parsed as T[]) : [];
  } catch {
    return [];
  }
}

function parseJsonObject(value: string): Record<string, unknown> {
  try {
    const parsed = JSON.parse(value) as unknown;
    return parsed !== null && typeof parsed === "object" && !Array.isArray(parsed)
      ? (parsed as Record<string, unknown>)
      : {};
  } catch {
    return {};
  }
}
