import { buildFtsQuery, rankQueryResult } from "./search-document.ts";
import type { SearchPackageRow, SearchResult } from "./types.ts";

export async function applySearchMigrations(db: D1Database): Promise<void> {
  await db.exec(
    "CREATE TABLE IF NOT EXISTS packages (" +
      "package_name TEXT PRIMARY KEY, " +
      "normalized_name TEXT NOT NULL, " +
      "latest_version TEXT NOT NULL, " +
      "description TEXT, " +
      "license TEXT, " +
      "homepage TEXT, " +
      "repository TEXT, " +
      "root_module TEXT, " +
      "canonical_locator TEXT NOT NULL, " +
      "repo_url TEXT NOT NULL, " +
      "repo_owner TEXT NOT NULL, " +
      "repo_name TEXT NOT NULL, " +
      "subdir TEXT NOT NULL, " +
      "release_count INTEGER NOT NULL, " +
      "updated_at TEXT NOT NULL" +
      ")",
  );

  await db.exec(
    "CREATE VIRTUAL TABLE IF NOT EXISTS package_search USING fts5 (" +
      "package_name, " +
      "description, " +
      "repo_owner, " +
      "repo_name, " +
      "subdir, " +
      "repository, " +
      "tokenize = 'unicode61 remove_diacritics 2'" +
      ")",
  );
}

export async function upsertSearchRow(db: D1Database, row: SearchPackageRow): Promise<void> {
  await db.batch([
    db
      .prepare(
        `INSERT INTO packages (
          package_name,
          normalized_name,
          latest_version,
          description,
          license,
          homepage,
          repository,
          root_module,
          canonical_locator,
          repo_url,
          repo_owner,
          repo_name,
          subdir,
          release_count,
          updated_at
        ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
        ON CONFLICT(package_name) DO UPDATE SET
          normalized_name = excluded.normalized_name,
          latest_version = excluded.latest_version,
          description = excluded.description,
          license = excluded.license,
          homepage = excluded.homepage,
          repository = excluded.repository,
          root_module = excluded.root_module,
          canonical_locator = excluded.canonical_locator,
          repo_url = excluded.repo_url,
          repo_owner = excluded.repo_owner,
          repo_name = excluded.repo_name,
          subdir = excluded.subdir,
          release_count = excluded.release_count,
          updated_at = excluded.updated_at`
      )
      .bind(
        row.package_name,
        row.normalized_name,
        row.latest_version,
        row.description,
        row.license,
        row.homepage,
        row.repository,
        row.root_module,
        row.canonical_locator,
        row.repo_url,
        row.repo_owner,
        row.repo_name,
        row.subdir,
        row.release_count,
        row.updated_at,
      ),
    db.prepare(`DELETE FROM package_search WHERE package_name = ?`).bind(row.package_name),
    db
      .prepare(
        `INSERT INTO package_search (
          package_name,
          description,
          repo_owner,
          repo_name,
          subdir,
          repository
        ) VALUES (?, ?, ?, ?, ?, ?)`
      )
      .bind(
        row.package_name,
        row.description ?? "",
        row.repo_owner,
        row.repo_name,
        row.subdir,
        row.repository ?? "",
      ),
  ]);
}

export async function searchPackages(
  db: D1Database,
  query: string,
  limit: number,
  offset: number,
): Promise<SearchResult[]> {
  const normalizedQuery = query.trim();
  if (normalizedQuery.length === 0) {
    return [];
  }

  const ftsQuery = buildFtsQuery(normalizedQuery);
  const results = new Map<string, SearchResult & { fts_rank: number | null }>();

  if (ftsQuery.length > 0) {
    const ftsRows = await db
      .prepare(
        `SELECT
           p.package_name,
           p.normalized_name,
           p.latest_version,
           p.description,
           p.license,
           p.homepage,
           p.repository,
           p.root_module,
           p.canonical_locator,
           p.repo_url,
           p.repo_owner,
           p.repo_name,
           p.subdir,
           p.release_count,
           p.updated_at,
           bm25(package_search) AS fts_rank
         FROM package_search
         JOIN packages p ON p.package_name = package_search.package_name
         WHERE package_search MATCH ?`
      )
      .bind(ftsQuery)
      .all<SearchResult & { fts_rank: number | null }>();

    for (const row of ftsRows.results ?? []) {
      results.set(row.package_name, row);
    }
  }

  const directRows = await db
    .prepare(
      `SELECT
         package_name,
         normalized_name,
         latest_version,
         description,
         license,
         homepage,
         repository,
         root_module,
         canonical_locator,
         repo_url,
         repo_owner,
         repo_name,
         subdir,
         release_count,
         updated_at,
         NULL AS fts_rank
       FROM packages
       WHERE normalized_name = ?
          OR normalized_name LIKE ?
          OR normalized_name LIKE ?
          OR lower(repo_owner) = lower(?)
          OR lower(repo_name) = lower(?)`
    )
    .bind(
      normalizeName(normalizedQuery),
      `${normalizeName(normalizedQuery)}%`,
      `%${normalizeName(normalizedQuery)}%`,
      normalizedQuery,
      normalizedQuery,
    )
    .all<SearchResult & { fts_rank: number | null }>();

  for (const row of directRows.results ?? []) {
    results.set(row.package_name, row);
  }

  return [...results.values()]
    .sort((left, right) => {
      const leftScore = rankQueryResult(left, normalizedQuery, left.fts_rank);
      const rightScore = rankQueryResult(right, normalizedQuery, right.fts_rank);

      if (leftScore !== rightScore) {
        return rightScore - leftScore;
      }

      return left.package_name.localeCompare(right.package_name);
    })
    .slice(offset, offset + limit)
    .map(({ fts_rank: _ftsRank, ...row }) => row);
}

function normalizeName(value: string): string {
  return value.toLowerCase().replace(/[\s_-]+/g, "");
}
