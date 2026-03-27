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
);

CREATE VIRTUAL TABLE IF NOT EXISTS package_search USING fts5(
  package_name,
  description,
  repo_owner,
  repo_name,
  subdir,
  repository,
  tokenize = 'unicode61 remove_diacritics 2'
);
