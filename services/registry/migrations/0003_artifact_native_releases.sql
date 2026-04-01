CREATE TABLE published_releases_v2 (
  package_name TEXT NOT NULL,
  package_version TEXT NOT NULL,
  package_locator TEXT NOT NULL,
  source_url TEXT NOT NULL,
  package_subdir TEXT NOT NULL,
  artifact_sha256 TEXT NOT NULL,
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

INSERT INTO published_releases_v2 (
  package_name,
  package_version,
  package_locator,
  source_url,
  package_subdir,
  artifact_sha256,
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
)
SELECT
  package_name,
  package_version,
  package_locator,
  source_url,
  package_subdir,
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
FROM published_releases;

DROP TABLE published_releases;
ALTER TABLE published_releases_v2 RENAME TO published_releases;
CREATE INDEX IF NOT EXISTS idx_releases_package_name ON published_releases(package_name);

CREATE TABLE request_logs_v2 (
  request_id TEXT PRIMARY KEY,
  request_timestamp TEXT NOT NULL,
  method TEXT NOT NULL,
  path TEXT NOT NULL,
  route TEXT NOT NULL,
  package_locator TEXT,
  artifact_sha256 TEXT,
  status INTEGER NOT NULL,
  success INTEGER NOT NULL,
  error_category TEXT,
  error_message TEXT,
  user_agent TEXT
);

INSERT INTO request_logs_v2 (
  request_id,
  request_timestamp,
  method,
  path,
  route,
  package_locator,
  artifact_sha256,
  status,
  success,
  error_category,
  error_message,
  user_agent
)
SELECT
  request_id,
  request_timestamp,
  method,
  path,
  route,
  package_locator,
  resolved_sha,
  status,
  success,
  error_category,
  error_message,
  user_agent
FROM request_logs;

DROP TABLE request_logs;
ALTER TABLE request_logs_v2 RENAME TO request_logs;
CREATE INDEX IF NOT EXISTS idx_request_logs_timestamp ON request_logs(request_timestamp DESC);

DROP TABLE IF EXISTS selector_resolutions;
