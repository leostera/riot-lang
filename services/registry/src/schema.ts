import { index, integer, primaryKey, sqliteTable, text, uniqueIndex } from "drizzle-orm/sqlite-core";

export const users = sqliteTable("users", {
  userId: text("user_id").primaryKey(),
  githubId: integer("github_id").notNull().unique(),
  githubLogin: text("github_login").notNull().unique(),
  githubLoginLower: text("github_login_lower").notNull().unique(),
  githubName: text("github_name"),
  githubAvatarUrl: text("github_avatar_url"),
  githubEmail: text("github_email"),
  githubEmailVerified: integer("github_email_verified", { mode: "boolean" }).notNull().default(false),
  createdAt: text("created_at").notNull(),
  updatedAt: text("updated_at").notNull(),
});

export const userLogins = sqliteTable("user_logins", {
  githubLoginLower: text("github_login_lower").primaryKey(),
  githubLogin: text("github_login").notNull(),
  userId: text("user_id").notNull(),
  updatedAt: text("updated_at").notNull(),
});

export const oauthStates = sqliteTable("oauth_states", {
  stateId: text("state_id").primaryKey(),
  returnTo: text("return_to").notNull(),
  createdAt: text("created_at").notNull(),
});

export const sessions = sqliteTable(
  "sessions",
  {
    sessionId: text("session_id").primaryKey(),
    userId: text("user_id").notNull(),
    githubLogin: text("github_login").notNull(),
    createdAt: text("created_at").notNull(),
    expiresAt: text("expires_at").notNull(),
  },
  (table) => ({
    userIdIdx: index("idx_sessions_user_id").on(table.userId),
  }),
);

export const apiTokens = sqliteTable(
  "api_tokens",
  {
    tokenId: text("token_id").primaryKey(),
    userId: text("user_id").notNull(),
    githubLogin: text("github_login").notNull(),
    name: text("name").notNull(),
    secretHash: text("secret_hash").notNull().unique(),
    capabilitiesJson: text("capabilities_json").notNull(),
    createdAt: text("created_at").notNull(),
    lastUsedAt: text("last_used_at"),
    revokedAt: text("revoked_at"),
  },
  (table) => ({
    userIdIdx: index("idx_api_tokens_user_id").on(table.userId),
    secretHashIdx: index("idx_api_tokens_secret_hash").on(table.secretHash),
  }),
);

export const apiTokenLookups = sqliteTable("api_token_lookups", {
  secretHash: text("secret_hash").primaryKey(),
  tokenId: text("token_id").notNull(),
  userId: text("user_id").notNull(),
  githubLogin: text("github_login").notNull(),
  capabilitiesJson: text("capabilities_json").notNull(),
  revokedAt: text("revoked_at"),
});

export const packageClaims = sqliteTable(
  "package_claims",
  {
    packageName: text("package_name").primaryKey(),
    packageLocator: text("package_locator").notNull(),
    sourceUrl: text("source_url").notNull(),
    packageSubdir: text("package_subdir").notNull(),
    ownerUserId: text("owner_user_id"),
    ownerGithubLogin: text("owner_github_login"),
    claimedAt: text("claimed_at").notNull(),
    updatedAt: text("updated_at").notNull(),
  },
  (table) => ({
    ownerLoginIdx: index("idx_claims_owner_login").on(table.ownerGithubLogin),
  }),
);

export const publishedReleases = sqliteTable(
  "published_releases",
  {
    packageName: text("package_name").notNull(),
    packageVersion: text("package_version").notNull(),
    packageLocator: text("package_locator").notNull(),
    sourceUrl: text("source_url").notNull(),
    packageSubdir: text("package_subdir").notNull(),
    artifactSha256: text("artifact_sha256").notNull(),
    packageDescription: text("package_description"),
    packageLicense: text("package_license"),
    packageHomepage: text("package_homepage"),
    packageRepository: text("package_repository"),
    packageRootModule: text("package_root_module"),
    packageCategoriesJson: text("package_categories_json").notNull(),
    packageKeywordsJson: text("package_keywords_json").notNull(),
    dependenciesJson: text("dependencies_json").notNull(),
    sourceArchiveKey: text("source_archive_key").notNull(),
    manifestKey: text("manifest_key").notNull(),
    publishedAt: text("published_at").notNull(),
  },
  (table) => ({
    pk: primaryKey({ columns: [table.packageName, table.packageVersion] }),
    packageNameIdx: index("idx_releases_package_name").on(table.packageName),
  }),
);

export const registryEvents = sqliteTable(
  "registry_events",
  {
    sequenceId: integer("sequence_id").primaryKey({ autoIncrement: true }),
    eventId: text("event_id").notNull(),
    eventType: text("event_type").notNull(),
    packageName: text("package_name"),
    packageVersion: text("package_version"),
    packageLocator: text("package_locator"),
    payloadJson: text("payload_json").notNull(),
    createdAt: text("created_at").notNull(),
  },
  (table) => ({
    eventIdUnique: uniqueIndex("registry_events_event_id_unique").on(table.eventId),
    sequenceIdx: index("idx_registry_events_sequence_id").on(table.sequenceId),
    createdAtIdx: index("idx_registry_events_created_at").on(table.createdAt),
    packageIdx: index("idx_registry_events_package").on(
      table.packageName,
      table.packageVersion,
      table.createdAt,
    ),
  }),
);

export const packages = sqliteTable("packages", {
  packageName: text("package_name").primaryKey(),
  normalizedName: text("normalized_name").notNull(),
  latestVersion: text("latest_version").notNull(),
  description: text("description"),
  license: text("license"),
  homepage: text("homepage"),
  repository: text("repository"),
  rootModule: text("root_module"),
  canonicalLocator: text("canonical_locator").notNull(),
  repoUrl: text("repo_url").notNull(),
  repoOwner: text("repo_owner").notNull(),
  repoName: text("repo_name").notNull(),
  subdir: text("subdir").notNull(),
  releaseCount: integer("release_count").notNull(),
  updatedAt: text("updated_at").notNull(),
});
