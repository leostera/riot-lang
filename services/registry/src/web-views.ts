import { getConfig } from "./config.ts";
import {
  listPackageIndexDocuments,
  readPackageClaim,
  readUserLoginRecord,
  readUserRecord,
  writeCategoriesIndexDocument,
  writeOwnerPackagesDocument,
  writePackageOverviewDocument,
  writePackageRelationsDocument,
  writePopularPackagesDocument,
  writeRecentPackagesDocument,
} from "./storage.ts";
import type {
  CategoriesIndexDocument,
  CategorySummary,
  Env,
  IndexedPackageRelease,
  OwnerPackagesDocument,
  PackageIndexDocument,
  PackageOverviewDocument,
  PackageRelationDependency,
  PackageRelationDependent,
  PackageRelationsDocument,
  PopularPackagesDocument,
  RecentPackagesDocument,
  WebPackageListItem,
} from "./types.ts";

export async function rebuildWebViews(env: Env): Promise<void> {
  const config = getConfig(env);
  const documents = await listPackageIndexDocuments(env.ML_PKGS_CDN, config);
  const latestEntries = await Promise.all(
    documents.map(async (document) => {
      const latestRelease = getLatestRelease(document);
      const claim = await readPackageClaim(env.ML_PKGS_CDN, document.name);
      const owner = claim?.owner_github_login ?? parseCanonicalLocator(latestRelease.canonical_locator).owner;
      const ownerAvatarUrl = await resolveOwnerAvatarUrl(env, claim, owner);

      return {
        document,
        latestRelease,
        owner,
        ownerAvatarUrl,
      };
    }),
  );

  const dependentsByPackage = new Map<string, PackageRelationDependent[]>();
  for (const { document, latestRelease } of latestEntries) {
    for (const dependency of normalizeDependencies(latestRelease.dependencies)) {
      const existing = dependentsByPackage.get(dependency.package_name) ?? [];
      existing.push({
        package_name: document.name,
        latest_version: latestRelease.version,
        requirement: dependency.requirement,
      });
      dependentsByPackage.set(dependency.package_name, existing);
    }
  }

  const overviews: PackageOverviewDocument[] = latestEntries.map(({ document, latestRelease, owner, ownerAvatarUrl }) => ({
    schema_version: 1,
    package_name: document.name,
    latest_version: latestRelease.version,
    updated_at: document.updated_at,
    published_at: latestRelease.published_at,
    description: latestRelease.description,
    license: latestRelease.license,
    homepage: latestRelease.homepage,
    repository: latestRelease.repository,
    root_module: latestRelease.root_module,
    canonical_locator: latestRelease.canonical_locator,
    repo_url: latestRelease.repo_url,
    subdir: latestRelease.subdir,
    source_key: latestRelease.source_key,
    manifest_key: latestRelease.manifest_key,
    sha: latestRelease.sha,
    owner_github_login: owner,
    owner_github_avatar_url: ownerAvatarUrl,
    release_count: document.releases.length,
    dependency_count: normalizeDependencies(latestRelease.dependencies).length,
    dependent_count: (dependentsByPackage.get(document.name) ?? []).length,
    categories: [...(latestRelease.categories ?? [])],
    keywords: [...(latestRelease.keywords ?? [])],
  }));

  const overviewByPackage = new Map(overviews.map((overview) => [overview.package_name, overview]));

  const relations: PackageRelationsDocument[] = latestEntries.map(({ document, latestRelease }) => ({
    schema_version: 1,
    package_name: document.name,
    updated_at: document.updated_at,
    dependencies: normalizeDependencies(latestRelease.dependencies),
    dependents: [...(dependentsByPackage.get(document.name) ?? [])].sort((left, right) =>
      left.package_name.localeCompare(right.package_name),
    ),
  }));

  const recent: RecentPackagesDocument = {
    schema_version: 1,
    generated_at: new Date().toISOString(),
    packages: [...overviews]
      .sort(compareOverviewByRecent)
      .slice(0, 12)
      .map(toWebPackageListItem),
  };

  const popular: PopularPackagesDocument = {
    schema_version: 1,
    generated_at: new Date().toISOString(),
    packages: [...overviews]
      .sort(compareOverviewByPopularity)
      .slice(0, 12)
      .map((overview) => ({
        ...toWebPackageListItem(overview),
        dependent_count: overview.dependent_count,
        release_count: overview.release_count,
      })),
  };

  const categories: CategoriesIndexDocument = {
    schema_version: 1,
    generated_at: new Date().toISOString(),
    categories: buildCategorySummaries(overviews),
  };
  const owners = buildOwnerDocuments(overviews);

  await Promise.all([
    ...overviews.map(async (overview) => {
      await writePackageOverviewDocument(env.ML_PKGS_CDN, config, overview);
      const relation = relations.find((candidate) => candidate.package_name === overview.package_name);
      if (relation === undefined) {
        throw new Error(`Missing relations for package ${overview.package_name}.`);
      }

      await writePackageRelationsDocument(env.ML_PKGS_CDN, config, relation);
    }),
    writeRecentPackagesDocument(env.ML_PKGS_CDN, config, recent),
    writePopularPackagesDocument(env.ML_PKGS_CDN, config, popular),
    writeCategoriesIndexDocument(env.ML_PKGS_CDN, config, categories),
    ...owners.map(async (owner) => {
      await writeOwnerPackagesDocument(env.ML_PKGS_CDN, config, owner);
    }),
  ]);
}

function getLatestRelease(document: PackageIndexDocument): IndexedPackageRelease {
  const release = document.releases.find((candidate) => candidate.version === document.latest);
  if (release === undefined) {
    throw new Error(`Package document ${document.name} is missing latest release ${document.latest}.`);
  }

  return release;
}

function normalizeDependencies(dependencies: Array<Record<string, unknown>>): PackageRelationDependency[] {
  return dependencies
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

function parseCanonicalLocator(locator: string): { owner: string } {
  const parts = locator.split("/");
  return {
    owner: parts[1] ?? "unknown",
  };
}

function compareOverviewByRecent(left: PackageOverviewDocument, right: PackageOverviewDocument): number {
  const rightTimestamp = Date.parse(right.updated_at);
  const leftTimestamp = Date.parse(left.updated_at);

  if (!Number.isNaN(rightTimestamp) && !Number.isNaN(leftTimestamp) && rightTimestamp !== leftTimestamp) {
    return rightTimestamp - leftTimestamp;
  }

  return left.package_name.localeCompare(right.package_name);
}

function compareOverviewByPopularity(left: PackageOverviewDocument, right: PackageOverviewDocument): number {
  if (right.dependent_count !== left.dependent_count) {
    return right.dependent_count - left.dependent_count;
  }

  if (right.release_count !== left.release_count) {
    return right.release_count - left.release_count;
  }

  return compareOverviewByRecent(left, right);
}

function toWebPackageListItem(overview: PackageOverviewDocument): WebPackageListItem {
  return {
    package_name: overview.package_name,
    latest_version: overview.latest_version,
    description: overview.description,
    license: overview.license,
    owner_github_login: overview.owner_github_login,
    owner_github_avatar_url: overview.owner_github_avatar_url,
    categories: overview.categories,
    updated_at: overview.updated_at,
    repo_url: overview.repo_url,
    repository: overview.repository,
    subdir: overview.subdir,
    release_count: overview.release_count,
    package_path: `/p/${overview.package_name}`,
  };
}

function buildCategorySummaries(overviews: PackageOverviewDocument[]): CategorySummary[] {
  const packagesByCategory = new Map<string, Set<string>>();

  for (const overview of overviews) {
    for (const category of overview.categories) {
      const normalized = category.trim();
      if (normalized.length === 0) {
        continue;
      }

      const existing = packagesByCategory.get(normalized) ?? new Set<string>();
      existing.add(overview.package_name);
      packagesByCategory.set(normalized, existing);
    }
  }

  return [...packagesByCategory.entries()]
    .map(([name, packages]) => ({
      name,
      slug: toSlug(name),
      package_count: packages.size,
      packages: [...packages].sort(),
    }))
    .sort((left, right) => {
      if (right.package_count !== left.package_count) {
        return right.package_count - left.package_count;
      }

      return left.name.localeCompare(right.name);
    });
}

function toSlug(value: string): string {
  return value
    .toLowerCase()
    .trim()
    .replace(/[^a-z0-9]+/g, "-")
    .replace(/^-+|-+$/g, "");
}

function buildOwnerDocuments(overviews: PackageOverviewDocument[]): OwnerPackagesDocument[] {
  const packagesByOwner = new Map<string, PackageOverviewDocument[]>();

  for (const overview of overviews) {
    const key = overview.owner_github_login.toLowerCase();
    const existing = packagesByOwner.get(key) ?? [];
    existing.push(overview);
    packagesByOwner.set(key, existing);
  }

  return [...packagesByOwner.entries()]
    .map(([ownerKey, ownerOverviews]) => {
      const packages = [...ownerOverviews].sort(compareOverviewByRecent).map(toWebPackageListItem);

      return {
        schema_version: 1,
        generated_at: new Date().toISOString(),
        owner_github_login: ownerOverviews[0]?.owner_github_login ?? ownerKey,
        owner_github_avatar_url: ownerOverviews[0]?.owner_github_avatar_url,
        package_count: packages.length,
        latest_update_at: packages[0]?.updated_at,
        packages,
      } satisfies OwnerPackagesDocument;
    })
    .sort((left, right) => left.owner_github_login.localeCompare(right.owner_github_login));
}

async function resolveOwnerAvatarUrl(
  env: Env,
  claim: { owner_user_id?: string; owner_github_login?: string } | null,
  fallbackGithubLogin: string,
): Promise<string | undefined> {
  if (claim?.owner_user_id !== undefined) {
    const user = await readUserRecord(env.ML_PKGS_CDN, claim.owner_user_id);
    if (user?.github_avatar_url !== undefined) {
      return user.github_avatar_url;
    }
  }

  const githubLogin = claim?.owner_github_login ?? fallbackGithubLogin;
  if (githubLogin.length > 0) {
    const loginRecord = await readUserLoginRecord(env.ML_PKGS_CDN, githubLogin);
    if (loginRecord !== null) {
      const user = await readUserRecord(env.ML_PKGS_CDN, loginRecord.user_id);
      if (user?.github_avatar_url !== undefined) {
        return user.github_avatar_url;
      }
    }
  }

  return undefined;
}
