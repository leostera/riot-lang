import type {
  IndexedPackageRelease,
  PackageIndexDocument,
  PackagePublicationManifest,
  PublishedReleaseRecord,
} from "./types.ts";

export function buildIndexedRelease(
  release: PublishedReleaseRecord,
  manifest: PackagePublicationManifest,
): IndexedPackageRelease {
  assertMatchingManifest(release, manifest);

  return {
    version: release.package_version,
    published_at: release.published_at,
    canonical_locator: release.package_locator,
    repo_url: release.source_url,
    subdir: release.package_subdir,
    sha: release.resolved_sha,
    description: release.package_description,
    license: release.package_license,
    homepage: release.package_homepage,
    repository: release.package_repository,
    root_module: release.package_root_module,
    categories: release.package_categories,
    keywords: release.package_keywords,
    manifest_key: release.manifest_key,
    source_key: release.source_archive_key,
    dependencies: release.dependencies,
  };
}

export function upsertPackageDocument(args: {
  existing: PackageIndexDocument | null;
  packageName: string;
  release: IndexedPackageRelease;
  updatedAt: string;
}): {
  document: PackageIndexDocument;
  changed: boolean;
} {
  const existing = args.existing;
  if (existing === null) {
    return {
      changed: true,
      document: {
        schema_version: 1,
        name: args.packageName,
        latest: args.release.version,
        updated_at: args.updatedAt,
        releases: [args.release],
      },
    };
  }

  if (existing.name !== args.packageName) {
    throw new Error(
      `Package document ${existing.name} does not match published package ${args.packageName}.`,
    );
  }

  const releases = [...existing.releases];
  const index = releases.findIndex((candidate) => candidate.version === args.release.version);
  let changed = false;

  if (index === -1) {
    releases.push(args.release);
    changed = true;
  } else {
    const existingRelease = releases[index];
    if (existingRelease === undefined) {
      throw new Error(
        `Package document ${args.packageName} lost release ${args.release.version} during upsert.`,
      );
    }

    if (!isSameIndexedRelease(existingRelease, args.release)) {
      releases[index] = args.release;
      changed = true;
    }
  }

  releases.sort((left, right) => compareSemverDescending(left.version, right.version));
  const latest = releases[0]?.version;
  if (latest === undefined) {
    throw new Error(`Package document ${args.packageName} has no releases after upsert.`);
  }

  if (!changed && existing.latest === latest) {
    return {
      changed: false,
      document: existing,
    };
  }

  return {
    changed: true,
    document: {
      schema_version: 1,
      name: args.packageName,
      latest,
      updated_at: args.updatedAt,
      releases,
    },
  };
}

function assertMatchingManifest(
  release: PublishedReleaseRecord,
  manifest: PackagePublicationManifest,
): void {
  if (!manifest.package_public) {
    throw new Error(`Published manifest ${manifest.manifest_key} is not public.`);
  }

  if (!isValidSemver(manifest.package_version)) {
    throw new Error(`Published release ${release.package_name}@${release.package_version} is not semver.`);
  }

  if (
    release.package_name !== manifest.package_name ||
    release.package_version !== manifest.package_version ||
    release.package_locator !== manifest.package_locator ||
    release.source_url !== manifest.source_url ||
    release.package_subdir !== manifest.package_subdir ||
    release.resolved_sha !== manifest.resolved_sha ||
    release.package_description !== manifest.package_description ||
    release.package_license !== manifest.package_license ||
    release.package_homepage !== manifest.package_homepage ||
    release.package_repository !== manifest.package_repository ||
    release.package_root_module !== manifest.package_root_module ||
    JSON.stringify(release.package_categories ?? []) !== JSON.stringify(manifest.package_categories ?? []) ||
    JSON.stringify(release.package_keywords ?? []) !== JSON.stringify(manifest.package_keywords ?? []) ||
    release.manifest_key !== manifest.manifest_key ||
    release.source_archive_key !== manifest.source_archive_key
  ) {
    throw new Error(
      `Published release ${release.package_name}@${release.package_version} does not match its source manifest.`,
    );
  }
}

function isSameIndexedRelease(left: IndexedPackageRelease, right: IndexedPackageRelease): boolean {
  return (
    left.version === right.version &&
    left.published_at === right.published_at &&
    left.canonical_locator === right.canonical_locator &&
    left.repo_url === right.repo_url &&
    left.subdir === right.subdir &&
    left.sha === right.sha &&
    left.description === right.description &&
    left.license === right.license &&
    left.homepage === right.homepage &&
    left.repository === right.repository &&
    left.root_module === right.root_module &&
    JSON.stringify(left.categories ?? []) === JSON.stringify(right.categories ?? []) &&
    JSON.stringify(left.keywords ?? []) === JSON.stringify(right.keywords ?? []) &&
    left.manifest_key === right.manifest_key &&
    left.source_key === right.source_key &&
    JSON.stringify(left.dependencies) === JSON.stringify(right.dependencies)
  );
}

function compareSemverDescending(left: string, right: string): number {
  const leftParsed = parseSemver(left);
  const rightParsed = parseSemver(right);

  if (leftParsed.major !== rightParsed.major) {
    return rightParsed.major - leftParsed.major;
  }

  if (leftParsed.minor !== rightParsed.minor) {
    return rightParsed.minor - leftParsed.minor;
  }

  if (leftParsed.patch !== rightParsed.patch) {
    return rightParsed.patch - leftParsed.patch;
  }

  return comparePrereleaseDescending(leftParsed.prerelease, rightParsed.prerelease);
}

function isValidSemver(value: string): boolean {
  return /^(0|[1-9]\d*)\.(0|[1-9]\d*)\.(0|[1-9]\d*)(?:-[0-9A-Za-z.-]+)?(?:\+[0-9A-Za-z.-]+)?$/.test(
    value,
  );
}

function parseSemver(value: string): {
  major: number;
  minor: number;
  patch: number;
  prerelease: string[];
} {
  const match =
    value.match(
      /^(0|[1-9]\d*)\.(0|[1-9]\d*)\.(0|[1-9]\d*)(?:-([0-9A-Za-z.-]+))?(?:\+[0-9A-Za-z.-]+)?$/,
    );
  if (match === null) {
    throw new Error(`Invalid semver ${value}.`);
  }

  return {
    major: Number(match[1]),
    minor: Number(match[2]),
    patch: Number(match[3]),
    prerelease: match[4]?.split(".") ?? [],
  };
}

function comparePrereleaseDescending(left: string[], right: string[]): number {
  if (left.length === 0 && right.length === 0) {
    return 0;
  }

  if (left.length === 0) {
    return -1;
  }

  if (right.length === 0) {
    return 1;
  }

  const length = Math.max(left.length, right.length);
  for (let index = 0; index < length; index += 1) {
    const leftIdentifier = left[index];
    const rightIdentifier = right[index];

    if (leftIdentifier === undefined) {
      return 1;
    }

    if (rightIdentifier === undefined) {
      return -1;
    }

    const comparison = comparePrereleaseIdentifier(leftIdentifier, rightIdentifier);
    if (comparison !== 0) {
      return comparison;
    }
  }

  return 0;
}

function comparePrereleaseIdentifier(left: string, right: string): number {
  const leftNumeric = /^\d+$/.test(left);
  const rightNumeric = /^\d+$/.test(right);

  if (leftNumeric && rightNumeric) {
    return Number(right) - Number(left);
  }

  if (leftNumeric) {
    return -1;
  }

  if (rightNumeric) {
    return 1;
  }

  return right.localeCompare(left);
}
