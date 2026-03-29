import { parse as parseToml } from "smol-toml";

import { readRepoFileFromTarGz } from "./archive.ts";
import { HttpError } from "./errors.ts";
import { canonicalSourceUrl, packageSubdir } from "./locator.ts";
import { manifestKey, sourceArchiveKey } from "./storage.ts";
import type { PackageLocator, PackagePublicationManifest } from "./types.ts";

export async function buildPublicationManifest(args: {
  locator: PackageLocator;
  selector: string;
  resolvedSha: string;
  archiveBytes: Uint8Array<ArrayBuffer>;
  materializedAt: string;
}): Promise<PackagePublicationManifest> {
  const tuskTomlPath = toRepoRelativeTuskTomlPath(args.locator);
  const tuskToml = await readRepoFileFromTarGz(args.archiveBytes, tuskTomlPath);

  if (tuskToml === null) {
    throw new HttpError(
      404,
      "package_not_found",
      `No tusk.toml exists at package locator ${args.locator.normalized}.`,
    );
  }

  const parsed = parseTuskToml(tuskToml, args.locator.normalized);
  const packageSection = asRecord(parsed.package, "package", args.locator.normalized);
  const packageName = expectString(packageSection.name, "package.name", args.locator.normalized);
  const packageVersion = expectString(
    packageSection.version,
    "package.version",
    args.locator.normalized,
  );
  const packagePublic = readBoolean(packageSection.public, "package.public", args.locator.normalized);
  const packageDescription = readOptionalString(
    packageSection.description,
    "package.description",
    args.locator.normalized,
  );
  const packageLicense = readOptionalString(
    packageSection.license,
    "package.license",
    args.locator.normalized,
  );
  const packageHomepage = readOptionalString(
    packageSection.homepage,
    "package.homepage",
    args.locator.normalized,
  );
  const packageRepository = readOptionalString(
    packageSection.repository,
    "package.repository",
    args.locator.normalized,
  );
  const packageRootModule = readOptionalString(
    packageSection.root_module,
    "package.root_module",
    args.locator.normalized,
  );
  const packageCategories = readOptionalStringArray(
    packageSection.categories,
    "package.categories",
    args.locator.normalized,
  );
  const packageKeywords = readOptionalStringArray(
    packageSection.keywords,
    "package.keywords",
    args.locator.normalized,
  );

  return {
    package_locator: args.locator.normalized,
    source_url: canonicalSourceUrl(args.locator),
    package_subdir: packageSubdir(args.locator),
    selector: args.selector,
    resolved_sha: args.resolvedSha,
    package_name: packageName,
    package_version: packageVersion,
    package_public: packagePublic,
    package_description: packageDescription,
    package_license: packageLicense,
    package_homepage: packageHomepage,
    package_repository: packageRepository,
    package_root_module: packageRootModule,
    package_categories: packageCategories,
    package_keywords: packageKeywords,
    dependencies: extractDependencies(parsed.dependencies),
    source_archive_key: sourceArchiveKey(args.locator, args.resolvedSha),
    manifest_key: manifestKey(args.locator, args.resolvedSha),
    materialized_at: args.materializedAt,
  };
}

function parseTuskToml(source: string, locator: string): Record<string, unknown> {
  const parsed = parseToml(source);
  return asRecord(parsed, "document", locator);
}

function extractDependencies(section: unknown): Array<Record<string, unknown>> {
  if (section === undefined) {
    return [];
  }

  const dependencies = asRecord(section, "dependencies", "dependencies");
  return Object.entries(dependencies).map(([name, specification]) => {
    if (typeof specification === "string") {
      return {
        name,
        raw: specification,
      };
    }

    if (specification !== null && typeof specification === "object" && !Array.isArray(specification)) {
      return {
        name,
        ...specification,
      };
    }

    return {
      name,
      raw: specification,
    };
  });
}

function toRepoRelativeTuskTomlPath(locator: PackageLocator): string {
  return locator.subpath === null ? "tusk.toml" : `${locator.subpath}/tusk.toml`;
}

function asRecord(value: unknown, field: string, locator: string): Record<string, unknown> {
  if (value !== null && typeof value === "object" && !Array.isArray(value)) {
    return value as Record<string, unknown>;
  }

  throw new HttpError(
    422,
    "invalid_package_manifest",
    `Field ${field} in ${locator} must be a TOML table.`,
  );
}

function expectString(value: unknown, field: string, locator: string): string {
  if (typeof value === "string" && value.length > 0) {
    return value;
  }

  throw new HttpError(
    422,
    "invalid_package_manifest",
    `Field ${field} in ${locator} must be a non-empty string.`,
  );
}

function readBoolean(value: unknown, field: string, locator: string): boolean {
  if (value === undefined) {
    return false;
  }

  if (typeof value === "boolean") {
    return value;
  }

  throw new HttpError(
    422,
    "invalid_package_manifest",
    `Field ${field} in ${locator} must be a boolean when present.`,
  );
}

function readOptionalString(value: unknown, field: string, locator: string): string | undefined {
  if (value === undefined) {
    return undefined;
  }

  if (typeof value === "string" && value.length > 0) {
    return value;
  }

  throw new HttpError(
    422,
    "invalid_package_manifest",
    `Field ${field} in ${locator} must be a non-empty string when present.`,
  );
}

function readOptionalStringArray(
  value: unknown,
  field: string,
  locator: string,
): string[] | undefined {
  if (value === undefined) {
    return undefined;
  }

  if (!Array.isArray(value)) {
    throw new HttpError(
      422,
      "invalid_package_manifest",
      `Field ${field} in ${locator} must be an array of non-empty strings when present.`,
    );
  }

  const items = value.map((item) => {
    if (typeof item === "string" && item.length > 0) {
      return item;
    }

    throw new HttpError(
      422,
      "invalid_package_manifest",
      `Field ${field} in ${locator} must be an array of non-empty strings when present.`,
    );
  });

  return items.length === 0 ? undefined : items;
}
