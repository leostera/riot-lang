import type { PackageIndexDocument, SearchConfig } from "./types.ts";

export function packageIndexKey(config: SearchConfig, packageName: string): string {
  const normalized = packageName.toLowerCase();

  if (normalized.length === 1) {
    return `${config.indexBasePath}/1/${normalized}.json`;
  }

  if (normalized.length === 2) {
    return `${config.indexBasePath}/2/${normalized}.json`;
  }

  if (normalized.length === 3) {
    return `${config.indexBasePath}/3/${normalized[0]}/${normalized}.json`;
  }

  return `${config.indexBasePath}/${normalized.slice(0, 2)}/${normalized.slice(2, 4)}/${normalized}.json`;
}

export async function readPackageIndexDocument(
  bucket: R2Bucket,
  key: string,
): Promise<PackageIndexDocument | null> {
  const object = await bucket.get(key);
  if (object === null) {
    return null;
  }

  return await object.json<PackageIndexDocument>();
}
