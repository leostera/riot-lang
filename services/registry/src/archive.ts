import { HttpError } from "./errors.ts";

export async function readRepoFileFromTarGz(
  archiveBytes: Uint8Array<ArrayBuffer>,
  repoRelativePath: string,
): Promise<string | null> {
  const normalizedPath = repoRelativePath.replace(/^\/+|\/+$/g, "");
  const tarBytes = await gunzip(archiveBytes);

  for (const entry of iterateTarEntries(tarBytes)) {
    const relativePath = stripRootPrefix(entry.name);
    if (relativePath === normalizedPath) {
      return new TextDecoder().decode(entry.body);
    }
  }

  return null;
}

async function gunzip(bytes: Uint8Array<ArrayBuffer>): Promise<Uint8Array<ArrayBuffer>> {
  const stream = new Response(bytes).body;
  if (stream === null) {
    throw new HttpError(500, "archive_read_failed", "Archive body was unexpectedly empty.");
  }

  const decompressed = stream.pipeThrough(new DecompressionStream("gzip"));
  return new Uint8Array(await new Response(decompressed).arrayBuffer());
}

function *iterateTarEntries(bytes: Uint8Array<ArrayBuffer>): Iterable<TarEntry> {
  let offset = 0;

  while (offset + 512 <= bytes.length) {
    const header = bytes.subarray(offset, offset + 512);
    if (isZeroBlock(header)) {
      break;
    }

    const name = readString(header, 0, 100);
    const prefix = readString(header, 345, 155);
    const size = parseOctal(readString(header, 124, 12));
    const typeFlag = readString(header, 156, 1);
    const fullName = prefix.length > 0 ? `${prefix}/${name}` : name;
    const bodyOffset = offset + 512;
    const body = bytes.subarray(bodyOffset, bodyOffset + size);

    if (typeFlag === "" || typeFlag === "0") {
      yield { name: fullName, body };
    }

    offset = bodyOffset + alignTo512(size);
  }
}

function stripRootPrefix(path: string): string {
  const firstSlash = path.indexOf("/");
  return firstSlash === -1 ? path : path.slice(firstSlash + 1);
}

function readString(bytes: Uint8Array<ArrayBuffer>, start: number, length: number): string {
  const slice = bytes.subarray(start, start + length);
  const raw = new TextDecoder().decode(slice);
  const nul = raw.indexOf("\u0000");
  return (nul === -1 ? raw : raw.slice(0, nul)).trim();
}

function parseOctal(value: string): number {
  return value.length === 0 ? 0 : Number.parseInt(value, 8);
}

function alignTo512(size: number): number {
  return Math.ceil(size / 512) * 512;
}

function isZeroBlock(bytes: Uint8Array<ArrayBuffer>): boolean {
  for (const byte of bytes) {
    if (byte !== 0) {
      return false;
    }
  }

  return true;
}

interface TarEntry {
  name: string;
  body: Uint8Array<ArrayBuffer>;
}
