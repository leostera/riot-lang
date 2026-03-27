import { handlePublicationCoordinatorRequest } from "../src/publication-coordinator-handler.ts";
import type { Env } from "../src/types.ts";

interface StoredObject {
  body: Uint8Array;
  httpMetadata?: {
    contentType?: string;
  };
}

export class FakeR2Bucket {
  private readonly objects = new Map<string, StoredObject>();

  async head(key: string): Promise<R2Object | null> {
    const object = this.objects.get(key);
    if (object === undefined) {
      return null;
    }

    return {
      key,
      size: object.body.byteLength,
      etag: "fake-etag",
      httpEtag: "fake-etag",
      uploaded: new Date(),
      checksums: {},
      version: "1",
      writeHttpMetadata(headers: Headers): void {
        if (object.httpMetadata?.contentType !== undefined) {
          headers.set("content-type", object.httpMetadata.contentType);
        }
      },
      httpMetadata: object.httpMetadata,
      customMetadata: {},
      range: undefined,
      storageClass: "Standard",
      ssecKeyMd5: undefined,
    } as unknown as R2Object;
  }

  async get(key: string): Promise<R2ObjectBody | null> {
    const object = this.objects.get(key);
    if (object === undefined) {
      return null;
    }

    const bytes = copyBytes(object.body);
    const body = new Response(bytes).body;
    if (body === null) {
      throw new Error("Response body was unexpectedly null.");
    }

    return {
      key,
      size: object.body.byteLength,
      etag: "fake-etag",
      httpEtag: "fake-etag",
      uploaded: new Date(),
      checksums: {},
      version: "1",
      body,
      bodyUsed: false,
      arrayBuffer: async () => copyBytes(object.body).buffer,
      text: async () => new TextDecoder().decode(object.body),
      json: async <T>() => JSON.parse(new TextDecoder().decode(object.body)) as T,
      blob: async () => new Blob([copyBytes(object.body)]),
      writeHttpMetadata(headers: Headers): void {
        if (object.httpMetadata?.contentType !== undefined) {
          headers.set("content-type", object.httpMetadata.contentType);
        }
      },
      httpMetadata: object.httpMetadata,
      customMetadata: {},
      range: undefined,
      storageClass: "Standard",
      ssecKeyMd5: undefined,
    } as unknown as R2ObjectBody;
  }

  async put(
    key: string,
    value: string | ArrayBuffer | ArrayBufferView,
    options?: R2PutOptions,
  ): Promise<R2Object> {
    const body = encodeValue(value);
    this.objects.set(key, {
      body,
      httpMetadata: normalizeHttpMetadata(options?.httpMetadata),
    });

    const head = await this.head(key);
    if (head === null) {
      throw new Error("Stored object was unexpectedly missing.");
    }

    return head;
  }

  async text(key: string): Promise<string | null> {
    const object = this.objects.get(key);
    return object === undefined ? null : new TextDecoder().decode(object.body);
  }

  keys(): string[] {
    return [...this.objects.keys()].sort();
  }
}

export class FakeQueue {
  readonly messages: unknown[] = [];

  async send(message: unknown): Promise<void> {
    this.messages.push(message);
  }
}

export class FakeExecutionContext implements ExecutionContext {
  props: unknown = undefined;
  private readonly promises: Promise<unknown>[] = [];

  waitUntil(promise: Promise<unknown>): void {
    this.promises.push(promise);
  }

  passThroughOnException(): void {}

  async drain(): Promise<void> {
    await Promise.all(this.promises);
  }
}

class FakeDurableObjectNamespace {
  async fetch(request: Request, env: Env): Promise<Response> {
    return await handlePublicationCoordinatorRequest(request, env);
  }

  idFromName(name: string): DurableObjectId {
    return { toString: () => name } as DurableObjectId;
  }

  get(_id: DurableObjectId): DurableObjectStub {
    return {
      fetch: async (input: RequestInfo | URL, init?: RequestInit) => {
        const request = input instanceof Request ? input : new Request(input, init);
        return await this.fetch(request, this.env);
      },
    } as DurableObjectStub;
  }

  constructor(private readonly env: Env) {}
}

export function makeEnv(overrides: Partial<Env> = {}): {
  env: Env;
  bucket: FakeR2Bucket;
  queue: FakeQueue;
  indexedQueue: FakeQueue;
} {
  const bucket = new FakeR2Bucket();
  const queue = new FakeQueue();
  const indexedQueue = new FakeQueue();

  const env: Env = {
    ML_PKGS_CDN: bucket as unknown as R2Bucket,
    PACKAGE_PUBLISHED_QUEUE: queue as unknown as Queue,
    PACKAGE_INDEXED_QUEUE: indexedQueue as unknown as Queue,
    PUBLICATION_COORDINATOR: undefined as unknown as DurableObjectNamespace,
    CDN_BASE_URL: "https://cdn.pkgs.ml",
    INDEX_BASE_PATH: "index/v1",
    GITHUB_TOKEN: "",
    ROOT_AUTH_TOKEN: "root-secret",
    ...overrides,
  };

  if (overrides.PUBLICATION_COORDINATOR === undefined) {
    env.PUBLICATION_COORDINATOR = new FakeDurableObjectNamespace(env) as unknown as DurableObjectNamespace;
  }

  return {
    env,
    bucket,
    queue,
    indexedQueue,
  };
}

export async function withMockedFetch<T>(
  mock: (input: RequestInfo | URL, init?: RequestInit) => Promise<Response>,
  run: () => Promise<T>,
): Promise<T> {
  const originalFetch = globalThis.fetch;
  globalThis.fetch = mock as typeof fetch;

  try {
    return await run();
  } finally {
    globalThis.fetch = originalFetch;
  }
}

export async function makeTarGz(
  files: Record<string, string>,
  rootPrefix = "repo-root",
): Promise<Uint8Array<ArrayBuffer>> {
  const chunks: Uint8Array[] = [];

  for (const [path, content] of Object.entries(files)) {
    const body = new TextEncoder().encode(content);
    const header = makeTarHeader(`${rootPrefix}/${path}`, body.byteLength);
    chunks.push(header, body, new Uint8Array(padLength(body.byteLength)));
  }

  chunks.push(new Uint8Array(1024));

  const tarBytes = concatBytes(chunks);
  const stream = new Response(tarBytes).body;
  if (stream === null) {
    throw new Error("Tar archive stream was unexpectedly null.");
  }

  const gzipStream = stream.pipeThrough(new CompressionStream("gzip"));
  return new Uint8Array(await new Response(gzipStream).arrayBuffer());
}

function encodeValue(value: string | ArrayBuffer | ArrayBufferView): Uint8Array {
  if (typeof value === "string") {
    return new TextEncoder().encode(value);
  }

  if (value instanceof ArrayBuffer) {
    return new Uint8Array(value);
  }

  return copyBytes(new Uint8Array(value.buffer, value.byteOffset, value.byteLength));
}

function copyBytes(value: Uint8Array): Uint8Array<ArrayBuffer> {
  return Uint8Array.from(value);
}

function normalizeHttpMetadata(
  metadata: Headers | R2HTTPMetadata | undefined,
): StoredObject["httpMetadata"] {
  if (metadata === undefined) {
    return undefined;
  }

  if (metadata instanceof Headers) {
    const contentType = metadata.get("content-type");
    return contentType === null ? undefined : { contentType };
  }

  return {
    contentType: metadata.contentType,
  };
}

function makeTarHeader(path: string, size: number): Uint8Array {
  const header = new Uint8Array(512);

  writeTarString(header, 0, 100, path);
  writeTarOctal(header, 100, 8, 0o644);
  writeTarOctal(header, 108, 8, 0);
  writeTarOctal(header, 116, 8, 0);
  writeTarOctal(header, 124, 12, size);
  writeTarOctal(header, 136, 12, 0);
  header[156] = "0".charCodeAt(0);
  writeTarString(header, 257, 6, "ustar");
  writeTarString(header, 263, 2, "00");

  for (let index = 148; index < 156; index += 1) {
    header[index] = 0x20;
  }

  const checksum = header.reduce((sum, value) => sum + value, 0);
  writeTarOctal(header, 148, 8, checksum);
  return header;
}

function writeTarString(
  header: Uint8Array,
  offset: number,
  length: number,
  value: string,
): void {
  const bytes = new TextEncoder().encode(value);
  header.set(bytes.subarray(0, length), offset);
}

function writeTarOctal(
  header: Uint8Array,
  offset: number,
  length: number,
  value: number,
): void {
  const encoded = value.toString(8).padStart(length - 1, "0");
  writeTarString(header, offset, length - 1, encoded);
  header[offset + length - 1] = 0;
}

function padLength(size: number): number {
  const remainder = size % 512;
  return remainder === 0 ? 0 : 512 - remainder;
}

function concatBytes(chunks: Uint8Array[]): Uint8Array<ArrayBuffer> {
  const totalLength = chunks.reduce((sum, chunk) => sum + chunk.byteLength, 0);
  const result = new Uint8Array(totalLength);
  let offset = 0;

  for (const chunk of chunks) {
    result.set(chunk, offset);
    offset += chunk.byteLength;
  }

  return result;
}
