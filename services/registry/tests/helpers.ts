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

export function makeEnv(overrides: Partial<Env> = {}): {
  env: Env;
  bucket: FakeR2Bucket;
  queue: FakeQueue;
} {
  const bucket = new FakeR2Bucket();
  const queue = new FakeQueue();

  const env: Env = {
    ML_PKGS_CDN: bucket as unknown as R2Bucket,
    PACKAGE_PUBLISHED_QUEUE: queue as unknown as Queue,
    CDN_BASE_URL: "https://cdn.pkgs.ml",
    GITHUB_TOKEN: "",
    ...overrides,
  };

  return {
    env,
    bucket,
    queue,
  };
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
