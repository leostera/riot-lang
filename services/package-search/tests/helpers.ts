import { Database, type SQLQueryBindings } from "bun:sqlite";

import { consumeIndexedBatch } from "../src/consumer.ts";
import type { Env, PackageIndexedEvent, PackageIndexDocument } from "../src/types.ts";

interface StoredObject {
  body: Uint8Array;
  httpMetadata?: {
    contentType?: string;
  };
}

export class FakeR2Bucket {
  private readonly objects = new Map<string, StoredObject>();

  async get(key: string): Promise<R2ObjectBody | null> {
    const object = this.objects.get(key);
    if (object === undefined) {
      return null;
    }

    const body = new Blob([Uint8Array.from(object.body).buffer as ArrayBuffer]).stream();
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
      arrayBuffer: async () => Uint8Array.from(object.body).buffer as ArrayBuffer,
      text: async () => new TextDecoder().decode(object.body),
      json: async <T>() => JSON.parse(new TextDecoder().decode(object.body)) as T,
      blob: async () => new Blob([object.body.slice()]),
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

  async put(key: string, value: string): Promise<void> {
    this.objects.set(key, {
      body: new TextEncoder().encode(value),
      httpMetadata: {
        contentType: "application/json; charset=utf-8",
      },
    });
  }
}

export class FakeExecutionContext implements ExecutionContext {
  props: unknown = undefined;
  waitUntil(_promise: Promise<unknown>): void {}
  passThroughOnException(): void {}
}

export class FakeMessage<T> {
  acked = false;

  constructor(readonly body: T) {}

  ack(): void {
    this.acked = true;
  }

  retry(): void {}
}

export function makeBatch<T>(messages: T[]): MessageBatch<T> {
  const wrapped = messages.map((message) => new FakeMessage(message));

  return {
    queue: "test",
    messages: wrapped as unknown as Message<T>[],
    retryAll(): void {},
    ackAll(): void {
      for (const message of wrapped) {
        message.ack();
      }
    },
  };
}

export function makeEnv(): {
  env: Env;
  bucket: FakeR2Bucket;
  db: FakeD1Database;
} {
  const bucket = new FakeR2Bucket();
  const db = new FakeD1Database();

  const env: Env = {
    ML_PKGS_CDN: bucket as unknown as R2Bucket,
    SEARCH_DB: db as unknown as D1Database,
    CDN_BASE_URL: "https://cdn.pkgs.ml",
    INDEX_BASE_PATH: "index/v1",
  };

  return { env, bucket, db };
}

export async function putPackageIndexDocument(
  bucket: FakeR2Bucket,
  key: string,
  document: PackageIndexDocument,
): Promise<void> {
  await bucket.put(key, JSON.stringify(document, null, 2));
}

export async function consumeIndexed(
  env: Env,
  event: PackageIndexedEvent,
): Promise<void> {
  await consumeIndexedBatch(makeBatch([event]), env, new FakeExecutionContext());
}

class FakeD1Database {
  private readonly sqlite = new Database(":memory:");

  async exec(query: string): Promise<D1ExecResult> {
    this.sqlite.exec(query);
    return {
      count: 0,
      duration: 0,
    };
  }

  prepare(query: string): FakeD1PreparedStatement {
    return new FakeD1PreparedStatement(this.sqlite, query);
  }

  async batch<T = unknown>(
    statements: FakeD1PreparedStatement[],
  ): Promise<D1Result<T>[]> {
    return await Promise.all(statements.map(async (statement) => await statement.run<T>()));
  }
}

class FakeD1PreparedStatement {
  private bindings: SQLQueryBindings[] = [];

  constructor(
    private readonly sqlite: Database,
    private readonly query: string,
  ) {}

  bind(...values: SQLQueryBindings[]): FakeD1PreparedStatement {
    this.bindings = values;
    return this;
  }

  async run<T = unknown>(): Promise<D1Result<T>> {
    const statement = this.sqlite.query(this.query);
    statement.run(...this.bindings);
    return {
      success: true,
      meta: {
        changed_db: false,
        changes: 0,
        duration: 0,
        last_row_id: 0,
        rows_read: 0,
        rows_written: 0,
        served_by_region: "test",
        size_after: 0,
      },
    } as D1Result<T>;
  }

  async all<T = unknown>(): Promise<D1Result<T>> {
    const statement = this.sqlite.query(this.query);
    const results = statement.all(...this.bindings) as T[];
    return {
      success: true,
      meta: {
        changed_db: false,
        changes: 0,
        duration: 0,
        last_row_id: 0,
        rows_read: results.length,
        rows_written: 0,
        served_by_region: "test",
        size_after: 0,
      },
      results,
    } as D1Result<T>;
  }
}
