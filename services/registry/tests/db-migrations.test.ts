import { describe, expect, test } from "bun:test";

import { listRequestLogs, writeRequestLog } from "../src/metadata-db.ts";
import { makeEnv } from "./helpers.ts";

describe("registry metadata migrations", () => {
  test("fresh metadata migrations create request_logs and allow writes", async () => {
    const { db } = makeEnv();

    await writeRequestLog(db as unknown as D1Database, {
      request_id: "req_123",
      request_timestamp: "2026-03-31T23:40:00.000Z",
      method: "GET",
      path: "/v1/views/categories",
      route: "views.categories",
      status: 200,
      success: true,
      user_agent: "bun:test",
    });

    const logs = await listRequestLogs(db as unknown as D1Database, 10);
    expect(logs).toEqual([
      expect.objectContaining({
        request_id: "req_123",
        route: "views.categories",
        status: 200,
        success: true,
      }),
    ]);
  });
});
