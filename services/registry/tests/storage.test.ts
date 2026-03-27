import { describe, expect, test } from "bun:test";

import { requestLogKey } from "../src/storage.ts";
import type { RequestLogEntry } from "../src/types.ts";

describe("registry storage", () => {
  test("request logs are partitioned by UTC hour", () => {
    const entry: RequestLogEntry = {
      request_id: "01HZXTEST",
      request_timestamp: "2026-03-27T12:34:56.000Z",
      method: "GET",
      path: "/",
      route: "root",
      status: 200,
      success: true,
      user_agent: "bun:test",
    };

    expect(requestLogKey(entry)).toBe("requests/2026/03/27/12/01HZXTEST.json");
  });
});
