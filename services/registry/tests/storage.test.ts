import { describe, expect, test } from "bun:test";

import { packageIndexKey, requestLogKey } from "../src/storage.ts";
import type { IndexConfig, RequestLogEntry } from "../src/types.ts";

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

  test("package documents use cargo-style sharding", () => {
    const config: IndexConfig = {
      cdnBaseUrl: "https://cdn.pkgs.ml",
      indexBasePath: "index/v1",
      viewsBasePath: "views/v1",
      authCookieDomain: "pkgs.ml",
      pkgsWebBaseUrl: "https://pkgs.ml",
    };

    expect(packageIndexKey(config, "x")).toBe("index/v1/1/x.json");
    expect(packageIndexKey(config, "io")).toBe("index/v1/2/io.json");
    expect(packageIndexKey(config, "mcp")).toBe("index/v1/3/m/mcp.json");
    expect(packageIndexKey(config, "kernel")).toBe("index/v1/ke/rn/kernel.json");
  });
});
