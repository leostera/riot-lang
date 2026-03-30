import { describe, expect, test } from "bun:test";

import { handleRequest } from "../src/routes.ts";
import { applyMetadataMigrations, readPackageClaim, readUserLoginRecord } from "../src/metadata-db.ts";
import { FakeExecutionContext, makeEnv, makeTarGz, withMockedFetch } from "./helpers.ts";

const SHA = "0123456789abcdef0123456789abcdef01234567";

describe("riot package registry auth", () => {
  test("github oauth callback creates a session and /v1/me returns the user", async () => {
    const { env, db } = makeEnv({
      GITHUB_OAUTH_CLIENT_ID: "github-client-id",
      GITHUB_OAUTH_CLIENT_SECRET: "github-client-secret",
      PKGS_WEB_BASE_URL: "https://pkgs.ml",
    });

    const startCtx = new FakeExecutionContext();
    const startResponse = await handleRequest(
      new Request("https://registry.test/v1/auth/github/start?return_to=/u/leostera/tokens"),
      env,
      startCtx,
    );
    await startCtx.drain();

    expect(startResponse.status).toBe(302);
    const authorizationUrl = new URL(startResponse.headers.get("location") ?? "");
    expect(authorizationUrl.origin).toBe("https://github.com");
    expect(authorizationUrl.pathname).toBe("/login/oauth/authorize");

    const state = authorizationUrl.searchParams.get("state");
    expect(typeof state).toBe("string");
    expect(state).not.toBeNull();

    const callbackCtx = new FakeExecutionContext();
    const callbackResponse = await withMockedFetch(async (input) => {
      const url = new URL(toRequestUrl(input));
      if (url.origin === "https://github.com" && url.pathname === "/login/oauth/access_token") {
        return Response.json({
          access_token: "github-access-token",
          token_type: "bearer",
        });
      }

      if (url.origin === "https://api.github.com" && url.pathname === "/user") {
        return Response.json({
          id: 42,
          login: "leostera",
          name: "Leo Stera",
          avatar_url: "https://avatars.githubusercontent.com/u/42",
        });
      }

      if (url.origin === "https://api.github.com" && url.pathname === "/user/emails") {
        return Response.json([
          {
            email: "leo@example.com",
            primary: true,
            verified: true,
          },
        ]);
      }

      throw new Error(`Unexpected fetch to ${url.toString()}`);
    }, async () => {
      return await handleRequest(
        new Request(
          `https://registry.test/v1/auth/github/callback?code=oauth-code&state=${encodeURIComponent(
            state ?? "",
          )}`,
        ),
        env,
        callbackCtx,
      );
    });
    await callbackCtx.drain();

    expect(callbackResponse.status).toBe(302);
    expect(callbackResponse.headers.get("location")).toBe("https://pkgs.ml/u/leostera/tokens");

    const cookie = callbackResponse.headers.get("set-cookie");
    expect(cookie).toContain("pkgs_session=");
    await applyMetadataMigrations(db as unknown as D1Database);
    expect(await readUserLoginRecord(db as unknown as D1Database, "leostera")).not.toBeNull();

    const sessionCtx = new FakeExecutionContext();
    const sessionResponse = await handleRequest(
      new Request("https://registry.test/v1/me", {
        headers: {
          cookie: cookie ?? "",
          accept: "application/json",
        },
      }),
      env,
      sessionCtx,
    );
    await sessionCtx.drain();

    expect(sessionResponse.status).toBe(200);
    expect(await readJson(sessionResponse)).toMatchObject({
      authenticated: true,
      user: {
        github_login: "leostera",
        github_name: "Leo Stera",
        github_email: "leo@example.com",
        github_email_verified: true,
      },
    });
  });

  test("github oauth callback rejects users without verified primary email", async () => {
    const { env } = makeEnv({
      GITHUB_OAUTH_CLIENT_ID: "github-client-id",
      GITHUB_OAUTH_CLIENT_SECRET: "github-client-secret",
      PKGS_WEB_BASE_URL: "https://pkgs.ml",
    });

    const startCtx = new FakeExecutionContext();
    const startResponse = await handleRequest(
      new Request("https://registry.test/v1/auth/github/start?return_to=/u/leostera/tokens"),
      env,
      startCtx,
    );
    await startCtx.drain();

    const state = new URL(startResponse.headers.get("location") ?? "").searchParams.get("state");
    if (state === null) {
      throw new Error("OAuth state was missing.");
    }

    const callbackCtx = new FakeExecutionContext();
    const callbackResponse = await withMockedFetch(async (input) => {
      const url = new URL(toRequestUrl(input));
      if (url.origin === "https://github.com" && url.pathname === "/login/oauth/access_token") {
        return Response.json({
          access_token: "github-access-token",
          token_type: "bearer",
        });
      }
      if (url.origin === "https://api.github.com" && url.pathname === "/user") {
        return Response.json({
          id: 42,
          login: "leostera",
          name: "Leo Stera",
          avatar_url: "https://avatars.githubusercontent.com/u/42",
        });
      }
      if (url.origin === "https://api.github.com" && url.pathname === "/user/emails") {
        return Response.json([
          {
            email: "leo@example.com",
            primary: true,
            verified: false,
          },
        ]);
      }
      throw new Error(`Unexpected fetch to ${url.toString()}`);
    }, async () => {
      return await handleRequest(
        new Request(
          `https://registry.test/v1/auth/github/callback?code=oauth-code&state=${encodeURIComponent(
            state,
          )}`,
        ),
        env,
        callbackCtx,
      );
    });
    await callbackCtx.drain();

    expect(callbackResponse.status).toBe(403);
    expect(await readJson(callbackResponse)).toMatchObject({
      error: "github_email_not_verified",
    });
  });

  test("authenticated users can create, list, and revoke publish tokens", async () => {
    const { env } = makeEnv({
      GITHUB_OAUTH_CLIENT_ID: "github-client-id",
      GITHUB_OAUTH_CLIENT_SECRET: "github-client-secret",
      PKGS_WEB_BASE_URL: "https://pkgs.ml",
    });

    const cookie = await loginAsGitHubUser(env, "leostera");

    const createCtx = new FakeExecutionContext();
    const createResponse = await handleRequest(
      new Request("https://registry.test/v1/me/tokens", {
        method: "POST",
        headers: {
          cookie,
          accept: "application/json",
          "content-type": "application/json",
        },
        body: JSON.stringify({ name: "local publish" }),
      }),
      env,
      createCtx,
    );
    await createCtx.drain();

    expect(createResponse.status).toBe(201);
    const created = (await readJson(createResponse)) as {
      plaintext_token: string;
      token: { token_id: string; name: string };
    };
    expect(created.plaintext_token.startsWith("sk-")).toBe(true);
    expect(created.token.name).toBe("local publish");

    const listCtx = new FakeExecutionContext();
    const listResponse = await handleRequest(
      new Request("https://registry.test/v1/me/tokens", {
        headers: {
          cookie,
          accept: "application/json",
        },
      }),
      env,
      listCtx,
    );
    await listCtx.drain();

    expect(listResponse.status).toBe(200);
    expect(await readJson(listResponse)).toMatchObject({
      user: {
        github_login: "leostera",
      },
      tokens: [
        {
          token_id: created.token.token_id,
          name: "local publish",
          capabilities: ["publish"],
        },
      ],
    });

    const deleteCtx = new FakeExecutionContext();
    const deleteResponse = await handleRequest(
      new Request(`https://registry.test/v1/me/tokens/${created.token.token_id}`, {
        method: "DELETE",
        headers: {
          cookie,
          accept: "application/json",
        },
      }),
      env,
      deleteCtx,
    );
    await deleteCtx.drain();

    expect(deleteResponse.status).toBe(200);
    expect(await readJson(deleteResponse)).toMatchObject({
      token: {
        token_id: created.token.token_id,
        revoked_at: expect.any(String),
      },
    });
  });

  test("publish tokens can create claims for matching github owners and adopt legacy claims", async () => {
    const { env, db, queue, indexedQueue } = makeEnv({
      GITHUB_OAUTH_CLIENT_ID: "github-client-id",
      GITHUB_OAUTH_CLIENT_SECRET: "github-client-secret",
      PKGS_WEB_BASE_URL: "https://pkgs.ml",
    });
    const cookie = await loginAsGitHubUser(env, "leostera");
    const publishToken = await createPublishToken(env, cookie, "publish");
    const archive = await makeTarGz({
      "tusk.toml": [
        "[package]",
        'name = "minttea"',
        'version = "0.4.2"',
        "public = true",
        'description = "Terminal UI toolkit for Riot"',
        'license = "MIT"',
      ].join("\n"),
    });

    await withMockedFetch(async (input) => {
      const url = new URL(toRequestUrl(input));
      if (url.pathname === "/repos/leostera/minttea") {
        return Response.json({ private: false });
      }
      if (url.pathname === "/repos/leostera/minttea/commits/main") {
        return Response.json({ sha: SHA });
      }
      if (url.pathname === `/repos/leostera/minttea/tarball/${SHA}`) {
        return new Response(archive, { status: 200 });
      }

      throw new Error(`Unexpected fetch to ${url.toString()}`);
    }, async () => {
      const rootCtx = new FakeExecutionContext();
      const rootPublish = await handleRequest(
        new Request("https://registry.test/package/leostera/minttea/-/publish?ref=main", {
          method: "POST",
          headers: {
            authorization: "Bearer root-secret",
          },
        }),
        env,
        rootCtx,
      );
      await rootCtx.drain();
      expect(rootPublish.status).toBe(200);

      const userCtx = new FakeExecutionContext();
      const userPublish = await handleRequest(
        new Request("https://registry.test/package/leostera/minttea/-/publish?ref=main", {
          method: "POST",
          headers: {
            authorization: `Bearer ${publishToken}`,
          },
        }),
        env,
        userCtx,
      );
      await userCtx.drain();

      expect(userPublish.status).toBe(200);
    });

    await applyMetadataMigrations(db as unknown as D1Database);
    const claim = await readPackageClaim(db as unknown as D1Database, "minttea");
    expect(claim).toMatchObject({
      owner_github_login: "leostera",
    });
    expect(queue.messages).toHaveLength(1);
    expect(indexedQueue.messages).toHaveLength(2);
  });

  test("publish tokens cannot claim packages for a different github owner", async () => {
    const { env, db } = makeEnv({
      GITHUB_OAUTH_CLIENT_ID: "github-client-id",
      GITHUB_OAUTH_CLIENT_SECRET: "github-client-secret",
      PKGS_WEB_BASE_URL: "https://pkgs.ml",
    });
    const cookie = await loginAsGitHubUser(env, "someoneelse");
    const publishToken = await createPublishToken(env, cookie, "publish");
    const archive = await makeTarGz({
      "tusk.toml": [
        "[package]",
        'name = "minttea"',
        'version = "0.4.2"',
        "public = true",
        'description = "Terminal UI toolkit for Riot"',
        'license = "MIT"',
      ].join("\n"),
    });

    await withMockedFetch(async (input) => {
      const url = new URL(toRequestUrl(input));
      if (url.pathname === "/repos/leostera/minttea") {
        return Response.json({ private: false });
      }
      if (url.pathname === "/repos/leostera/minttea/commits/main") {
        return Response.json({ sha: SHA });
      }
      if (url.pathname === `/repos/leostera/minttea/tarball/${SHA}`) {
        return new Response(archive, { status: 200 });
      }

      throw new Error(`Unexpected fetch to ${url.toString()}`);
    }, async () => {
      const publishCtx = new FakeExecutionContext();
      const publishResponse = await handleRequest(
        new Request("https://registry.test/package/leostera/minttea/-/publish?ref=main", {
          method: "POST",
          headers: {
            authorization: `Bearer ${publishToken}`,
          },
        }),
        env,
        publishCtx,
      );
      await publishCtx.drain();

      expect(publishResponse.status).toBe(403);
      expect(await readJson(publishResponse)).toMatchObject({
        error: "package_claim_forbidden",
      });
    });

    await applyMetadataMigrations(db as unknown as D1Database);
    expect(await readPackageClaim(db as unknown as D1Database, "minttea")).toBeNull();
  });
});

async function loginAsGitHubUser(env: ReturnType<typeof makeEnv>["env"], login: string): Promise<string> {
  const startCtx = new FakeExecutionContext();
  const startResponse = await handleRequest(
    new Request(`https://registry.test/v1/auth/github/start?return_to=${encodeURIComponent(`/u/${login}/tokens`)}`),
    env,
    startCtx,
  );
  await startCtx.drain();

  const authorizationUrl = new URL(startResponse.headers.get("location") ?? "");
  const state = authorizationUrl.searchParams.get("state");
  if (state === null) {
    throw new Error("OAuth state was missing.");
  }

  const callbackCtx = new FakeExecutionContext();
  const callbackResponse = await withMockedFetch(async (input) => {
    const url = new URL(toRequestUrl(input));
    if (url.origin === "https://github.com" && url.pathname === "/login/oauth/access_token") {
      return Response.json({
        access_token: `token-for-${login}`,
        token_type: "bearer",
      });
    }

    if (url.origin === "https://api.github.com" && url.pathname === "/user") {
      return Response.json({
        id: Math.floor(Math.random() * 10_000),
        login,
        name: login,
      });
    }

    if (url.origin === "https://api.github.com" && url.pathname === "/user/emails") {
      return Response.json([
        {
          email: `${login}@example.com`,
          primary: true,
          verified: true,
        },
      ]);
    }

    throw new Error(`Unexpected fetch to ${url.toString()}`);
  }, async () => {
    return await handleRequest(
      new Request(
        `https://registry.test/v1/auth/github/callback?code=oauth-code&state=${encodeURIComponent(state)}`,
      ),
      env,
      callbackCtx,
    );
  });
  await callbackCtx.drain();

  const cookie = callbackResponse.headers.get("set-cookie");
  if (cookie === null) {
    throw new Error("Session cookie was missing.");
  }

  return cookie;
}

async function createPublishToken(
  env: ReturnType<typeof makeEnv>["env"],
  cookie: string,
  name: string,
): Promise<string> {
  const ctx = new FakeExecutionContext();
  const response = await handleRequest(
    new Request("https://registry.test/v1/me/tokens", {
      method: "POST",
      headers: {
        cookie,
        accept: "application/json",
        "content-type": "application/json",
      },
      body: JSON.stringify({ name }),
    }),
    env,
    ctx,
  );
  await ctx.drain();

  const payload = (await readJson(response)) as { plaintext_token: string };
  return payload.plaintext_token;
}

async function readJson(response: Response): Promise<unknown> {
  return await response.json();
}

function toRequestUrl(input: RequestInfo | URL): string {
  if (input instanceof Request) {
    return input.url;
  }

  return typeof input === "string" ? input : input.toString();
}
