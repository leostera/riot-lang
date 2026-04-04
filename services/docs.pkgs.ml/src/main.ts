interface AssetFetcher {
  fetch(request: Request): Response | Promise<Response>;
}

interface StoredObject {
  key: string;
  size: number;
  body: ReadableStream | null;
  httpEtag: string;
  writeHttpMetadata(headers: Headers): void;
}

interface ObjectBucket {
  get(key: string): Promise<StoredObject | null>;
}

export interface Env {
  ASSETS: AssetFetcher;
  ML_PKGS_CDN: ObjectBucket;
}

const TEXT_CONTENT_TYPE = "text/plain; charset=utf-8";

export default {
  async fetch(request: Request, env: Env): Promise<Response> {
    return await handleRequest(request, env);
  },
};

async function handleRequest(request: Request, env: Env): Promise<Response> {
  if (request.method !== "GET" && request.method !== "HEAD") {
    return new Response("Method Not Allowed", {
      status: 405,
      headers: {
        allow: "GET, HEAD",
        "content-type": TEXT_CONTENT_TYPE,
      },
    });
  }

  const url = new URL(request.url);
  const match = matchPackageDocsPath(url.pathname);

  if (match === null) {
    return await env.ASSETS.fetch(request);
  }

  if (!url.pathname.endsWith("/") && match.rest.length === 0) {
    url.pathname = `${url.pathname}/`;
    return Response.redirect(url.toString(), 308);
  }

  const objectKey = resolveDocsObjectKey(match.packageName, match.version, match.rest);
  const object = await env.ML_PKGS_CDN.get(objectKey);
  if (object === null) {
    return new Response("Package docs not found", {
      status: 404,
      headers: {
        "content-type": TEXT_CONTENT_TYPE,
      },
    });
  }

  return await respondWithObject(request, object);
}

function matchPackageDocsPath(pathname: string):
  | { packageName: string; version: string; rest: string }
  | null {
  const segments = pathname.split("/").filter((segment) => segment.length > 0);
  if (segments[0] !== "p" || segments.length < 3) {
    return null;
  }

  const packageName = decodeURIComponent(segments[1] ?? "");
  const version = decodeURIComponent(segments[2] ?? "");
  const rest = segments.slice(3).map((segment) => decodeURIComponent(segment)).join("/");

  if (packageName.length === 0 || version.length === 0) {
    return null;
  }

  return {
    packageName,
    version,
    rest,
  };
}

function resolveDocsObjectKey(packageName: string, version: string, rest: string): string {
  if (rest.length === 0) {
    return `docs/${packageName}/${version}/index.html`;
  }

  if (rest.endsWith("/")) {
    return `docs/${packageName}/${version}/${rest}index.html`;
  }

  if (!rest.includes(".") && !rest.endsWith(".html")) {
    return `docs/${packageName}/${version}/${rest}/index.html`;
  }

  return `docs/${packageName}/${version}/${rest}`;
}

async function respondWithObject(request: Request, object: StoredObject): Promise<Response> {
  const etag = object.httpEtag;
  if (request.headers.get("if-none-match") === etag) {
    return new Response(null, {
      status: 304,
      headers: {
        etag,
        "cache-control": "public, max-age=300",
      },
    });
  }

  const headers = new Headers();
  object.writeHttpMetadata(headers);
  if (!headers.has("content-type")) {
    const fallbackContentType = contentTypeForKey(object.key);
    if (fallbackContentType !== null) {
      headers.set("content-type", fallbackContentType);
    }
  }
  headers.set("cache-control", cacheControlForKey(object.key));
  headers.set("etag", etag);
  headers.set("content-length", String(object.size));

  if (request.method === "HEAD") {
    return new Response(null, {
      status: 200,
      headers,
    });
  }

  return new Response(object.body, {
    status: 200,
    headers,
  });
}

function cacheControlForKey(key: string): string {
  if (key.endsWith(".html")) {
    return "public, max-age=300";
  }

  return "public, max-age=31536000, immutable";
}

function contentTypeForKey(key: string): string | null {
  if (key.endsWith(".html")) return "text/html; charset=utf-8";
  if (key.endsWith(".css")) return "text/css; charset=utf-8";
  if (key.endsWith(".js")) return "text/javascript; charset=utf-8";
  if (key.endsWith(".json")) return "application/json; charset=utf-8";
  if (key.endsWith(".svg")) return "image/svg+xml";
  if (key.endsWith(".txt")) return "text/plain; charset=utf-8";
  if (key.endsWith(".xml")) return "application/xml; charset=utf-8";
  if (key.endsWith(".wasm")) return "application/wasm";
  if (key.endsWith(".ico")) return "image/x-icon";
  if (key.endsWith(".png")) return "image/png";
  if (key.endsWith(".jpg") || key.endsWith(".jpeg")) return "image/jpeg";
  if (key.endsWith(".webp")) return "image/webp";
  return null;
}
