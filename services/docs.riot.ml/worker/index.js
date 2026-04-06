const missingOriginResponse = () =>
  new Response("Missing MINTLIFY_ORIGIN for docs proxy.", {
    status: 503,
    headers: {
      "content-type": "text/plain; charset=utf-8",
      "cache-control": "no-store"
    }
  });

const proxyHeaders = (request, targetHost) => {
  const sourceUrl = new URL(request.url);
  const headers = new Headers(request.headers);

  headers.delete("host");
  headers.set("origin", `https://${targetHost}`);
  headers.set("x-forwarded-host", sourceUrl.host);
  headers.set("x-forwarded-proto", sourceUrl.protocol.replace(":", ""));

  return headers;
};

export default {
  async fetch(request, env) {
    const targetHost = env.MINTLIFY_ORIGIN;

    if (!targetHost) {
      return missingOriginResponse();
    }

    const sourceUrl = new URL(request.url);
    const targetUrl = new URL(request.url);

    targetUrl.protocol = "https:";
    targetUrl.hostname = targetHost;

    const method = request.method;
    const hasBody = method != "GET" && method != "HEAD";

    return fetch(
      new Request(targetUrl, {
        method,
        headers: proxyHeaders(request, targetHost),
        body: hasBody ? request.body : undefined,
        redirect: "manual"
      })
    );
  }
};
