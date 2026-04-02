interface Env {
  RIOT_INSTALL_SCRIPT_URL?: string;
}

interface PagesContext {
  request: Request;
  env: Env;
  next: () => Promise<Response>;
}

const DEFAULT_INSTALL_SCRIPT_URL = "https://cdn.pkgs.ml/riot/install.sh";
const INSTALLER_HOST = "get.riot.ml";
const INSTALLER_PATHS = new Set(["/", "/install.sh"]);

export const onRequest = async (context: PagesContext): Promise<Response> => {
  const url = new URL(context.request.url);
  if (url.hostname !== INSTALLER_HOST) {
    return await context.next();
  }

  if (!INSTALLER_PATHS.has(url.pathname)) {
    return new Response("Not Found", {
      status: 404,
      headers: {
        "content-type": "text/plain; charset=utf-8",
      },
    });
  }

  const upstream = await fetch(context.env.RIOT_INSTALL_SCRIPT_URL ?? DEFAULT_INSTALL_SCRIPT_URL, {
    method: context.request.method === "HEAD" ? "HEAD" : "GET",
    headers: {
      accept: context.request.headers.get("accept") ?? "*/*",
    },
  });

  if (!upstream.ok) {
    return new Response("Installer unavailable", {
      status: 502,
      headers: {
        "content-type": "text/plain; charset=utf-8",
        "cache-control": "public, max-age=30",
      },
    });
  }

  const headers = new Headers(upstream.headers);
  headers.set("cache-control", "public, max-age=300");
  headers.set("x-robots-tag", "noindex");

  return new Response(context.request.method === "HEAD" ? null : upstream.body, {
    status: upstream.status,
    headers,
  });
};
