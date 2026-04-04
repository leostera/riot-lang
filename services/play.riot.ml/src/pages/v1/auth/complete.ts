import type { APIRoute } from "astro";
import { env } from "cloudflare:workers";

import { completePlaySessionHandoff, type PlayAuthEnv } from "@/lib/auth.ts";

export const GET: APIRoute = async ({ request }) => {
  const url = new URL(request.url);
  const handoffId = url.searchParams.get("handoff");
  if (handoffId === null || handoffId.trim().length === 0) {
    return new Response("Missing handoff id", {
      status: 400,
      headers: {
        "content-type": "text/plain; charset=utf-8",
      },
    });
  }

  const { returnTo, sessionCookie } = await completePlaySessionHandoff(env as unknown as PlayAuthEnv, handoffId);

  return new Response(null, {
    status: 302,
    headers: {
      location: returnTo,
      "set-cookie": sessionCookie,
    },
  });
};
