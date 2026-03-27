export function json(data: unknown, init: ResponseInit = {}): Response {
  const headers = new Headers(init.headers);
  if (!headers.has("content-type")) {
    headers.set("content-type", "application/json; charset=utf-8");
  }

  return new Response(JSON.stringify(data, null, 2), {
    ...init,
    headers,
  });
}

export function methodNotAllowed(methods: string[]): Response {
  return json(
    {
      error: "method_not_allowed",
      allowed_methods: methods,
    },
    {
      status: 405,
      headers: {
        allow: methods.join(", "),
      },
    },
  );
}
