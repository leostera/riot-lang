export { PublicationCoordinator } from "./src/publication-coordinator.ts";

import { handleRequest } from "./src/routes.ts";
import type { Env } from "./src/types.ts";

export default {
  async fetch(request, env, ctx): Promise<Response> {
    return await handleRequest(request, env, ctx);
  },
} satisfies ExportedHandler<Env>;
