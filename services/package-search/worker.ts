import { consumeIndexedBatch } from "./src/consumer.ts";
import { handleRequest } from "./src/routes.ts";
import type { Env, PackageIndexedEvent } from "./src/types.ts";

export default {
  async fetch(request: Request, env: Env): Promise<Response> {
    return await handleRequest(request, env);
  },

  async queue(
    batch: MessageBatch<PackageIndexedEvent>,
    env: Env,
    ctx: ExecutionContext,
  ): Promise<void> {
    await consumeIndexedBatch(batch, env, ctx);
  },
};
