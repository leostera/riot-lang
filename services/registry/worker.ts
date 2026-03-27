import { DurableObject } from 'cloudflare:workers';

export class RiotRegistry extends DurableObject<Env> {
	container: globalThis.Container;
	monitor?: Promise<unknown>;

	constructor(ctx: DurableObjectState, env: Env) {
		super(ctx, env);
		this.container = ctx.container!;
		void this.ctx.blockConcurrencyWhile(async () => {
			if (!this.container.running) this.container.start();
		});
	}

	async fetch(req: Request) {
		return await this.container.getTcpPort(8080).fetch(req);
	}
}

export default {
	async fetch(request, env): Promise<Response> {
		try {
			return await env.CODE_EXECUTOR.get(env.CODE_EXECUTOR.idFromName('executor')).fetch(request);
		} catch (err) {
			console.error('Error fetch:', err.message);
			return new Response(err.message, { status: 500 });
		}
	},
} satisfies ExportedHandler<Env>;
