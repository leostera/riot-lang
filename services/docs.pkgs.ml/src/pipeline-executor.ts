import { Container, getContainer } from "@cloudflare/containers";
import type {
  DocsPipelineProcessResult,
  PackagePipelineExecutor,
  PackagePipelineProcessRequest,
} from "./pipeline-types.ts";
export type {
  DocsPipelineProcessResult,
  PackagePipelineExecutor,
  PackagePipelineProcessRequest,
} from "./pipeline-types.ts";

export interface ContainerExecutorEnv {
  DOCS_PIPELINE_CONTAINER?: DurableObjectNamespace;
}

export class DocsPipelineContainer extends Container {
  defaultPort = 8080;
  sleepAfter = "5m";
  enableInternet = true;
  pingEndpoint = "/health";

  override async onActivityExpired(): Promise<void> {
    await this.destroy();
  }

  override onError(error: unknown): never {
    console.error("docs pipeline container error", error);
    if (error instanceof Error) {
      throw error;
    }

    throw new Error(String(error));
  }
}

export class ContainerPackagePipelineExecutor implements PackagePipelineExecutor {
  constructor(private readonly env: ContainerExecutorEnv) {}

  async processRelease(request: PackagePipelineProcessRequest): Promise<DocsPipelineProcessResult> {
    if (this.env.DOCS_PIPELINE_CONTAINER === undefined) {
      throw new Error("DOCS_PIPELINE_CONTAINER binding is not configured.");
    }

    const runnerId = buildRunnerId(
      request.package_name,
      request.package_version,
      request.artifact_sha256,
    );
    const container = getContainer(
      this.env.DOCS_PIPELINE_CONTAINER as unknown as DurableObjectNamespace<DocsPipelineContainer>,
      runnerId,
    );
    const response = await container.fetch(
      new Request("http://container/process", {
        method: "POST",
        headers: {
          "content-type": "application/json; charset=utf-8",
        },
        body: JSON.stringify(request),
      }),
    );

    if (!response.ok) {
      const body = await response.text();
      throw new Error(
        `container runner returned ${response.status}: ${body.slice(0, 500)}`,
      );
    }

    return (await response.json()) as DocsPipelineProcessResult;
  }
}

const RUNNER_REVISION = "v3";

function buildRunnerId(packageName: string, packageVersion: string, artifactSha256: string): string {
  const identity = `${RUNNER_REVISION}-${packageName}-${packageVersion}-${artifactSha256.slice(0, 12)}`;
  return identity
    .toLowerCase()
    .replaceAll(/[^a-z0-9-]+/g, "-")
    .replaceAll(/-+/g, "-")
    .replaceAll(/^-|-$/g, "");
}
