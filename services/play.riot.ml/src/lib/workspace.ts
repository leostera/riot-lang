export interface WorkspaceFile {
  path: string;
  sourceCode: string;
}

interface DependencySpec {
  name: string;
  requirement: string;
}

function parseDependencySpec(raw: string): DependencySpec | null {
  const trimmed = raw.trim();
  if (trimmed.length === 0) {
    return null;
  }

  const atIndex = trimmed.indexOf("@");
  const colonIndex = trimmed.indexOf(":");
  const separatorIndex =
    atIndex > 0 ? atIndex : colonIndex > 0 ? colonIndex : -1;

  if (separatorIndex === -1) {
    return {
      name: trimmed,
      requirement: "*",
    };
  }

  const name = trimmed.slice(0, separatorIndex).trim();
  const requestedRequirement = trimmed.slice(separatorIndex + 1).trim();
  if (name.length === 0) {
    return null;
  }

  const requirement =
    requestedRequirement.length === 0 || requestedRequirement === "latest"
      ? "*"
      : requestedRequirement;

  return {
    name,
    requirement,
  };
}

export function parseWorkspaceDependencies(value: string | null): DependencySpec[] {
  const requestedDependencies =
    value === null
      ? []
      : value
          .split(",")
          .map((entry) => parseDependencySpec(entry))
          .filter((entry): entry is DependencySpec => entry !== null);

  const deduped = new Map<string, DependencySpec>();
  deduped.set("std", {
    name: "std",
    requirement: "*",
  });

  for (const dependency of requestedDependencies) {
    deduped.set(dependency.name, dependency);
  }

  return Array.from(deduped.values());
}

function renderDependencies(dependencies: DependencySpec[]): string {
  return dependencies
    .map((dependency) => `${dependency.name} = "${dependency.requirement}"`)
    .join("\n");
}

function renderManifest(dependencies: DependencySpec[], entryPath: string): string {
  return `[package]
name = "playground"
version = "0.0.0"
public = false

[[bin]]
name = "playground"
path = "${entryPath}"

[dependencies]
${renderDependencies(dependencies)}
`;
}

function renderStarterSource(dependencies: DependencySpec[]): string {
  const requestedDependencies = dependencies.filter((dependency) => dependency.name !== "std");
  const dependencyComment =
    requestedDependencies.length === 0
      ? ""
      : `(* Requested dependencies:\n${requestedDependencies
          .map((dependency) => `   - ${dependency.name}:${dependency.requirement}`)
          .join("\n")}\n*)`;

  const dependencyBlock = dependencyComment.length > 0 ? `${dependencyComment}\n\n` : "";

  return `open Std

(* Riot Playground *)

${dependencyBlock}let main ~args =
  println "Start typing here"


let () = Actors.run ~main ~env:Env.args ()
`;
}

export function buildStarterWorkspace(dependencies: DependencySpec[]): {
  activePath: string;
  files: WorkspaceFile[];
} {
  const activePath = "/workspace/src/main.ml";

  return {
    activePath,
    files: [
      {
        path: "/workspace/riot.toml",
        sourceCode: renderManifest(dependencies, "src/main.ml"),
      },
      {
        path: activePath,
        sourceCode: renderStarterSource(dependencies),
      },
    ],
  };
}

export function buildExampleWorkspace(options: {
  packageName: string;
  packageVersion: string;
  examples: WorkspaceFile[];
  activeExamplePath: string;
}): {
  activePath: string;
  files: WorkspaceFile[];
} {
  const files: WorkspaceFile[] = [
    {
      path: "/workspace/riot.toml",
      sourceCode: renderManifest(
        [
          {
            name: "std",
            requirement: "*",
          },
          {
            name: options.packageName,
            requirement: options.packageVersion,
          },
        ],
        options.activeExamplePath.replace(/^\/workspace\//, ""),
      ),
    },
    ...options.examples,
  ];

  return {
    activePath: options.activeExamplePath,
    files,
  };
}
