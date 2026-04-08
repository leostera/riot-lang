import * as path from "node:path";
import * as vscode from "vscode";
import { readRiotInfo, type RiotInfoPackage, type RiotWorkspaceInfo } from "./riot";

export interface RiotDiscoveryRoot {
	root: vscode.Uri;
	kind: "package" | "workspace";
	packageName?: string;
	packages: RiotInfoPackage[];
}

export interface RiotDiscoveryTargets {
	manifests: RiotDiscoveryRoot[];
	workspaces: RiotDiscoveryRoot[];
	standalonePackages: RiotDiscoveryRoot[];
	packageRoots: Map<string, vscode.Uri>;
}

export const riotPackageKey = (workspaceRoot: vscode.Uri, packageName: string): string =>
	`${workspaceRoot.toString()}::${packageName}`;

const discoveryRoot = (info: RiotWorkspaceInfo): RiotDiscoveryRoot => ({
	root: info.root,
	kind: info.kind,
	packageName: info.kind === "package"
		? info.packages[0]?.name ?? path.basename(info.root.fsPath)
		: undefined,
	packages: info.packages,
});

export const discoverRiotRoots = async (
	context: vscode.ExtensionContext,
): Promise<RiotDiscoveryTargets> => {
	const folders = vscode.workspace.workspaceFolders ?? [];
	const resolvedRoots = new Map<string, RiotWorkspaceInfo>();

	for (const folder of folders) {
		const info = await readRiotInfo(context, folder.uri);
		if (!info) {
			continue;
		}

		resolvedRoots.set(info.root.toString(), info);
	}

	const infos = [...resolvedRoots.values()]
		.sort((left, right) => left.root.fsPath.localeCompare(right.root.fsPath));
	const manifests = infos.map(discoveryRoot);
	const workspaces = manifests.filter((manifest) => manifest.kind === "workspace");
	const standalonePackages = manifests.filter((manifest) => manifest.kind === "package");

	const packageRoots = new Map<string, vscode.Uri>();
	for (const info of infos) {
		for (const pkg of info.packages) {
			packageRoots.set(riotPackageKey(info.root, pkg.name), pkg.root);
		}
	}

	return {
		manifests,
		workspaces,
		standalonePackages,
		packageRoots,
	};
};
