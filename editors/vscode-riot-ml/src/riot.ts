import * as vscode from "vscode";
import * as fs from "node:fs/promises";
import * as path from "node:path";
import { spawn } from "node:child_process";

export type Json =
	| null
	| boolean
	| number
	| string
	| Json[]
	| { [key: string]: Json };

export type JsonObject = { [key: string]: Json };

export interface CommandResult {
	code: number;
	stdout: string;
	stderr: string;
}

export interface ReleaseMetadata {
	release_id: string;
	build_sha: string;
	notes_url?: string;
	compare_url?: string;
	issues_url?: string;
}

const defaultInstallUrl = "https://get.riot.ml";
const defaultLatestMetadataUrl = "https://cdn.pkgs.ml/riot/latest.json";

export const isOcamlUri = (uri: vscode.Uri): boolean => {
	return uri.scheme === "file" && (uri.fsPath.endsWith(".ml") || uri.fsPath.endsWith(".mli"));
};

export const quote = (value: string): string => {
	return JSON.stringify(value);
};

export const getConfig = () => vscode.workspace.getConfiguration("riot");

export const configuredRiotPath = (): string | undefined => {
	const value = getConfig().get<string>("path");
	if (typeof value === "string" && value.trim() !== "") {
		return value.trim();
	}

	return undefined;
};

export const managedInstallHomePath = (context: vscode.ExtensionContext): string =>
	path.join(context.globalStorageUri.fsPath, "managed-home");

export const riotHomePath = (context: vscode.ExtensionContext): string =>
	path.join(managedInstallHomePath(context), ".riot");

export const managedRiotBinaryPath = (context: vscode.ExtensionContext): string =>
	path.join(riotHomePath(context), "bin", "riot");

export const managedReleaseMetadataPath = (context: vscode.ExtensionContext): string =>
	path.join(riotHomePath(context), "release.json");

export const latestReleaseMetadataUrl = (): string =>
	getConfig().get<string>("latestMetadataUrl") || defaultLatestMetadataUrl;

const exists = async (filePath: string): Promise<boolean> => {
	try {
		await fs.access(filePath);
		return true;
	} catch {
		return false;
	}
};

export const resolveRiotCommand = async (context: vscode.ExtensionContext): Promise<string> => {
	const configured = configuredRiotPath();
	if (configured) {
		return configured;
	}

	const managed = managedRiotBinaryPath(context);
	if (await exists(managed)) {
		return managed;
	}

	return "riot";
};

export const runCommand = async (
	command: string,
	args: string[],
	options: { cwd?: string; env?: NodeJS.ProcessEnv; stdin?: string } = {},
): Promise<CommandResult> =>
	new Promise((resolve, reject) => {
		const child = spawn(command, args, {
			cwd: options.cwd,
			env: { ...process.env, ...options.env },
			stdio: ["pipe", "pipe", "pipe"],
		});

		let stdout = "";
		let stderr = "";

		child.stdout.setEncoding("utf8");
		child.stderr.setEncoding("utf8");

		child.stdout.on("data", (chunk: string) => {
			stdout += chunk;
		});

		child.stderr.on("data", (chunk: string) => {
			stderr += chunk;
		});

		child.on("error", (error) => {
			reject(error);
		});

		child.on("close", (code) => {
			resolve({
				code: code ?? 1,
				stdout,
				stderr,
			});
		});

		child.stdin.setDefaultEncoding("utf8");
		child.stdin.end(options.stdin ?? "");
	});

export const runRiot = async (
	context: vscode.ExtensionContext,
	args: string[],
	options: { cwd?: string; env?: NodeJS.ProcessEnv } = {},
): Promise<CommandResult> => {
	const riot = await resolveRiotCommand(context);
	return runCommand(riot, args, options);
};

export const parseJsonLines = (stdout: string): JsonObject[] => {
	const results: JsonObject[] = [];

	for (const line of stdout.split(/\r?\n/)) {
		const trimmed = line.trim();
		if (trimmed.length === 0) {
			continue;
		}

		try {
			const parsed = JSON.parse(trimmed) as Json;
			if (parsed !== null && typeof parsed === "object" && !Array.isArray(parsed)) {
				results.push(parsed as JsonObject);
			}
		} catch {
			// Ignore non-JSON lines.
		}
	}

	return results;
};

export const normalizeFilePath = (filePath: string, cwd?: string): string => {
	if (path.isAbsolute(filePath)) {
		return path.normalize(filePath);
	}

	if (cwd) {
		return path.normalize(path.join(cwd, filePath));
	}

	return path.normalize(filePath);
};

export const nearestRiotRoot = async (uri: vscode.Uri): Promise<vscode.Uri | undefined> => {
	if (uri.scheme !== "file") {
		return undefined;
	}

	let current = path.dirname(uri.fsPath);
	while (true) {
		const manifest = path.join(current, "riot.toml");
		if (await exists(manifest)) {
			return vscode.Uri.file(current);
		}

		const parent = path.dirname(current);
		if (parent === current) {
			return undefined;
		}

		current = parent;
	}
};

export const workspaceRootFor = async (uri?: vscode.Uri): Promise<vscode.Uri | undefined> => {
	if (uri) {
		const root = await nearestRiotRoot(uri);
		if (root) {
			return root;
		}
	}

	const folder = uri ? vscode.workspace.getWorkspaceFolder(uri) : vscode.workspace.workspaceFolders?.[0];
	return folder?.uri;
};

const readInstalledReleaseMetadata = async (
	context: vscode.ExtensionContext,
): Promise<ReleaseMetadata | undefined> => {
	try {
		const contents = await fs.readFile(managedReleaseMetadataPath(context), "utf8");
		return JSON.parse(contents) as ReleaseMetadata;
	} catch {
		return undefined;
	}
};

export const parseVersionString = (version: string): ReleaseMetadata | undefined => {
	const trimmed = version.trim();
	const match = /^riot\s+(.+?)\s+\(build\s+([^)]+)\)$/.exec(trimmed);
	if (!match) {
		return undefined;
	}

	return {
		release_id: match[1].trim(),
		build_sha: match[2].trim(),
	};
};

export const sameReleaseIdentity = (
	left: Pick<ReleaseMetadata, "release_id" | "build_sha">,
	right: Pick<ReleaseMetadata, "release_id" | "build_sha">,
): boolean => left.release_id === right.release_id && left.build_sha === right.build_sha;

export const latestReleaseMetadata = async (): Promise<ReleaseMetadata> => {
	const response = await fetch(latestReleaseMetadataUrl(), {
		headers: {
			accept: "application/json, text/plain, */*",
		},
	});

	if (!response.ok) {
		throw new Error(`Failed to fetch Riot release metadata from ${latestReleaseMetadataUrl()}`);
	}

	return await response.json() as ReleaseMetadata;
};

export const currentReleaseMetadata = async (
	context: vscode.ExtensionContext,
): Promise<ReleaseMetadata | undefined> => {
	const installed = await readInstalledReleaseMetadata(context);
	if (installed) {
		return installed;
	}

	try {
		const resolved = await resolveRiotCommand(context);
		const version = await runCommand(resolved, ["--version"]);
		if (version.code !== 0) {
			return undefined;
		}

		return parseVersionString(version.stdout);
	} catch {
		return undefined;
	}
};

export interface ResolvedRiotVersion {
	command: string;
	version: string;
}

export const resolvedRiotVersion = async (
	context: vscode.ExtensionContext,
): Promise<ResolvedRiotVersion | undefined> => {
	try {
		const command = await resolveRiotCommand(context);
		const result = await runCommand(command, ["--version"]);
		if (result.code !== 0) {
			return undefined;
		}

		return {
			command,
			version: result.stdout.trim(),
		};
	} catch {
		return undefined;
	}
};

export const isManagedRiotCommand = async (
	context: vscode.ExtensionContext,
	command: string,
): Promise<boolean> => {
	return path.normalize(command) === path.normalize(managedRiotBinaryPath(context));
};

const readInstallerScript = async (): Promise<string> => {
	const installerUrl = getConfig().get<string>("installUrl") || defaultInstallUrl;
	const response = await fetch(installerUrl, {
		headers: {
			accept: "text/plain, */*",
		},
	});

	if (!response.ok) {
		throw new Error(`Failed to download Riot installer from ${installerUrl}`);
	}

	return await response.text();
};

export const installManagedRiot = async (context: vscode.ExtensionContext): Promise<ReleaseMetadata> => {
	await fs.mkdir(context.globalStorageUri.fsPath, { recursive: true });
	const managedHome = managedInstallHomePath(context);
	await fs.mkdir(managedHome, { recursive: true });

	const script = await readInstallerScript();
	const install = await runCommand("sh", [], {
		cwd: managedHome,
		env: {
			HOME: managedHome,
			SHELL: "riot-vscode-extension",
		},
		stdin: script,
	});

	if (install.code !== 0) {
		throw new Error(
			install.stderr.trim() || install.stdout.trim() || "Riot installer exited unsuccessfully",
		);
	}

	const installedBinary = managedRiotBinaryPath(context);
	if (!(await exists(installedBinary))) {
		throw new Error("Riot installer finished without producing a managed riot binary");
	}

	const metadata = await readInstalledReleaseMetadata(context);
	if (metadata) {
		return metadata;
	}

	const version = await runCommand(installedBinary, ["--version"]);
	const releaseId = version.stdout.trim() || "latest";
	return {
		release_id: releaseId,
		build_sha: "unknown",
	};
};

interface EnsureRiotOptions {
	prompt?: boolean;
}

export const ensureRiotAvailable = async (
	context: vscode.ExtensionContext,
	options: EnsureRiotOptions = {},
): Promise<string | undefined> => {
	const riot = await resolveRiotCommand(context);
	try {
		const result = await runCommand(riot, ["--version"]);
		if (result.code === 0) {
			return riot;
		}
	} catch {
		// fall through
	}

	if (options.prompt === false) {
		return undefined;
	}

	const choice = await vscode.window.showWarningMessage(
		"Riot is not available. Install it for this extension?",
		"Install Riot",
	);

	if (choice !== "Install Riot") {
		return undefined;
	}

	const metadata = await vscode.window.withProgress(
		{
			location: vscode.ProgressLocation.Notification,
			title: "Installing Riot",
		},
		async () => installManagedRiot(context),
	);

	vscode.window.showInformationMessage(`Installed Riot ${metadata.release_id}.`);
	return managedRiotBinaryPath(context);
};
