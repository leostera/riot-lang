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

export interface RiotManifestInfo {
	root: vscode.Uri;
	kind: "package" | "workspace";
	packageName?: string;
}

export interface RiotManifestBinary {
	name: string;
	path: string;
}

export interface RiotManifest extends RiotManifestInfo {
	bins: RiotManifestBinary[];
}

export interface RiotInfoPackage {
	name: string;
	root: vscode.Uri;
	relativePath?: string;
	manifestPath?: vscode.Uri;
	manifest?: JsonObject | null;
}

export interface RiotWorkspaceInfo {
	kind: "package" | "workspace";
	root: vscode.Uri;
	name?: string;
	targetDirRoot?: vscode.Uri;
	manifestPath?: vscode.Uri;
	manifest?: JsonObject | null;
	packages: RiotInfoPackage[];
}

export interface RiotRunnableBinary {
	kind: "binary" | "example";
	packageName: string;
	binaryName: string;
	path: string;
	selector: string;
}

export interface PackageCommandTarget {
	cwd: vscode.Uri;
	args: string[];
	label: string;
}

export interface RunCommandOptions {
	cwd?: string;
	env?: NodeJS.ProcessEnv;
	stdin?: string;
	cancellation?: vscode.CancellationToken;
	onStdoutLine?: (line: string) => void;
	onStderrLine?: (line: string) => void;
}

const defaultInstallUrl = "https://get.riot.ml";
const defaultLatestMetadataUrl = "https://api.pkgs.ml/v1/riot/latest.json";

export const isOcamlUri = (uri: vscode.Uri): boolean => {
	return uri.scheme === "file" && (uri.fsPath.endsWith(".ml") || uri.fsPath.endsWith(".mli"));
};

export const quote = (value: string): string => {
	return JSON.stringify(value);
};

export const stripAnsi = (value: string): string =>
	value
		.replace(/\u001B\[[0-?]*[ -/]*[@-~]/g, "")
		.replace(/\[(?:\d{1,3}(?:;\d{1,3})*)m/g, "");

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

const streamText = (
	buffer: string,
	chunk: string,
	onLine?: (line: string) => void,
): string => {
	if (!onLine) {
		return buffer + chunk;
	}

	let pending = buffer + chunk;
	while (true) {
		const newlineIndex = pending.indexOf("\n");
		if (newlineIndex < 0) {
			return pending;
		}

		let line = pending.slice(0, newlineIndex);
		if (line.endsWith("\r")) {
			line = line.slice(0, -1);
		}
		onLine(line);
		pending = pending.slice(newlineIndex + 1);
	}
};

const flushText = (buffer: string, onLine?: (line: string) => void): void => {
	if (buffer.length === 0 || !onLine) {
		return;
	}

	onLine(buffer.endsWith("\r") ? buffer.slice(0, -1) : buffer);
};

export const runCommandStreaming = async (
	command: string,
	args: string[],
	options: RunCommandOptions = {},
): Promise<CommandResult> =>
	new Promise((resolve, reject) => {
		const supportsProcessGroups = process.platform !== "win32";
		const child = spawn(command, args, {
			cwd: options.cwd,
			env: { ...process.env, ...options.env },
			stdio: ["pipe", "pipe", "pipe"],
			detached: supportsProcessGroups,
		});

		let stdout = "";
		let stderr = "";
		let stdoutBuffer = "";
		let stderrBuffer = "";

		child.stdout.setEncoding("utf8");
		child.stderr.setEncoding("utf8");

		child.stdout.on("data", (chunk: string) => {
			stdout += chunk;
			stdoutBuffer = streamText(stdoutBuffer, chunk, options.onStdoutLine);
		});

		child.stderr.on("data", (chunk: string) => {
			stderr += chunk;
			stderrBuffer = streamText(stderrBuffer, chunk, options.onStderrLine);
		});

		child.on("error", (error) => {
			cancellationDisposable?.dispose();
			reject(error);
		});

		child.on("close", (code) => {
			cancellationDisposable?.dispose();
			flushText(stdoutBuffer, options.onStdoutLine);
			flushText(stderrBuffer, options.onStderrLine);
			resolve({
				code: code ?? 1,
				stdout,
				stderr,
			});
		});

		const cancelChild = (): void => {
			if (supportsProcessGroups && typeof child.pid === "number") {
				try {
					process.kill(-child.pid, "SIGTERM");
				} catch {
					child.kill();
				}

				setTimeout(() => {
					try {
						process.kill(-child.pid!, "SIGKILL");
					} catch {
						// Ignore missing-process races.
					}
				}, 1500);
				return;
			}

			child.kill();
		};

		const cancellationDisposable = options.cancellation?.onCancellationRequested(() => {
			cancelChild();
		});

		child.stdin.setDefaultEncoding("utf8");
		child.stdin.end(options.stdin ?? "");
	});

export const runCommand = async (
	command: string,
	args: string[],
	options: RunCommandOptions = {},
): Promise<CommandResult> => runCommandStreaming(command, args, options);

export const runRiot = async (
	context: vscode.ExtensionContext,
	args: string[],
	options: RunCommandOptions = {},
): Promise<CommandResult> => {
	const riot = await resolveRiotCommand(context);
	return runCommand(riot, args, options);
};

export const runRiotStreaming = async (
	context: vscode.ExtensionContext,
	args: string[],
	options: RunCommandOptions = {},
): Promise<CommandResult> => {
	const riot = await resolveRiotCommand(context);
	return runCommandStreaming(riot, args, options);
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

export const findJsonLineByType = (stdout: string, type: string): JsonObject | undefined => {
	const parsed = parseJsonLines(stdout);
	for (let idx = parsed.length - 1; idx >= 0; idx -= 1) {
		const item = parsed[idx];
		if (item.type === type) {
			return item;
		}
	}

	return undefined;
};

const jsonStringField = (value: Json | undefined): string | undefined =>
	typeof value === "string" ? value : undefined;

const jsonObjectField = (value: Json | undefined): JsonObject | undefined =>
	value !== null && value !== undefined && typeof value === "object" && !Array.isArray(value)
		? value as JsonObject
		: undefined;

const jsonArrayField = (value: Json | undefined): Json[] =>
	Array.isArray(value) ? value : [];

const inferRunnableKind = (binaryPath: string): "binary" | "example" =>
	/(^|\/)examples\//.test(binaryPath) ? "example" : "binary";

const uriField = (value: Json | undefined): vscode.Uri | undefined => {
	const filePath = jsonStringField(value);
	return filePath ? vscode.Uri.file(filePath) : undefined;
};

const parseRiotInfoPackage = (value: Json): RiotInfoPackage | undefined => {
	const object = jsonObjectField(value);
	if (!object) {
		return undefined;
	}

	const name = jsonStringField(object.name);
	const root = uriField(object.root);
	if (!name || !root) {
		return undefined;
	}

	return {
		name,
		root,
		relativePath: jsonStringField(object.relative_path),
		manifestPath: uriField(object.manifest_path),
		manifest: jsonObjectField(object.manifest) ?? null,
	};
};

const parseRiotWorkspaceInfo = (value: JsonObject): RiotWorkspaceInfo | undefined => {
	if (jsonStringField(value.type) !== "workspace_info") {
		return undefined;
	}

	const kind = jsonStringField(value.kind);
	const root = uriField(value.root);
	if ((kind !== "workspace" && kind !== "package") || !root) {
		return undefined;
	}

	return {
		kind,
		root,
		name: jsonStringField(value.name),
		targetDirRoot: uriField(value.target_dir_root),
		manifestPath: uriField(value.manifest_path),
		manifest: jsonObjectField(value.manifest) ?? null,
		packages: jsonArrayField(value.packages)
			.map(parseRiotInfoPackage)
			.filter((pkg): pkg is RiotInfoPackage => pkg !== undefined),
	};
};

const packageContainsUri = (pkg: RiotInfoPackage, uri: vscode.Uri): boolean => {
	if (uri.scheme !== "file") {
		return false;
	}

	const relativePath = path.relative(pkg.root.fsPath, uri.fsPath);
	return relativePath === ""
		|| (!relativePath.startsWith("..") && !path.isAbsolute(relativePath));
};

const infoLookupCwd = (uri?: vscode.Uri): string | undefined => {
	if (uri?.scheme === "file") {
		const basename = path.basename(uri.fsPath);
		if (basename === "riot.toml" || uri.fsPath.endsWith(".ml") || uri.fsPath.endsWith(".mli")) {
			return path.dirname(uri.fsPath);
		}

		return uri.fsPath;
	}

	return vscode.workspace.workspaceFolders?.[0]?.uri.fsPath;
};

export const readRiotInfo = async (
	context: vscode.ExtensionContext,
	uri?: vscode.Uri,
): Promise<RiotWorkspaceInfo | undefined> => {
	const cwd = infoLookupCwd(uri);
	if (!cwd) {
		return undefined;
	}

	const result = await runRiot(context, ["info", "--json"], { cwd });
	const payload = findJsonLineByType(result.stdout, "workspace_info");
	if (!payload) {
		return undefined;
	}

	return parseRiotWorkspaceInfo(payload);
};

export const packageForUri = (
	info: RiotWorkspaceInfo,
	uri?: vscode.Uri,
): RiotInfoPackage | undefined => {
	if (!uri) {
		return undefined;
	}

	return info.packages
		.filter((pkg) => packageContainsUri(pkg, uri))
		.sort((left, right) => right.root.fsPath.length - left.root.fsPath.length)[0];
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

const parseManifestInfo = (source: string): Omit<RiotManifest, "root"> | undefined => {
	let inPackage = false;
	let sawWorkspace = false;
	let packageName: string | undefined;
	let inBin = false;
	let currentBinName: string | undefined;
	let currentBinPath: string | undefined;
	const bins: RiotManifestBinary[] = [];

	const flushBin = (): void => {
		if (currentBinName && currentBinPath) {
			bins.push({
				name: currentBinName,
				path: currentBinPath,
			});
		}

		currentBinName = undefined;
		currentBinPath = undefined;
	};

	for (const rawLine of source.split(/\r?\n/)) {
		const line = rawLine.trim();

		if (line === "[package]") {
			inPackage = true;
			inBin = false;
			flushBin();
			continue;
		}

		if (line === "[workspace]") {
			inPackage = false;
			inBin = false;
			flushBin();
			sawWorkspace = true;
			continue;
		}

		if (line === "[[bin]]") {
			inPackage = false;
			inBin = true;
			flushBin();
			continue;
		}

		if (line.startsWith("[")) {
			inPackage = false;
			inBin = false;
			flushBin();
			continue;
		}

		if (!inPackage && !inBin) {
			continue;
		}

		const match = /^name\s*=\s*"([^"]+)"$/.exec(line);
		if (match && inPackage) {
			packageName = match[1];
			continue;
		}

		if (match && inBin) {
			currentBinName = match[1];
			continue;
		}

		const pathMatch = /^path\s*=\s*"([^"]+)"$/.exec(line);
		if (pathMatch && inBin) {
			currentBinPath = pathMatch[1];
		}
	}

	flushBin();

	if (packageName) {
		return {
			kind: "package",
			packageName,
			bins,
		};
	}

	if (sawWorkspace) {
		return {
			kind: "workspace",
			bins,
		};
	}

	return undefined;
};

export const readRiotManifest = async (root: vscode.Uri): Promise<RiotManifest | undefined> => {
	try {
		const manifest = await fs.readFile(path.join(root.fsPath, "riot.toml"), "utf8");
		const info = parseManifestInfo(manifest);
		if (!info) {
			return undefined;
		}

		return {
			root,
			...info,
		};
	} catch {
		return undefined;
	}
};

export const nearestRiotManifestInfo = async (
	uri: vscode.Uri,
): Promise<RiotManifestInfo | undefined> => {
	if (uri.scheme !== "file") {
		return undefined;
	}

	let current = path.dirname(uri.fsPath);
	while (true) {
		const manifestPath = path.join(current, "riot.toml");
		if (await exists(manifestPath)) {
			return readRiotManifest(vscode.Uri.file(current));
		}

		const parent = path.dirname(current);
		if (parent === current) {
			return undefined;
		}

		current = parent;
	}
};

export const packageCommandTargetFor = async (
	context: vscode.ExtensionContext,
	uri?: vscode.Uri,
): Promise<PackageCommandTarget | undefined> => {
	if (uri) {
		const info = await readRiotInfo(context, uri);
		if (info) {
			const pkg = packageForUri(info, uri);
			if (pkg) {
				return {
					cwd: pkg.root,
					args: [],
					label: `package ${pkg.name}`,
				};
			}

			return {
				cwd: info.root,
				args: ["--workspace"],
				label: "workspace",
			};
		}

		const manifest = await nearestRiotManifestInfo(uri);
		if (manifest?.kind === "package" && manifest.packageName) {
			return {
				cwd: manifest.root,
				args: [],
				label: `package ${manifest.packageName}`,
			};
		}

		if (manifest?.kind === "workspace") {
			return {
				cwd: manifest.root,
				args: ["--workspace"],
				label: "workspace",
			};
		}
	}

	const root = await workspaceRootFor(context, uri);
	if (!root) {
		return undefined;
	}

	return {
		cwd: root,
		args: ["--workspace"],
		label: "workspace",
	};
};

const parseRunnableBinary = (value: Json): RiotRunnableBinary | undefined => {
	const object = jsonObjectField(value);
	if (!object) {
		return undefined;
	}

	const kind = jsonStringField(object.kind);
	const packageName = jsonStringField(object.package);
	const binaryName = jsonStringField(object.binary);
	const selector = jsonStringField(object.selector);
	const binaryPath = jsonStringField(object.path);
	if (!packageName || !binaryName || !selector || !binaryPath) {
		return undefined;
	}

	return {
		kind: kind === "example" || kind === "binary" ? kind : inferRunnableKind(binaryPath),
		packageName,
		binaryName,
		path: binaryPath,
		selector,
	};
};

export const listRunnableBinaries = async (
	context: vscode.ExtensionContext,
	uri?: vscode.Uri,
): Promise<{
	root: vscode.Uri;
	activePackage?: RiotInfoPackage;
	binaries: RiotRunnableBinary[];
	result: CommandResult;
} | undefined> => {
	const info = await readRiotInfo(context, uri);
	const root = info?.root ?? await workspaceRootFor(context, uri);
	if (!root) {
		return undefined;
	}

	const activePackage = info ? packageForUri(info, uri) : undefined;
	const args = ["run", "--list", "--json"];

	const result = await runRiot(context, args, { cwd: root.fsPath });
	const payload = findJsonLineByType(result.stdout, "RunList");
	const binaries = payload
		? jsonArrayField(payload.binaries)
			.map(parseRunnableBinary)
			.filter((binary): binary is RiotRunnableBinary => binary !== undefined)
		: [];

	return {
		root,
		activePackage,
		binaries,
		result,
	};
};

export const workspaceRootFor = async (
	context: vscode.ExtensionContext,
	uri?: vscode.Uri,
): Promise<vscode.Uri | undefined> => {
	const info = await readRiotInfo(context, uri);
	if (info) {
		return info.root;
	}

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
