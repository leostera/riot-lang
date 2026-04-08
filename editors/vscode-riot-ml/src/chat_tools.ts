import * as path from "node:path";
import * as vscode from "vscode";
import {
	ensureRiotAvailable,
	listRunnableBinaries,
	packageCommandTargetFor,
	quote,
	readRiotInfo,
	resolveRiotCommand,
	runRiot,
	stripAnsi,
	workspaceRootFor,
} from "./riot";

type ToolScope = "workspace" | "package";
type RunnableKind = "all" | "binary" | "example";

interface WorkspaceInfoInput {
	path?: string;
}

interface ScopedCommandInput {
	path?: string;
	scope?: ToolScope;
}

interface ListRunnablesInput {
	path?: string;
	kind?: RunnableKind;
	limit?: number;
}

interface RunRunnableInput {
	path?: string;
	selector: string;
}

const defaultRunnableLimit = 100;

const activeFileUri = (): vscode.Uri | undefined => {
	const uri = vscode.window.activeTextEditor?.document.uri;
	return uri?.scheme === "file" ? uri : undefined;
};

const resolveInputUri = (inputPath?: string): vscode.Uri | undefined => {
	if (typeof inputPath === "string" && inputPath.trim() !== "") {
		return vscode.Uri.file(path.resolve(inputPath.trim()));
	}

	return activeFileUri() ?? vscode.workspace.workspaceFolders?.[0]?.uri;
};

const toolResult = (
	text: string,
	data?: unknown,
): vscode.LanguageModelToolResult => {
	const content: Array<vscode.LanguageModelTextPart | vscode.LanguageModelDataPart> = [
		new vscode.LanguageModelTextPart(text),
	];
	if (data !== undefined) {
		content.push(vscode.LanguageModelDataPart.json(data));
	}
	return new vscode.LanguageModelToolResult(content);
};

const appendCommandRun = (
	output: vscode.OutputChannel,
	command: string,
	args: string[],
	cwd: string,
	result?: { stdout: string; stderr: string },
): void => {
	output.appendLine(`$ (cd ${quote(cwd)} && ${[quote(command), ...args.map(quote)].join(" ")})`);
	if (result?.stdout.trim()) {
		output.appendLine(result.stdout.trimEnd());
	}
	if (result?.stderr.trim()) {
		output.appendLine(result.stderr.trimEnd());
	}
	output.appendLine("");
};

const cleanText = (value: string): string => stripAnsi(value).replace(/\r/g, "").trim();

const summarizeCommandResult = (result: { code: number; stdout: string; stderr: string }): string => {
	const stderr = cleanText(result.stderr);
	if (stderr !== "") {
		return stderr;
	}

	const stdout = cleanText(result.stdout);
	if (stdout !== "") {
		return stdout;
	}

	return result.code === 0 ? "Command completed successfully." : `Command failed with exit code ${result.code}.`;
};

const ensureRiot = async (context: vscode.ExtensionContext): Promise<void> => {
	const riot = await ensureRiotAvailable(context, { prompt: false });
	if (!riot) {
		throw new Error("Riot is not available in this VS Code session.");
	}
};

const resolveCommandTarget = async (
	context: vscode.ExtensionContext,
	scope: ToolScope,
	inputPath?: string,
): Promise<{ cwd: string; args: string[]; label: string }> => {
	const uri = resolveInputUri(inputPath);
	if (scope === "workspace") {
		const root = await workspaceRootFor(context, uri);
		if (!root) {
			throw new Error("Could not resolve a Riot workspace root.");
		}

		return {
			cwd: root.fsPath,
			args: [],
			label: "workspace",
		};
	}

	const target = await packageCommandTargetFor(context, uri);
	if (!target) {
		throw new Error("Could not resolve a Riot package or workspace target.");
	}

	return {
		cwd: target.cwd.fsPath,
		args: target.args,
		label: target.label,
	};
};

class RiotWorkspaceInfoTool implements vscode.LanguageModelTool<WorkspaceInfoInput> {
	constructor(private readonly context: vscode.ExtensionContext) {}

	prepareInvocation(
		options: vscode.LanguageModelToolInvocationPrepareOptions<WorkspaceInfoInput>,
	): vscode.PreparedToolInvocation {
		const uri = resolveInputUri(options.input.path);
		return {
			invocationMessage: uri
				? `Inspecting Riot workspace information for ${uri.fsPath}.`
				: "Inspecting Riot workspace information.",
		};
	}

	async invoke(
		options: vscode.LanguageModelToolInvocationOptions<WorkspaceInfoInput>,
	): Promise<vscode.LanguageModelToolResult> {
		await ensureRiot(this.context);
		const uri = resolveInputUri(options.input.path);
		const info = await readRiotInfo(this.context, uri);
		if (!info) {
			throw new Error("Could not resolve Riot workspace information.");
		}

		const data = {
			kind: info.kind,
			root: info.root.fsPath,
			name: info.name,
			packages: info.packages.map((pkg) => ({
				name: pkg.name,
				root: pkg.root.fsPath,
				relativePath: pkg.relativePath,
				manifestPath: pkg.manifestPath?.fsPath,
			})),
		};

		return toolResult(
			`Resolved Riot ${info.kind} at ${info.root.fsPath} with ${info.packages.length} package(s).`,
			data,
		);
	}
}

class RiotScopedCommandTool implements vscode.LanguageModelTool<ScopedCommandInput> {
	constructor(
		private readonly context: vscode.ExtensionContext,
		private readonly output: vscode.OutputChannel,
		private readonly command: "build" | "check",
	) {}

	prepareInvocation(
		options: vscode.LanguageModelToolInvocationPrepareOptions<ScopedCommandInput>,
	): vscode.PreparedToolInvocation {
		const scope = options.input.scope ?? "package";
		return {
			invocationMessage: `Running \`riot ${this.command}\` for the ${scope}.`,
		};
	}

	async invoke(
		options: vscode.LanguageModelToolInvocationOptions<ScopedCommandInput>,
	): Promise<vscode.LanguageModelToolResult> {
		await ensureRiot(this.context);
		const scope = options.input.scope ?? "package";
		const target = await resolveCommandTarget(this.context, scope, options.input.path);
		const riot = await resolveRiotCommand(this.context);
		const args = [this.command, ...target.args];
		const result = await runRiot(this.context, args, { cwd: target.cwd });
		appendCommandRun(this.output, riot, args, target.cwd, result);

		return toolResult(
			`Ran \`riot ${this.command}\` for ${target.label}. Exit code: ${result.code}. ${summarizeCommandResult(result)}`,
			{
				command: this.command,
				target: target.label,
				cwd: target.cwd,
				exitCode: result.code,
				stdout: cleanText(result.stdout),
				stderr: cleanText(result.stderr),
			},
		);
	}
}

class RiotListRunnablesTool implements vscode.LanguageModelTool<ListRunnablesInput> {
	constructor(private readonly context: vscode.ExtensionContext) {}

	prepareInvocation(
		options: vscode.LanguageModelToolInvocationPrepareOptions<ListRunnablesInput>,
	): vscode.PreparedToolInvocation {
		const kind = options.input.kind ?? "all";
		return {
			invocationMessage: `Listing Riot ${kind === "all" ? "runnables" : kind + "s"}.`,
		};
	}

	async invoke(
		options: vscode.LanguageModelToolInvocationOptions<ListRunnablesInput>,
	): Promise<vscode.LanguageModelToolResult> {
		await ensureRiot(this.context);
		const uri = resolveInputUri(options.input.path);
		const listing = await listRunnableBinaries(this.context, uri);
		if (!listing) {
			throw new Error("Could not resolve Riot runnables.");
		}

		const kind = options.input.kind ?? "all";
		const limit = Math.max(1, Math.min(options.input.limit ?? defaultRunnableLimit, 500));
		const runnables = listing.binaries
			.filter((binary) => kind === "all" || binary.kind === kind)
			.slice(0, limit)
			.map((binary) => ({
				kind: binary.kind,
				package: binary.packageName,
				binary: binary.binaryName,
				selector: binary.selector,
				path: binary.path,
			}));

		return toolResult(
			`Found ${runnables.length} Riot ${kind === "all" ? "runnable(s)" : kind + "(s)"} in ${listing.root.fsPath}.`,
			{
				root: listing.root.fsPath,
				activePackage: listing.activePackage?.name,
				total: runnables.length,
				runnables,
			},
		);
	}
}

class RiotRunRunnableTool implements vscode.LanguageModelTool<RunRunnableInput> {
	constructor(
		private readonly context: vscode.ExtensionContext,
		private readonly output: vscode.OutputChannel,
	) {}

	prepareInvocation(
		options: vscode.LanguageModelToolInvocationPrepareOptions<RunRunnableInput>,
	): vscode.PreparedToolInvocation {
		return {
			invocationMessage: `Launching Riot runnable \`${options.input.selector}\`.`,
			confirmationMessages: {
				title: "Run Riot runnable?",
				message: `Launch \`${options.input.selector}\` in a VS Code terminal.`,
			},
		};
	}

	async invoke(
		options: vscode.LanguageModelToolInvocationOptions<RunRunnableInput>,
	): Promise<vscode.LanguageModelToolResult> {
		await ensureRiot(this.context);
		const uri = resolveInputUri(options.input.path);
		const listing = await listRunnableBinaries(this.context, uri);
		if (!listing) {
			throw new Error("Could not resolve Riot runnables.");
		}

		const runnable = listing.binaries.find((binary) => binary.selector === options.input.selector);
		if (!runnable) {
			throw new Error(`Riot runnable \`${options.input.selector}\` was not found in the current workspace.`);
		}

		const riot = await resolveRiotCommand(this.context);
		const terminal = vscode.window.createTerminal({
			name: `Riot Run: ${runnable.binaryName}`,
			cwd: listing.root.fsPath,
		});
		terminal.show(true);
		terminal.sendText(`${quote(riot)} run ${quote(runnable.selector)}`, true);
		appendCommandRun(this.output, riot, ["run", runnable.selector], listing.root.fsPath);

		return toolResult(
			`Started Riot ${runnable.kind} \`${runnable.selector}\` in terminal \`${terminal.name}\`.`,
			{
				selector: runnable.selector,
				kind: runnable.kind,
				package: runnable.packageName,
				binary: runnable.binaryName,
				path: runnable.path,
				cwd: listing.root.fsPath,
				terminal: terminal.name,
			},
		);
	}
}

export const registerLanguageModelTools = (
	context: vscode.ExtensionContext,
	output: vscode.OutputChannel,
): vscode.Disposable[] => {
	return [
		vscode.lm.registerTool("riot_info_workspace", new RiotWorkspaceInfoTool(context)),
		vscode.lm.registerTool("riot_build", new RiotScopedCommandTool(context, output, "build")),
		vscode.lm.registerTool("riot_check", new RiotScopedCommandTool(context, output, "check")),
		vscode.lm.registerTool("riot_list_runnables", new RiotListRunnablesTool(context)),
		vscode.lm.registerTool("riot_run_runnable", new RiotRunRunnableTool(context, output)),
	];
};
