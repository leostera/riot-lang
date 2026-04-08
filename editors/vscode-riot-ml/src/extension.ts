import * as path from "node:path";
import * as vscode from "vscode";
import { RiotEditorFeatures } from "./editor_features";
import { registerFormatOnSave } from "./format";
import {
	currentReleaseMetadata,
	ensureRiotAvailable,
	installManagedRiot,
	isManagedRiotCommand,
	type RiotRunnableBinary,
	listRunnableBinaries,
	latestReleaseMetadata,
	packageCommandTargetFor,
	quote,
	readRiotInfo,
	resolveRiotCommand,
	resolvedRiotVersion,
	runRiot,
	sameReleaseIdentity,
} from "./riot";
import { RiotTaskProvider, runScopedTask, runWorkspaceTask } from "./tasks";
import { RiotBenchmarkController } from "./benchmarking";
import { RiotBenchmarkResultsView } from "./benchmark_results";
import { RiotTestController } from "./testing";
import { registerLanguageModelTools } from "./chat_tools";

type DependencyCommand = "add" | "rm";

const activeOcamlDocument = (): vscode.TextDocument | undefined => {
	const document = vscode.window.activeTextEditor?.document;
	if (!document || document.uri.scheme !== "file") {
		return undefined;
	}

	if (!document.uri.fsPath.endsWith(".ml") && !document.uri.fsPath.endsWith(".mli")) {
		return undefined;
	}

	return document;
};

const activeFileUri = (): vscode.Uri | undefined => {
	const uri = vscode.window.activeTextEditor?.document.uri;
	if (!uri || uri.scheme !== "file") {
		return undefined;
	}

	return uri;
};

const isRiotManifestDocument = (document: vscode.TextDocument): boolean =>
	document.uri.scheme === "file" && path.basename(document.uri.fsPath) === "riot.toml";

const dependencyScopeChoices = [
	{
		label: "Runtime dependency",
		description: "Write into [dependencies]",
		args: [] as string[],
		scopeLabel: "runtime",
	},
	{
		label: "Dev dependency",
		description: "Write into [dev-dependencies]",
		args: ["--dev"],
		scopeLabel: "dev",
	},
	{
		label: "Build dependency",
		description: "Write into [build-dependencies]",
		args: ["--build"],
		scopeLabel: "build",
	},
];

const commandLabel = (command: DependencyCommand): string =>
	command === "add" ? "add" : "remove";

const dependencyPrompt = (command: DependencyCommand): string =>
	command === "add"
		? "Dependency spec to add"
		: "Dependency name to remove";

const dependencyPlaceholder = (command: DependencyCommand): string =>
	command === "add"
		? "std, std@^1.0.0, ../local-pkg, github.com/owner/repo"
		: "std";

type RunnableKind = "binary" | "example";

const runnableKind = (binary: RiotRunnableBinary): RunnableKind =>
	binary.kind;

const sortRunnables = (
	binaries: RiotRunnableBinary[],
	activePackageName?: string,
): RiotRunnableBinary[] =>
	binaries.slice().sort((left, right) => {
		const leftActive = left.packageName === activePackageName ? 0 : 1;
		const rightActive = right.packageName === activePackageName ? 0 : 1;
		if (leftActive !== rightActive) {
			return leftActive - rightActive;
		}

		const leftKind = runnableKind(left);
		const rightKind = runnableKind(right);
		if (leftKind !== rightKind) {
			return leftKind.localeCompare(rightKind);
		}

		return left.selector.localeCompare(right.selector);
	});

const appendCommandRun = (
	output: vscode.OutputChannel,
	riot: string,
	args: string[],
	cwd: string,
	result: { stdout: string; stderr: string },
): void => {
	output.appendLine(`$ (cd ${quote(cwd)} && ${[quote(riot), ...args.map(quote)].join(" ")})`);

	if (result.stdout.trim() !== "") {
		output.appendLine(result.stdout.trimEnd());
	}

	if (result.stderr.trim() !== "") {
		output.appendLine(result.stderr.trimEnd());
	}

	output.appendLine("");
};

const commandFailureMessage = (
	command: DependencyCommand,
	dependency: string,
	result: { stdout: string; stderr: string },
): string => {
	const stderr = result.stderr.trim();
	const stdout = result.stdout.trim();

	if (stderr !== "") {
		return stderr;
	}

	if (stdout !== "") {
		return stdout;
	}

	return `Failed to ${commandLabel(command)} ${dependency}.`;
};

const runCommandFailureMessage = (result: { stdout: string; stderr: string }): string => {
	const stderr = result.stderr.trim();
	if (stderr !== "") {
		return stderr;
	}

	const stdout = result.stdout.trim();
	if (stdout !== "") {
		return stdout;
	}

	return "Riot command failed.";
};

const refreshProjectContext = async (context: vscode.ExtensionContext): Promise<void> => {
	const candidates: vscode.Uri[] = [];
	const active = activeFileUri();
	if (active) {
		candidates.push(active);
	}

	for (const folder of vscode.workspace.workspaceFolders ?? []) {
		candidates.push(folder.uri);
	}

	let inRiotProject = false;
	for (const candidate of candidates) {
		if (await readRiotInfo(context, candidate)) {
			inRiotProject = true;
			break;
		}
	}

	await vscode.commands.executeCommand("setContext", "inRiotProject", inRiotProject);
};

const runDependencyCommand = async (
	context: vscode.ExtensionContext,
	output: vscode.OutputChannel,
	command: DependencyCommand,
): Promise<void> => {
	if (!(await ensureRiotAvailable(context))) {
		return;
	}

	const dependency = await vscode.window.showInputBox({
		prompt: dependencyPrompt(command),
		placeHolder: dependencyPlaceholder(command),
		ignoreFocusOut: true,
		validateInput: (value) => value.trim() === "" ? "Dependency cannot be empty" : undefined,
	});
	if (!dependency) {
		return;
	}

	const scope = await vscode.window.showQuickPick(dependencyScopeChoices, {
		title: `Riot: ${command === "add" ? "Add" : "Remove"} Package`,
		placeHolder: "Choose which dependency section to edit",
		ignoreFocusOut: true,
	});
	if (!scope) {
		return;
	}

	const target = await packageCommandTargetFor(context, activeFileUri());
	if (!target) {
		void vscode.window.showWarningMessage(
			`Open a Riot workspace to ${commandLabel(command)} dependencies.`,
		);
		return;
	}

	const args = [command, "--json", ...scope.args, ...target.args, dependency.trim()];
	const riot = await resolveRiotCommand(context);
	const result = await vscode.window.withProgress(
		{
			location: vscode.ProgressLocation.Notification,
			title: `Riot: ${command === "add" ? "Adding" : "Removing"} dependency`,
		},
		async () => runRiot(context, args, { cwd: target.cwd.fsPath }),
	);

	appendCommandRun(output, riot, args, target.cwd.fsPath, result);

	if (result.code !== 0) {
		const showOutput = "Show Output";
		void vscode.window
			.showErrorMessage(commandFailureMessage(command, dependency, result), showOutput)
			.then((selection) => {
				if (selection === showOutput) {
					output.show(true);
				}
			});
		return;
	}

	void vscode.window.showInformationMessage(
		`${command === "add" ? "Added" : "Removed"} ${dependency.trim()} in ${target.label} (${scope.scopeLabel}).`,
	);
};

const startupStatus = async (
	context: vscode.ExtensionContext,
	editorFeatures: RiotEditorFeatures,
): Promise<void> => {
	const resolved = await resolvedRiotVersion(context);
	if (!resolved) {
		void vscode.window.showWarningMessage("RiotML\nCould not find a Riot installation.");
		return;
	}

	void vscode.window.showInformationMessage(`RiotML\nFound ${resolved.version} installed.`);

	try {
		const latest = await latestReleaseMetadata();
		const current = await currentReleaseMetadata(context);
		if (!current) {
			return;
		}

		if (sameReleaseIdentity(current, latest)) {
			return;
		}

		const upgradeLabel = "Upgrade";
		const message = "RiotML\nNew version available!";
		void vscode.window.showInformationMessage(message, upgradeLabel).then(async (selection) => {
			if (selection !== upgradeLabel) {
				return;
			}

			const metadata = await vscode.window.withProgress(
				{
					location: vscode.ProgressLocation.Notification,
					title: "Installing Riot",
				},
				async () => installManagedRiot(context),
			);
			await editorFeatures.restartLanguageServer();

			const wasManaged = await isManagedRiotCommand(context, resolved.command);
			const installedMessage = wasManaged
				? `RiotML\nInstalled Riot ${metadata.release_id} (${metadata.build_sha}).`
				: `RiotML\nInstalled Riot ${metadata.release_id} (${metadata.build_sha}) for RiotML.`;
			void vscode.window.showInformationMessage(installedMessage);
		});
	} catch (error) {
		const message = error instanceof Error ? error.message : String(error);
		void vscode.window.showWarningMessage(`RiotML\nCould not check for upgrades: ${message}`);
	}
};

const runRunnable = async (
	context: vscode.ExtensionContext,
	commandOutput: vscode.OutputChannel,
	kind?: RunnableKind,
): Promise<void> => {
	if (!(await ensureRiotAvailable(context))) {
		return;
	}

	const targetUri = activeFileUri();
	const listing = await vscode.window.withProgress(
		{
			location: vscode.ProgressLocation.Notification,
			title: "Riot: Discovering runnable binaries",
		},
		async () => listRunnableBinaries(context, targetUri),
	);
	if (!listing) {
		void vscode.window.showWarningMessage("Open a Riot workspace to run a binary.");
		return;
	}

	const riot = await resolveRiotCommand(context);
	appendCommandRun(commandOutput, riot, [
		"run",
		"--list",
		"--json",
	], listing.root.fsPath, listing.result);

	if (listing.result.code !== 0) {
		const showOutput = "Show Output";
		void vscode.window.showErrorMessage(runCommandFailureMessage(listing.result), showOutput).then((selection) => {
			if (selection === showOutput) {
				commandOutput.show(true);
			}
		});
		return;
	}

	const filtered = sortRunnables(listing.binaries, listing.activePackage?.name)
		.filter((binary) => kind === undefined || runnableKind(binary) === kind);
	if (filtered.length === 0) {
		const label = kind === "example" ? "examples" : kind === "binary" ? "binaries" : "runnables";
		void vscode.window.showInformationMessage(`No Riot ${label} found in the current workspace.`);
		return;
	}

	const selected = filtered.length === 1
		? filtered[0]
		: (await vscode.window.showQuickPick(
			filtered.map((binary) => ({
				label: binary.binaryName,
				description: binary.packageName,
				detail: `${runnableKind(binary)}: ${binary.selector} (${binary.path})`,
				binary,
			})),
			{
				title: kind === "example" ? "Riot: Run Example" : kind === "binary" ? "Riot: Run Binary" : "Riot: Run",
				placeHolder: kind === "example"
					? "Choose an example to run"
					: kind === "binary"
						? "Choose a binary to run"
						: "Choose a runnable to run",
				ignoreFocusOut: true,
			},
		))?.binary;
	if (!selected) {
		return;
	}

	const terminal = vscode.window.createTerminal({
		name: `Riot Run: ${selected.binaryName}`,
		cwd: listing.root.fsPath,
	});
	terminal.show(true);
	terminal.sendText(
		`${quote(riot)} run ${quote(selected.selector)}`,
		true,
	);
};

export function activate(context: vscode.ExtensionContext) {
	const commandOutput = vscode.window.createOutputChannel("Riot Commands");
	const extensionOutput = vscode.window.createOutputChannel("Riot Extension");
	const lspOutput = vscode.window.createOutputChannel("Riot LSP");
	const benchmarkOutput = vscode.window.createOutputChannel("Riot Benchmarks");
	const editorFeatures = new RiotEditorFeatures(context, extensionOutput, lspOutput);
	const benchmarkResultsView = new RiotBenchmarkResultsView(context);
	const testController = new RiotTestController(context, commandOutput);
	const benchmarkController = new RiotBenchmarkController(context, benchmarkOutput, benchmarkResultsView);
	void editorFeatures.start();
	void refreshProjectContext(context);

	context.subscriptions.push(
		editorFeatures,
		commandOutput,
		extensionOutput,
		lspOutput,
		benchmarkOutput,
		benchmarkResultsView,
		testController,
		benchmarkController,
		registerFormatOnSave(),
		...registerLanguageModelTools(context, commandOutput),
		vscode.window.registerWebviewViewProvider("riotBenchmarkResults", benchmarkResultsView),
		vscode.tasks.registerTaskProvider("riot", new RiotTaskProvider(context)),
		vscode.commands.registerCommand("riot.install", async () => {
			const metadata = await vscode.window.withProgress(
				{
					location: vscode.ProgressLocation.Notification,
					title: "Installing Riot",
				},
				async () => installManagedRiot(context),
			);
			await editorFeatures.restartLanguageServer();
			await refreshProjectContext(context);

			void vscode.window.showInformationMessage(
				`Installed Riot ${metadata.release_id} (${metadata.build_sha}).`,
			);
		}),
		vscode.commands.registerCommand("riot.startLanguageServer", async () => {
			await editorFeatures.startLanguageServer();
		}),
		vscode.commands.registerCommand("riot.stopLanguageServer", async () => {
			await editorFeatures.stopLanguageServer();
		}),
		vscode.commands.registerCommand("riot.restartLanguageServer", async () => {
			await editorFeatures.restartLanguageServer();
		}),
		vscode.commands.registerCommand("riot.showLanguageServerOutput", () => {
			editorFeatures.showLanguageServerOutput();
		}),
		vscode.commands.registerCommand("riot.showExtensionOutput", () => {
			editorFeatures.showExtensionOutput();
		}),
		vscode.commands.registerCommand("riot.showCommandOutput", () => {
			commandOutput.show(true);
		}),
		vscode.commands.registerCommand("riot.buildWorkspace", async () => {
			await runWorkspaceTask(context, "build", activeOcamlDocument()?.uri);
		}),
		vscode.commands.registerCommand("riot.testWorkspace", async () => {
			await testController.runWorkspaceFromCommand(activeFileUri());
		}),
		vscode.commands.registerCommand("riot.checkWorkspace", async () => {
			await runScopedTask(context, "check", activeFileUri());
		}),
		vscode.commands.registerCommand("riot.run", async () => {
			await runRunnable(context, commandOutput);
		}),
		vscode.commands.registerCommand("riot.runBinary", async () => {
			await runRunnable(context, commandOutput, "binary");
		}),
		vscode.commands.registerCommand("riot.runExample", async () => {
			await runRunnable(context, commandOutput, "example");
		}),
		vscode.commands.registerCommand("riot.formatDocument", async () => {
			if (!(await ensureRiotAvailable(context))) {
				return;
			}

			await vscode.commands.executeCommand("editor.action.formatDocument");
		}),
		vscode.commands.registerCommand("riot.refreshDiagnostics", async () => {
			const document = activeOcamlDocument();
			if (!document) {
				return;
			}

			await editorFeatures.refreshDiagnostics(document);
		}),
		vscode.commands.registerCommand("riot.addPackage", async () => {
			await runDependencyCommand(context, commandOutput, "add");
		}),
		vscode.commands.registerCommand("riot.removePackage", async () => {
			await runDependencyCommand(context, commandOutput, "rm");
		}),
		vscode.workspace.onDidChangeConfiguration((event) => {
			if (event.affectsConfiguration("riot.path")) {
				void editorFeatures.handleRiotPathChange();
			}
		}),
		vscode.window.onDidChangeActiveTextEditor(() => {
			void refreshProjectContext(context);
		}),
		vscode.workspace.onDidChangeWorkspaceFolders(() => {
			void refreshProjectContext(context);
		}),
		vscode.workspace.onDidSaveTextDocument((document) => {
			if (isRiotManifestDocument(document)) {
				void refreshProjectContext(context);
			}
		}),
	);

	void startupStatus(context, editorFeatures);
}

export function deactivate() {
	return undefined;
}
