import * as vscode from "vscode";
import { RiotEditorFeatures, ocamlDocumentSelector } from "./editor_features";
import { registerFormatOnSave } from "./format";
import {
	currentReleaseMetadata,
	ensureRiotAvailable,
	installManagedRiot,
	isManagedRiotCommand,
	latestReleaseMetadata,
	packageCommandTargetFor,
	quote,
	resolveRiotCommand,
	resolvedRiotVersion,
	runRiot,
	sameReleaseIdentity,
} from "./riot";
import { RiotTaskProvider, runWorkspaceTask } from "./tasks";

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

	const target = await packageCommandTargetFor(activeFileUri());
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
			await editorFeatures.restart();

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

export function activate(context: vscode.ExtensionContext) {
	const editorFeatures = new RiotEditorFeatures(context);
	const output = vscode.window.createOutputChannel("Riot ML");
	void editorFeatures.start();

	context.subscriptions.push(
		editorFeatures,
		output,
		registerFormatOnSave(),
		vscode.tasks.registerTaskProvider("riot", new RiotTaskProvider(context)),
		vscode.commands.registerCommand("riot.install", async () => {
			const metadata = await vscode.window.withProgress(
				{
					location: vscode.ProgressLocation.Notification,
					title: "Installing Riot",
				},
				async () => installManagedRiot(context),
			);
			await editorFeatures.restart();

			void vscode.window.showInformationMessage(
				`Installed Riot ${metadata.release_id} (${metadata.build_sha}).`,
			);
		}),
		vscode.commands.registerCommand("riot.buildWorkspace", async () => {
			await runWorkspaceTask(context, "build", activeOcamlDocument()?.uri);
		}),
		vscode.commands.registerCommand("riot.testWorkspace", async () => {
			await runWorkspaceTask(context, "test", activeOcamlDocument()?.uri);
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
			await runDependencyCommand(context, output, "add");
		}),
		vscode.commands.registerCommand("riot.removePackage", async () => {
			await runDependencyCommand(context, output, "rm");
		}),
		vscode.workspace.onDidChangeConfiguration((event) => {
			if (event.affectsConfiguration("riot.path")) {
				void editorFeatures.restart();
			}
		}),
	);

	void startupStatus(context, editorFeatures);
}

export function deactivate() {
	return undefined;
}
