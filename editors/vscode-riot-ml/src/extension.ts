import * as vscode from "vscode";
import { RiotDiagnostics } from "./diagnostics";
import { RiotFormattingProvider, registerFormatOnSave } from "./format";
import {
	currentReleaseMetadata,
	ensureRiotAvailable,
	installManagedRiot,
	isManagedRiotCommand,
	latestReleaseMetadata,
	resolvedRiotVersion,
	sameReleaseIdentity,
} from "./riot";
import { RiotTaskProvider, runWorkspaceTask } from "./tasks";

const ocamlDocumentSelector: vscode.DocumentSelector = [
	{ scheme: "file", language: "riot-ocaml" },
];

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

const startupStatus = async (
	context: vscode.ExtensionContext,
	output: vscode.OutputChannel,
): Promise<void> => {
	const resolved = await resolvedRiotVersion(context);
	if (!resolved) {
		output.appendLine("Riot: not found in extension-managed storage or PATH.");
		return;
	}

	output.appendLine(`Riot command: ${resolved.command}`);
	output.appendLine(`Riot version: ${resolved.version}`);

	try {
		const latest = await latestReleaseMetadata();
		const current = await currentReleaseMetadata(context);
		if (!current) {
			output.appendLine(
				`Latest published Riot: ${latest.release_id} (${latest.build_sha}). Installed Riot version could not be parsed for upgrade checks.`,
			);
			return;
		}

		if (sameReleaseIdentity(current, latest)) {
			output.appendLine(`Riot is up to date: ${latest.release_id} (${latest.build_sha}).`);
			return;
		}

		output.appendLine(
			`Riot upgrade available: ${latest.release_id} (${latest.build_sha}) is newer than ${current.release_id} (${current.build_sha}).`,
		);
		if (await isManagedRiotCommand(context, resolved.command)) {
			void vscode.window
				.showInformationMessage(
					`Riot ${latest.release_id} is available. You're on ${current.release_id}.`,
					"Install Riot",
				)
				.then(async (selection) => {
					if (selection === "Install Riot") {
						const metadata = await vscode.window.withProgress(
							{
								location: vscode.ProgressLocation.Notification,
								title: "Installing Riot",
							},
							async () => installManagedRiot(context),
						);

						output.appendLine(`Installed Riot ${metadata.release_id} (${metadata.build_sha}).`);
						void vscode.window.showInformationMessage(
							`Installed Riot ${metadata.release_id} (${metadata.build_sha}).`,
						);
					}
				});
		}
	} catch (error) {
		const message = error instanceof Error ? error.message : String(error);
		output.appendLine(`Failed to check for Riot upgrades: ${message}`);
	}
};

export function activate(context: vscode.ExtensionContext) {
	const diagnostics = new RiotDiagnostics(context);
	const formatter = new RiotFormattingProvider(context, diagnostics);
	const output = vscode.window.createOutputChannel("Riot ML");

	context.subscriptions.push(
		output,
		...diagnostics.register(),
		vscode.languages.registerDocumentFormattingEditProvider(ocamlDocumentSelector, formatter),
		registerFormatOnSave(formatter),
		vscode.tasks.registerTaskProvider("riot", new RiotTaskProvider(context)),
		vscode.commands.registerCommand("riot.install", async () => {
			const metadata = await vscode.window.withProgress(
				{
					location: vscode.ProgressLocation.Notification,
					title: "Installing Riot",
				},
				async () => installManagedRiot(context),
			);

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

			await diagnostics.refresh(document);
		}),
	);

	void startupStatus(context, output);
}

export function deactivate() {}
