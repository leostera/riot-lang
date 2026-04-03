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
	const diagnostics = new RiotDiagnostics(context);
	const formatter = new RiotFormattingProvider(context, diagnostics);

	context.subscriptions.push(
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

	void startupStatus(context);
}

export function deactivate() {}
