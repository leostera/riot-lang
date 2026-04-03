import * as vscode from "vscode";
import * as fs from "node:fs/promises";
import * as path from "node:path";
import * as os from "node:os";
import {
	ensureRiotAvailable,
	isOcamlUri,
	runRiot,
	workspaceRootFor,
} from "./riot";
import { RiotDiagnostics } from "./diagnostics";

const fullDocumentRange = (document: vscode.TextDocument): vscode.Range => {
	const start = new vscode.Position(0, 0);
	const end = document.lineCount === 0
		? start
		: document.lineAt(document.lineCount - 1).range.end;
	return new vscode.Range(start, end);
};

export class RiotFormattingProvider implements vscode.DocumentFormattingEditProvider {
	constructor(
		private readonly context: vscode.ExtensionContext,
		private readonly diagnostics: RiotDiagnostics,
	) {}

	async provideDocumentFormattingEdits(document: vscode.TextDocument): Promise<vscode.TextEdit[]> {
		if (!isOcamlUri(document.uri)) {
			return [];
		}

		if (!(await ensureRiotAvailable(this.context, { prompt: false }))) {
			return [];
		}

		const root = await workspaceRootFor(document.uri);
		const tempDir = await fs.mkdtemp(path.join(os.tmpdir(), "riot-vscode-format-"));
		const tempFile = path.join(
			tempDir,
			`document${path.extname(document.uri.fsPath) || ".ml"}`,
		);

		try {
			await fs.writeFile(tempFile, document.getText(), "utf8");
			const result = await runRiot(this.context, ["fmt", tempFile], { cwd: root?.fsPath });
			if (result.code !== 0) {
				void this.diagnostics.refresh(document);
				const message = result.stderr.trim() || result.stdout.trim() || "riot fmt failed";
				void vscode.window.showErrorMessage(message);
				return [];
			}

			const formatted = await fs.readFile(tempFile, "utf8");
			if (formatted === document.getText()) {
				return [];
			}

			return [vscode.TextEdit.replace(fullDocumentRange(document), formatted)];
		} finally {
			await fs.rm(tempDir, { recursive: true, force: true });
		}
	}
}

export const registerFormatOnSave = (
	_provider: RiotFormattingProvider,
): vscode.Disposable =>
	vscode.workspace.onWillSaveTextDocument((event) => {
		const document = event.document;
		if (!isOcamlUri(document.uri)) {
			return;
		}

		if (!vscode.workspace.getConfiguration("riot").get<boolean>("formatOnSave", true)) {
			return;
		}

		event.waitUntil(
			vscode.commands.executeCommand<vscode.TextEdit[]>(
				"vscode.executeFormatDocumentProvider",
				document.uri,
			).then(
				(edits) => edits ?? [],
				() => [],
			),
		);
	});
