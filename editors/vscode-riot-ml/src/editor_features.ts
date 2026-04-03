import * as vscode from "vscode";
import {
	CloseAction,
	ErrorAction,
	LanguageClient,
	type LanguageClientOptions,
	RevealOutputChannelOn,
	type ServerOptions,
} from "vscode-languageclient/node";
import { RiotDiagnostics } from "./diagnostics";
import { RiotFormattingProvider } from "./format";
import {
	ensureRiotAvailable,
	resolveRiotCommand,
} from "./riot";

export const ocamlDocumentSelector: vscode.DocumentFilter[] = [
	{ scheme: "file", language: "riot-ocaml" },
];

const diagnosticsMiddleware: LanguageClientOptions["middleware"] = {
	handleDiagnostics(uri, diagnostics, next) {
		const config = vscode.workspace.getConfiguration("riot");
		if (!config.get<boolean>("diagnostics.enabled", true)) {
			next(uri, []);
			return;
		}

		const filtered = config.get<boolean>("diagnostics.runFix", true)
			? diagnostics
			: diagnostics.filter((diagnostic) => diagnostic.source !== "riot-fix");
		next(uri, filtered);
	},
};

export class RiotEditorFeatures implements vscode.Disposable {
	private languageClient: LanguageClient | undefined;
	private fallbackDiagnostics: RiotDiagnostics | undefined;
	private readonly fallbackDisposables: vscode.Disposable[] = [];
	private lspOutputChannel: vscode.OutputChannel | undefined;

	constructor(private readonly context: vscode.ExtensionContext) {}

	async start(): Promise<void> {
		await this.activateBestAvailableFeatures();
	}

	async restart(): Promise<void> {
		await this.disposeActiveFeatures();
		await this.activateBestAvailableFeatures();
	}

	async refreshDiagnostics(document: vscode.TextDocument): Promise<void> {
		if (this.fallbackDiagnostics) {
			await this.fallbackDiagnostics.refresh(document);
		}
	}

	async disposeAsync(): Promise<void> {
		await this.disposeActiveFeatures();
	}

	dispose(): void {
		void this.disposeAsync();
	}

	private async activateBestAvailableFeatures(): Promise<void> {
		if (await this.tryStartLanguageClient()) {
			return;
		}

		this.activateCliFallback();
	}

	private async tryStartLanguageClient(): Promise<boolean> {
		if (!(await ensureRiotAvailable(this.context, { prompt: false }))) {
			return false;
		}

		const riot = await resolveRiotCommand(this.context);
		const outputChannel = vscode.window.createOutputChannel("Riot LSP");
		const serverOptions: ServerOptions = {
			command: riot,
			args: ["lsp", "stdio"],
		};
		const clientOptions: LanguageClientOptions = {
			documentSelector: ocamlDocumentSelector as LanguageClientOptions["documentSelector"],
			diagnosticCollectionName: "riot",
			middleware: diagnosticsMiddleware,
			outputChannel,
			revealOutputChannelOn: RevealOutputChannelOn.Never,
			errorHandler: {
				error: () => ({ action: ErrorAction.Continue }),
				closed: () => ({ action: CloseAction.Restart }),
			},
		};

		const client = new LanguageClient("riot-lsp", "Riot LSP", serverOptions, clientOptions);

		try {
			await client.start();
			this.languageClient = client;
			this.lspOutputChannel = outputChannel;
			return true;
		} catch {
			outputChannel.dispose();
			return false;
		}
	}

	private activateCliFallback(): void {
		if (this.fallbackDiagnostics) {
			return;
		}

		const diagnostics = new RiotDiagnostics(this.context);
		const formatter = new RiotFormattingProvider(this.context, diagnostics);
		this.fallbackDiagnostics = diagnostics;
		this.fallbackDisposables.push(
			...diagnostics.register(),
			vscode.languages.registerDocumentFormattingEditProvider(
				ocamlDocumentSelector,
				formatter,
			),
		);
	}

	private async disposeActiveFeatures(): Promise<void> {
		if (this.languageClient) {
			const client = this.languageClient;
			this.languageClient = undefined;
			await client.stop();
		}

		if (this.lspOutputChannel) {
			this.lspOutputChannel.dispose();
			this.lspOutputChannel = undefined;
		}

		this.fallbackDiagnostics = undefined;
		while (this.fallbackDisposables.length > 0) {
			this.fallbackDisposables.pop()?.dispose();
		}
	}
}
