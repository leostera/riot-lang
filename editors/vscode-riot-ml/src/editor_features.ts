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
} from "./riot";

export const ocamlDocumentSelector: vscode.DocumentFilter[] = [
	{ scheme: "file", language: "riot-ocaml" },
];

type RiotEditorState = "starting" | "running" | "fallback" | "stopped" | "unavailable";

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

const timestamp = (): string => new Date().toLocaleTimeString();

const stateLabel = (state: RiotEditorState): string => {
	switch (state) {
		case "starting":
			return "$(loading~spin) Riot: starting";
		case "running":
			return "$(check) Riot: ready";
		case "fallback":
			return "$(warning) Riot: CLI fallback";
		case "stopped":
			return "$(circle-slash) Riot: stopped";
		case "unavailable":
			return "$(error) Riot: unavailable";
	}
};

const stateTooltip = (state: RiotEditorState): string => {
	switch (state) {
		case "starting":
			return "Riot language server is starting. Click to open Riot Extension output.";
		case "running":
			return "Riot language server is running. Click to open Riot Extension output.";
		case "fallback":
			return "Riot CLI fallback is active for formatting and diagnostics. Click to open Riot Extension output.";
		case "stopped":
			return "Riot language server is stopped. Use `Riot: Start Language Server` to start it.";
		case "unavailable":
			return "Riot is not available. Install Riot or set `riot.path`. Click to open Riot Extension output.";
	}
};

export class RiotEditorFeatures implements vscode.Disposable {
	private languageClient: LanguageClient | undefined;
	private fallbackDiagnostics: RiotDiagnostics | undefined;
	private readonly fallbackDisposables: vscode.Disposable[] = [];
	private readonly statusBarItem: vscode.StatusBarItem;
	private userStopped = false;
	private activationGeneration = 0;
	private disposed = false;
	private stoppingClient = false;

	constructor(
		private readonly context: vscode.ExtensionContext,
		private readonly extensionOutput: vscode.OutputChannel,
		private readonly lspOutput: vscode.OutputChannel,
	) {
		this.statusBarItem = vscode.window.createStatusBarItem(
			"riot.editorState",
			vscode.StatusBarAlignment.Left,
			10,
		);
		this.statusBarItem.command = "riot.showExtensionOutput";
		this.statusBarItem.show();
		this.updateState("starting");
	}

	async start(): Promise<void> {
		this.userStopped = false;
		await this.activateBestAvailableFeatures({
			promptOnMissingRiot: false,
			reason: "startup",
		});
	}

	async startLanguageServer(): Promise<void> {
		this.userStopped = false;
		await this.activateBestAvailableFeatures({
			promptOnMissingRiot: true,
			reason: "manual start",
		});
	}

	async stopLanguageServer(): Promise<void> {
		this.userStopped = true;
		this.log("Stopping Riot language server and CLI fallback.");
		await this.disposeActiveFeatures();
		this.updateState("stopped");
	}

	async restartLanguageServer(): Promise<void> {
		this.userStopped = false;
		this.log("Restarting Riot language server.");
		await this.activateBestAvailableFeatures({
			promptOnMissingRiot: true,
			reason: "manual restart",
		});
	}

	async handleRiotPathChange(): Promise<void> {
		this.log("Riot path configuration changed.");
		if (this.userStopped) {
			await this.disposeActiveFeatures();
			this.updateState("stopped");
			return;
		}

		await this.activateBestAvailableFeatures({
			promptOnMissingRiot: false,
			reason: "configuration change",
		});
	}

	showLanguageServerOutput(): void {
		this.lspOutput.show(true);
	}

	showExtensionOutput(): void {
		this.extensionOutput.show(true);
	}

	async refreshDiagnostics(document: vscode.TextDocument): Promise<void> {
		if (this.fallbackDiagnostics) {
			await this.fallbackDiagnostics.refresh(document);
		}
	}

	async disposeAsync(): Promise<void> {
		if (this.disposed) {
			return;
		}

		this.disposed = true;
		await this.disposeActiveFeatures();
		this.statusBarItem.dispose();
	}

	dispose(): void {
		void this.disposeAsync();
	}

	private log(message: string): void {
		this.extensionOutput.appendLine(`[${timestamp()}] ${message}`);
	}

	private updateState(state: RiotEditorState): void {
		this.statusBarItem.text = stateLabel(state);
		this.statusBarItem.tooltip = stateTooltip(state);
		void vscode.commands.executeCommand("setContext", "riotLspRunning", state === "running");
		void vscode.commands.executeCommand("setContext", "riotCliFallbackActive", state === "fallback");
	}

	private async activateBestAvailableFeatures(
		options: {
			promptOnMissingRiot: boolean;
			reason: string;
		},
	): Promise<void> {
		const generation = this.activationGeneration + 1;
		this.activationGeneration = generation;
		await this.disposeActiveFeatures();

		if (this.userStopped || this.disposed) {
			this.updateState("stopped");
			return;
		}

		this.updateState("starting");
		this.log(`Activating Riot editor features (${options.reason}).`);

		const riot = await ensureRiotAvailable(this.context, { prompt: options.promptOnMissingRiot });
		if (!riot) {
			this.log("Riot is unavailable; editor features remain disabled.");
			this.updateState("unavailable");
			return;
		}

		if (await this.tryStartLanguageClient(riot, generation)) {
			return;
		}

		this.log("Riot LSP failed to start; using CLI fallback.");
		this.activateCliFallback();
	}

	private async tryStartLanguageClient(riot: string, generation: number): Promise<boolean> {
		const serverOptions: ServerOptions = {
			command: riot,
			args: ["lsp", "stdio"],
		};
		const clientOptions: LanguageClientOptions = {
			documentSelector: ocamlDocumentSelector as LanguageClientOptions["documentSelector"],
			diagnosticCollectionName: "riot",
			middleware: diagnosticsMiddleware,
			outputChannel: this.lspOutput,
			revealOutputChannelOn: RevealOutputChannelOn.Never,
			errorHandler: {
				error: (error) => {
					this.log(`Riot LSP error: ${error.message}`);
					return { action: ErrorAction.Continue };
				},
				closed: () => {
					if (this.userStopped || this.disposed || this.stoppingClient) {
						this.log("Riot LSP closed after a requested stop.");
						return { action: CloseAction.DoNotRestart };
					}

					this.log("Riot LSP connection closed; VS Code will attempt a restart.");
					return { action: CloseAction.Restart };
				},
			},
		};

		const client = new LanguageClient("riot-lsp", "Riot LSP", serverOptions, clientOptions);

		try {
			await client.start();
			if (generation !== this.activationGeneration || this.userStopped || this.disposed) {
				await client.stop();
				return false;
			}

			this.languageClient = client;
			this.log("Riot LSP is ready.");
			this.updateState("running");
			return true;
		} catch (error) {
			const message = error instanceof Error ? error.message : String(error);
			this.log(`Riot LSP failed to start: ${message}`);
			return false;
		}
	}

	private activateCliFallback(): void {
		if (this.fallbackDiagnostics) {
			this.updateState("fallback");
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
		this.updateState("fallback");
	}

	private async disposeActiveFeatures(): Promise<void> {
		if (this.languageClient) {
			const client = this.languageClient;
			this.languageClient = undefined;
			this.stoppingClient = true;
			try {
				await client.stop();
			} finally {
				this.stoppingClient = false;
			}
		}

		this.fallbackDiagnostics = undefined;
		while (this.fallbackDisposables.length > 0) {
			this.fallbackDisposables.pop()?.dispose();
		}
	}
}
