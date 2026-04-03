import * as vscode from "vscode";
import {
	ensureRiotAvailable,
	isOcamlUri,
	Json,
	JsonObject,
	normalizeFilePath,
	parseJsonLines,
	runRiot,
	workspaceRootFor,
} from "./riot";

type SynDiagnostic = {
	kind?: {
		id?: string;
		expected?: string;
		found?: {
			kind?: string;
			text?: string;
		};
		fix?: string;
		hint?: string;
	};
	span?: {
		start?: number;
		end?: number;
	};
};

type FixDiagnostic = {
	severity?: string;
	message?: string;
	rule_id?: string;
	suggestion?: string | null;
	span?: {
		start?: number;
		end?: number;
	};
	fix?: {
		title?: string;
	} | null;
};

const asObject = (value: Json | undefined): JsonObject | undefined => {
	if (value !== null && value !== undefined && typeof value === "object" && !Array.isArray(value)) {
		return value as JsonObject;
	}

	return undefined;
};

const asArray = (value: Json | undefined): Json[] => {
	return Array.isArray(value) ? value : [];
};

const asString = (value: Json | undefined): string | undefined => {
	return typeof value === "string" ? value : undefined;
};

const spanRange = (document: vscode.TextDocument, span: { start?: number; end?: number }): vscode.Range => {
	const start = Math.max(span.start ?? 0, 0);
	const end = Math.max(span.end ?? start, start);
	return new vscode.Range(document.positionAt(start), document.positionAt(end));
};

const synMessage = (diagnostic: SynDiagnostic): { message: string; code?: string } => {
	const kind = diagnostic.kind ?? {};
	const found = kind.found ?? {};
	let message = `expected ${kind.expected ?? "syntax"}, found ${found.kind ?? "unknown"}`;
	if (kind.fix) {
		message += `\nfix: ${kind.fix}`;
	}
	if (kind.hint) {
		message += `\nhint: ${kind.hint}`;
	}

	return { message, code: kind.id };
};

const synItems = (document: vscode.TextDocument, diagnostics: Json[]): vscode.Diagnostic[] =>
	diagnostics.flatMap((value) => {
		const object = asObject(value) as unknown as SynDiagnostic | undefined;
		if (!object) {
			return [];
		}

		const span = object.span ?? {};
		const range = spanRange(document, span);
		const { message, code } = synMessage(object);
		const diagnostic = new vscode.Diagnostic(range, message, vscode.DiagnosticSeverity.Error);
		diagnostic.source = "riot fmt";
		diagnostic.code = code;
		return [diagnostic];
	});

const fixSeverity = (severity?: string): vscode.DiagnosticSeverity => {
	switch (severity) {
		case "error":
			return vscode.DiagnosticSeverity.Error;
		case "warning":
			return vscode.DiagnosticSeverity.Warning;
		case "info":
			return vscode.DiagnosticSeverity.Information;
		default:
			return vscode.DiagnosticSeverity.Hint;
	}
};

const fixItems = (document: vscode.TextDocument, diagnostics: Json[]): vscode.Diagnostic[] =>
	diagnostics.flatMap((value) => {
		const object = asObject(value) as unknown as FixDiagnostic | undefined;
		if (!object) {
			return [];
		}

		const span = object.span ?? {};
		const range = spanRange(document, span);
		let message = object.message ?? "riot fix reported an issue";
		if (object.suggestion) {
			message += `\nsuggestion: ${object.suggestion}`;
		}
		if (object.fix?.title) {
			message += `\nfix: ${object.fix.title}`;
		}

		const diagnostic = new vscode.Diagnostic(range, message, fixSeverity(object.severity));
		diagnostic.source = "riot fix";
		diagnostic.code = object.rule_id;
		return [diagnostic];
	});

const findFmtFileEvent = (
	events: JsonObject[],
	document: vscode.TextDocument,
	cwd?: string,
): JsonObject | undefined => {
	const target = normalizeFilePath(document.uri.fsPath);
	return events.find((event) => {
		if (asString(event.type) !== "file") {
			return false;
		}

		const file = asString(event.file);
		if (!file) {
			return false;
		}

		return normalizeFilePath(file, cwd) === target;
	});
};

const findFixFileResult = (
	events: JsonObject[],
	document: vscode.TextDocument,
): JsonObject | undefined => {
	const target = normalizeFilePath(document.uri.fsPath);

	for (const event of events) {
		const files = asArray(event.files);
		for (const file of files) {
			const object = asObject(file);
			if (!object) {
				continue;
			}

			const filePath = asString(object.file);
			if (filePath && normalizeFilePath(filePath) === target) {
				return object;
			}
		}
	}

	return undefined;
};

export class RiotDiagnostics {
	private readonly collection: vscode.DiagnosticCollection;

	constructor(private readonly context: vscode.ExtensionContext) {
		this.collection = vscode.languages.createDiagnosticCollection("riot");
	}

	register(): vscode.Disposable[] {
		const subscriptions: vscode.Disposable[] = [this.collection];

		subscriptions.push(
			vscode.workspace.onDidOpenTextDocument((document) => {
				void this.refresh(document);
			}),
			vscode.workspace.onDidSaveTextDocument((document) => {
				void this.refresh(document);
			}),
			vscode.workspace.onDidCloseTextDocument((document) => {
				this.collection.delete(document.uri);
			}),
		);

		for (const document of vscode.workspace.textDocuments) {
			void this.refresh(document);
		}

		return subscriptions;
	}

	async refresh(document: vscode.TextDocument): Promise<void> {
		if (!isOcamlUri(document.uri)) {
			return;
		}

		if (!vscode.workspace.getConfiguration("riot").get<boolean>("diagnostics.enabled", true)) {
			this.collection.delete(document.uri);
			return;
		}

		if (!(await ensureRiotAvailable(this.context, { prompt: false }))) {
			return;
		}

		const root = await workspaceRootFor(document.uri);
		const cwd = root?.fsPath;
		const diagnostics: vscode.Diagnostic[] = [];

		const fmtResult = await runRiot(this.context, ["fmt", "--json", document.uri.fsPath], { cwd });
		const fmtEvents = parseJsonLines(fmtResult.stdout);
		const fmtFile = findFmtFileEvent(fmtEvents, document, cwd);
		if (fmtFile) {
			diagnostics.push(...synItems(document, asArray(fmtFile.diagnostics)));
		}

		if (vscode.workspace.getConfiguration("riot").get<boolean>("diagnostics.runFix", true)) {
			const fixResult = await runRiot(this.context, ["fix", "--json", document.uri.fsPath], { cwd });
			const fixEvents = parseJsonLines(fixResult.stdout);
			const fixFile = findFixFileResult(fixEvents, document);
			if (fixFile) {
				diagnostics.push(...synItems(document, asArray(fixFile.parse_diagnostics)));
				diagnostics.push(...fixItems(document, asArray(fixFile.diagnostics)));
			}
		}

		this.collection.set(document.uri, diagnostics);
	}

	clear(document: vscode.TextDocument): void {
		this.collection.delete(document.uri);
	}
}
