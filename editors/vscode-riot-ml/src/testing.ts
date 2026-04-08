import * as path from "node:path";
import * as vscode from "vscode";
import {
	type Json,
	type JsonObject,
	ensureRiotAvailable,
	packageForUri,
	quote,
	readRiotInfo,
	runRiotStreaming,
} from "./riot";
import { discoverRiotRoots, riotPackageKey } from "./project_roots";

type RiotTestKind = "workspace" | "package" | "suite" | "case";

interface RiotTestMeta {
	kind: RiotTestKind;
	workspaceRoot: vscode.Uri;
	packageRoot?: vscode.Uri;
	packageName?: string;
	suiteName?: string;
	suiteUri?: vscode.Uri;
	caseQuery?: string;
	caseIndex?: number;
}

interface PackageAggregate {
	item: vscode.TestItem;
	totalSuites: number;
	failedSuites: number;
	skippedSuites: number;
}

interface RunState {
	startedItemIds: Set<string>;
	packageAggregates: Map<string, PackageAggregate>;
	totalSuites: number;
	failedSuites: number;
	skippedSuites: number;
	sawSuiteCompleted: boolean;
	noSuitesFound: boolean;
	matchedCase: boolean;
	lastErrorMessage?: string;
}

type RootRunStatus = "passed" | "failed" | "skipped" | "errored";

const jsonObjectFromLine = (line: string): JsonObject | undefined => {
	const trimmed = line.trim();
	if (trimmed === "") {
		return undefined;
	}

	try {
		const parsed = JSON.parse(trimmed) as Json;
		if (parsed !== null && typeof parsed === "object" && !Array.isArray(parsed)) {
			return parsed as JsonObject;
		}
	} catch {
		// Ignore non-JSON lines.
	}

	return undefined;
};

const stringField = (value: Json | undefined): string | undefined =>
	typeof value === "string" ? value : undefined;

const numberField = (value: Json | undefined): number | undefined =>
	typeof value === "number" ? value : undefined;

const objectField = (value: Json | undefined): JsonObject | undefined =>
	value !== null && typeof value === "object" && !Array.isArray(value)
		? value as JsonObject
		: undefined;

const arrayField = (value: Json | undefined): Json[] | undefined => Array.isArray(value) ? value : undefined;

const durationMs = (value: number | undefined): number | undefined =>
	typeof value === "number" ? Math.max(0, Math.floor(value / 1000)) : undefined;

const collectionItems = (collection: vscode.TestItemCollection): vscode.TestItem[] => {
	const items: vscode.TestItem[] = [];
	collection.forEach((item) => {
		items.push(item);
	});
	return items;
};

const isSameOrDescendant = (item: vscode.TestItem, maybeAncestor: vscode.TestItem): boolean => {
	let current: vscode.TestItem | undefined = item;
	while (current) {
		if (current.id === maybeAncestor.id) {
			return true;
		}
		current = current.parent;
	}
	return false;
};

const suiteOutput = (label: string, stream: "stdout" | "stderr", output: string): string =>
	`[${label}] ${stream}\r\n${output.endsWith("\n") ? output : `${output}\n`}\r\n`;

export class RiotTestController implements vscode.Disposable {
	private readonly controller: vscode.TestController;
	private readonly metaById = new Map<string, RiotTestMeta>();
	private readonly itemById = new Map<string, vscode.TestItem>();
	private readonly disposables: vscode.Disposable[] = [];

	constructor(
		private readonly context: vscode.ExtensionContext,
		private readonly output: vscode.OutputChannel,
	) {
		this.controller = vscode.tests.createTestController("riotTests", "Riot Tests");
		this.controller.refreshHandler = async () => {
			await this.refresh();
		};

		this.disposables.push(
			this.controller,
			this.controller.createRunProfile(
				"Run",
				vscode.TestRunProfileKind.Run,
				async (request, cancellation) => {
					await this.run(request, cancellation);
				},
				true,
			),
			vscode.workspace.onDidSaveTextDocument((document) => {
				if (path.basename(document.uri.fsPath) === "riot.toml") {
					void this.refresh();
				}
			}),
		);

		void this.refresh();
	}

	dispose(): void {
		while (this.disposables.length > 0) {
			this.disposables.pop()?.dispose();
		}
		this.metaById.clear();
		this.itemById.clear();
	}

	async runWorkspaceFromCommand(uri?: vscode.Uri): Promise<void> {
		if (!(await ensureRiotAvailable(this.context))) {
			return;
		}

		let items = await this.commandRequestItems(uri);
		if (items.length === 0) {
			await this.refresh();
			items = await this.commandRequestItems(uri);
		}

		if (items.length === 0) {
			void vscode.window.showWarningMessage("No Riot tests were discovered for the current workspace.");
			return;
		}

		await this.run(new vscode.TestRunRequest(items), new vscode.CancellationTokenSource().token);
	}

	private workspaceId(root: vscode.Uri): string {
		return `workspace:${root.toString()}`;
	}

	private packageId(workspaceRoot: vscode.Uri, packageName: string): string {
		return `package:${workspaceRoot.toString()}:${packageName}`;
	}

	private suiteId(workspaceRoot: vscode.Uri, packageName: string, suiteName: string): string {
		return `suite:${workspaceRoot.toString()}:${packageName}:${suiteName}`;
	}

	private caseId(
		workspaceRoot: vscode.Uri,
		packageName: string,
		suiteName: string,
		index: number,
	): string {
		return `case:${workspaceRoot.toString()}:${packageName}:${suiteName}:${index}`;
	}

	private trackItem(item: vscode.TestItem, meta: RiotTestMeta): vscode.TestItem {
		this.metaById.set(item.id, meta);
		this.itemById.set(item.id, item);
		return item;
	}

	private untrackItem(item: vscode.TestItem): void {
		this.metaById.delete(item.id);
		this.itemById.delete(item.id);
	}

	private async commandRequestItems(uri?: vscode.Uri): Promise<vscode.TestItem[]> {
		if (!uri) {
			return collectionItems(this.controller.items);
		}

		const info = await readRiotInfo(this.context, uri);
		if (!info) {
			return [];
		}

		if (info.kind === "workspace") {
			const workspaceItem = this.itemById.get(this.workspaceId(info.root));
			return workspaceItem ? [workspaceItem] : [];
		}

		const pkg = packageForUri(info, uri) ?? info.packages[0];
		if (!pkg) {
			return [];
		}

		const packageItem = this.itemById.get(this.packageId(info.root, pkg.name));
		return packageItem ? [packageItem] : [];
	}

	private async refresh(): Promise<void> {
		this.controller.items.replace([]);
		this.metaById.clear();
		this.itemById.clear();

		if (!(await ensureRiotAvailable(this.context, { prompt: false }))) {
			return;
		}

		const discovery = await discoverRiotRoots(this.context);
		if (discovery.manifests.length === 0) {
			return;
		}

		for (const manifest of discovery.workspaces) {
			const item = this.controller.createTestItem(
				this.workspaceId(manifest.root),
				path.basename(manifest.root.fsPath),
				manifest.root,
			);
			item.description = "workspace";
			this.trackItem(item, {
				kind: "workspace",
				workspaceRoot: manifest.root,
			});
			this.controller.items.add(item);
		}

		for (const manifest of discovery.workspaces) {
			for (const pkg of manifest.packages) {
				this.ensurePackageItem(manifest.root, pkg.name, pkg.root);
			}
		}

		for (const manifest of discovery.standalonePackages) {
			for (const pkg of manifest.packages) {
				this.ensurePackageItem(manifest.root, pkg.name, pkg.root);
			}
		}

		const listedRoots = [
			...discovery.workspaces.map((manifest) => manifest.root),
			...discovery.standalonePackages.map((manifest) => manifest.root),
		];

		await Promise.all(
			listedRoots.map((root) => this.discoverSuitesForRoot(root, discovery.packageRoots)),
		);
	}

	private async discoverSuitesForRoot(
		root: vscode.Uri,
		packageRoots: Map<string, vscode.Uri>,
	): Promise<void> {
		let errorMessage: string | undefined;
		const result = await runRiotStreaming(this.context, ["test", "--list", "--json"], {
			cwd: root.fsPath,
			onStdoutLine: (line) => {
				const event = jsonObjectFromLine(line);
				if (!event) {
					return;
				}

				const eventType = stringField(event.type);
				switch (eventType) {
					case "TestSuiteListed": {
						const packageName = stringField(event.package);
						const suiteName = stringField(event.suite);
						if (!packageName || !suiteName) {
							return;
						}

						const packageRoot = packageRoots.get(riotPackageKey(root, packageName));
						this.ensurePackageItem(root, packageName, packageRoot);
						const suitePath = stringField(event.path);
						const suiteUri = suitePath
							? vscode.Uri.file(path.join(root.fsPath, suitePath))
							: packageRoot;
						this.ensureSuiteItem(root, packageName, suiteName, suiteUri);
						return;
					}
					case "TestCaseListed": {
						const packageName = stringField(event.package);
						const suiteName = stringField(event.suite);
						const caseJson = objectField(event.case);
						const index = numberField(caseJson?.index ?? event.index);
						const name = stringField(caseJson?.name ?? event.name);
						if (!packageName || !suiteName || typeof index !== "number" || !name) {
							return;
						}

						const packageRoot = packageRoots.get(riotPackageKey(root, packageName));
						this.ensurePackageItem(root, packageName, packageRoot);
						this.ensureCaseItem(root, packageName, suiteName, index, name);
						return;
					}
					case "TestSuiteListFailed": {
						const packageName = stringField(event.package) ?? "<unknown>";
						const suiteName = stringField(event.suite) ?? "<unknown>";
						const message = stringField(event.message) ?? "unknown failure";
						this.output.appendLine(
							`Failed to list Riot test suite ${packageName}:${suiteName} in ${root.fsPath}: ${message}`,
						);
						return;
					}
					case "test.error":
						errorMessage = stringField(event.message);
						return;
					default:
						return;
				}
			},
			onStderrLine: (line) => {
				if (line.trim() === "") {
					return;
				}

				this.output.appendLine(line);
			},
		});

		if (result.code !== 0) {
			this.output.appendLine(
				`Failed to list Riot tests in ${root.fsPath}: ${errorMessage || result.stderr.trim() || result.stdout.trim() || `exit ${result.code}`}`,
			);
		}
	}

	private ensurePackageItem(
		workspaceRoot: vscode.Uri,
		packageName: string,
		packageRoot?: vscode.Uri,
	): vscode.TestItem {
		const id = this.packageId(workspaceRoot, packageName);
		const existing = this.itemById.get(id);
		if (existing) {
			return existing;
		}

		const parent = this.itemById.get(this.workspaceId(workspaceRoot));
		const collection = parent ? parent.children : this.controller.items;
		const item = this.controller.createTestItem(id, packageName, packageRoot ?? workspaceRoot);
		item.description = "package";
		collection.add(item);
		return this.trackItem(item, {
			kind: "package",
			workspaceRoot,
			packageRoot,
			packageName,
		});
	}

	private ensureSuiteItem(
		workspaceRoot: vscode.Uri,
		packageName: string,
		suiteName: string,
		suiteUri?: vscode.Uri,
	): vscode.TestItem {
		const id = this.suiteId(workspaceRoot, packageName, suiteName);
		const existing = this.itemById.get(id);
		if (existing) {
			return existing;
		}

		const packageItem = this.ensurePackageItem(workspaceRoot, packageName);
		const item = this.controller.createTestItem(id, suiteName, suiteUri ?? packageItem.uri);
		item.description = "suite";
		packageItem.children.add(item);
		return this.trackItem(item, {
			kind: "suite",
			workspaceRoot,
			packageRoot: this.metaById.get(packageItem.id)?.packageRoot,
			packageName,
			suiteName,
			suiteUri,
		});
	}

	private ensureCaseItem(
		workspaceRoot: vscode.Uri,
		packageName: string,
		suiteName: string,
		index: number,
		name: string,
	): vscode.TestItem {
		const id = this.caseId(workspaceRoot, packageName, suiteName, index);
		const existing = this.itemById.get(id);
		if (existing) {
			return existing;
		}

		const suiteItem = this.ensureSuiteItem(workspaceRoot, packageName, suiteName);
		const item = this.controller.createTestItem(id, name, suiteItem.uri);
		item.description = "test";
		suiteItem.children.add(item);
		return this.trackItem(item, {
			kind: "case",
			workspaceRoot,
			packageRoot: this.metaById.get(suiteItem.id)?.packageRoot,
			packageName,
			suiteName,
			suiteUri: suiteItem.uri,
			caseQuery: name,
			caseIndex: index,
		});
	}

	private rootItems(request: vscode.TestRunRequest): vscode.TestItem[] {
		const requested = request.include ? [...request.include] : collectionItems(this.controller.items);
		const deduped = requested.filter((item, index) =>
			requested.findIndex((candidate) => candidate.id === item.id) === index);
		const collapsed = deduped.filter((item) =>
			!deduped.some((other) => other.id !== item.id && isSameOrDescendant(item, other)));
		const excludedIds = new Set((request.exclude ?? []).map((item) => item.id));
		return collapsed.filter((item) => !excludedIds.has(item.id));
	}

	private commandFor(meta: RiotTestMeta): { cwd: string; args: string[] } | undefined {
		const cwd = meta.workspaceRoot.fsPath;
		switch (meta.kind) {
			case "workspace":
				return { cwd, args: ["test", "--json"] };
			case "package":
				if (!meta.packageName) {
					return undefined;
				}
				if (meta.packageRoot && meta.packageRoot.fsPath === meta.workspaceRoot.fsPath) {
					return { cwd, args: ["test", "--json"] };
				}
				return { cwd, args: ["test", "--json", "-p", meta.packageName] };
			case "suite":
				if (!meta.packageName || !meta.suiteName) {
					return undefined;
				}
				return { cwd, args: ["test", "--json", `${meta.packageName}:${meta.suiteName}`] };
			case "case":
				if (!meta.packageName || !meta.suiteName || !meta.caseQuery) {
					return undefined;
				}
				return {
					cwd,
					args: ["test", "--json", `${meta.packageName}:${meta.suiteName}:${meta.caseQuery}`],
				};
		}
	}

	private appendRunOutput(run: vscode.TestRun, text: string, item?: vscode.TestItem): void {
		run.appendOutput(text.replace(/\n/g, "\r\n"), undefined, item);
	}

	private markStarted(run: vscode.TestRun, state: RunState, item: vscode.TestItem): void {
		if (state.startedItemIds.has(item.id)) {
			return;
		}

		state.startedItemIds.add(item.id);
		run.started(item);
	}

	private packageAggregateFor(
		state: RunState,
		item: vscode.TestItem,
	): PackageAggregate {
		const existing = state.packageAggregates.get(item.id);
		if (existing) {
			return existing;
		}

		const aggregate: PackageAggregate = {
			item,
			totalSuites: 0,
			failedSuites: 0,
			skippedSuites: 0,
		};
		state.packageAggregates.set(item.id, aggregate);
		return aggregate;
	}

	private suiteStatusMessage(summary: JsonObject, suiteName: string): vscode.TestMessage {
		const failed = numberField(summary.failed) ?? 0;
		return new vscode.TestMessage(
			failed > 0
				? `${suiteName} failed with ${failed} failing test case(s).`
				: `${suiteName} did not complete successfully.`,
		);
	}

	private applyCaseResult(
		run: vscode.TestRun,
		state: RunState,
		rootItem: vscode.TestItem,
		caseItem: vscode.TestItem,
		test: JsonObject,
	): void {
		const status = stringField(test.status);
		const duration = durationMs(numberField(test.duration_us));
		this.markStarted(run, state, caseItem);

		if (rootItem.id === caseItem.id) {
			state.matchedCase = true;
		}

		switch (status) {
			case "passed":
				run.passed(caseItem, duration);
				return;
			case "skipped":
				run.skipped(caseItem);
				return;
			case "timed_out": {
				const timeoutMs = numberField(test.timeout_ms);
				run.failed(
					caseItem,
					new vscode.TestMessage(
						timeoutMs ? `Test timed out after ${timeoutMs}ms.` : "Test timed out.",
					),
					duration,
				);
				return;
			}
			case "failed":
				run.failed(
					caseItem,
					new vscode.TestMessage(stringField(test.message) ?? "Test failed."),
					duration,
				);
				return;
			default:
				run.errored(caseItem, new vscode.TestMessage("Unknown Riot test status."), duration);
		}
	}

	private handleEvent(
		rootItem: vscode.TestItem,
		rootMeta: RiotTestMeta,
		run: vscode.TestRun,
		state: RunState,
		event: JsonObject,
	): void {
		const eventType = stringField(event.type);
		switch (eventType) {
			case "NoSuitesFound":
				state.noSuitesFound = true;
				return;
			case "RunningSuite": {
				const packageName = stringField(event.package);
				const suiteName = stringField(event.suite);
				if (!packageName || !suiteName) {
					return;
				}

				const packageItem = this.ensurePackageItem(rootMeta.workspaceRoot, packageName);
				const suiteItem = this.ensureSuiteItem(
					rootMeta.workspaceRoot,
					packageName,
					suiteName,
				);

				this.markStarted(run, state, packageItem);
				this.markStarted(run, state, suiteItem);
				return;
			}
			case "SuiteCompleted": {
				const packageName = stringField(event.package);
				const suiteName = stringField(event.suite);
				const summary = objectField(event.summary);
				if (!packageName || !suiteName || !summary) {
					return;
				}

				state.sawSuiteCompleted = true;
				state.totalSuites += 1;

				const packageItem = this.ensurePackageItem(rootMeta.workspaceRoot, packageName);
				const suiteItem = this.ensureSuiteItem(
					rootMeta.workspaceRoot,
					packageName,
					suiteName,
				);
				const packageAggregate = this.packageAggregateFor(state, packageItem);

				this.markStarted(run, state, packageItem);
				this.markStarted(run, state, suiteItem);

				packageAggregate.totalSuites += 1;

				const failed = numberField(summary.failed) ?? 0;
				const skipped = numberField(summary.skipped) ?? 0;
				const total = numberField(summary.total) ?? 0;
				if (failed > 0) {
					state.failedSuites += 1;
					packageAggregate.failedSuites += 1;
					run.failed(
						suiteItem,
						this.suiteStatusMessage(summary, suiteName),
						durationMs(numberField(event.duration_us)),
					);
				} else if (total > 0 && skipped === total) {
					state.skippedSuites += 1;
					packageAggregate.skippedSuites += 1;
					run.skipped(suiteItem);
				} else {
					run.passed(suiteItem, durationMs(numberField(event.duration_us)));
				}

				for (const testJson of arrayField(event.tests) ?? []) {
					const test = objectField(testJson);
					if (!test) {
						continue;
					}

					const testName = stringField(test.name);
					const index = numberField(test.index);
					if (!testName || typeof index !== "number") {
						continue;
					}

					const caseItem = this.ensureCaseItem(
						rootMeta.workspaceRoot,
						packageName,
						suiteName,
						index,
						testName,
					);
					this.applyCaseResult(run, state, rootItem, caseItem, test);
				}

				const stdout = stringField(event.stdout);
				if (stdout && stdout.trim() !== "") {
					const label = `${packageName}:${suiteName}`;
					this.output.appendLine(`[${label}] stdout`);
					this.output.appendLine(stdout.trimEnd());
					this.output.appendLine("");
					this.appendRunOutput(run, suiteOutput(label, "stdout", stdout), suiteItem);
					if (rootItem.id !== suiteItem.id) {
						this.appendRunOutput(run, suiteOutput(label, "stdout", stdout), rootItem);
					}
				}

				const stderr = stringField(event.stderr);
				if (stderr && stderr.trim() !== "") {
					const label = `${packageName}:${suiteName}`;
					this.output.appendLine(`[${label}] stderr`);
					this.output.appendLine(stderr.trimEnd());
					this.output.appendLine("");
					this.appendRunOutput(run, suiteOutput(label, "stderr", stderr), suiteItem);
					if (rootItem.id !== suiteItem.id) {
						this.appendRunOutput(run, suiteOutput(label, "stderr", stderr), rootItem);
					}
				}
				return;
			}
			case "test.error": {
				const message = stringField(event.message) ?? "Riot test failed.";
				state.lastErrorMessage = message;
				this.output.appendLine(`[test.error] ${message}`);
				this.appendRunOutput(run, `[test.error] ${message}\n`, rootItem);
				return;
			}
			default:
				return;
		}
	}

	private finalizePackageAggregates(run: vscode.TestRun, state: RunState, rootItem: vscode.TestItem): void {
		for (const aggregate of state.packageAggregates.values()) {
			if (aggregate.item.id === rootItem.id) {
				continue;
			}

			if (aggregate.failedSuites > 0) {
				run.failed(
					aggregate.item,
					new vscode.TestMessage(
						`${aggregate.failedSuites} suite(s) failed in ${aggregate.item.label}.`,
					),
				);
				continue;
			}

			if (aggregate.totalSuites > 0 && aggregate.skippedSuites === aggregate.totalSuites) {
				run.skipped(aggregate.item);
				continue;
			}

			if (aggregate.totalSuites > 0) {
				run.passed(aggregate.item);
			}
		}
	}

	private finalizeRootItem(
		run: vscode.TestRun,
		state: RunState,
		rootItem: vscode.TestItem,
		rootMeta: RiotTestMeta,
		result: { code: number; stderr: string; stdout: string },
	): RootRunStatus {
		const errorMessage =
			state.lastErrorMessage
			|| result.stderr.trim()
			|| result.stdout.trim()
			|| "Riot test failed.";
		switch (rootMeta.kind) {
			case "workspace":
			case "package":
				if (!state.sawSuiteCompleted && result.code !== 0) {
					run.errored(
						rootItem,
						new vscode.TestMessage(errorMessage),
					);
					return "errored";
				} else if (state.noSuitesFound || !state.sawSuiteCompleted) {
					run.skipped(rootItem);
					return "skipped";
				} else if (state.failedSuites > 0) {
					run.failed(
						rootItem,
						new vscode.TestMessage(`${state.failedSuites} suite(s) failed.`),
					);
					return "failed";
				} else if (state.totalSuites > 0 && state.skippedSuites === state.totalSuites) {
					run.skipped(rootItem);
					return "skipped";
				} else {
					run.passed(rootItem);
					return "passed";
				}
			case "suite":
				if (state.noSuitesFound) {
					run.skipped(rootItem);
					return "skipped";
				}
				if (!state.sawSuiteCompleted && result.code !== 0) {
					run.errored(
						rootItem,
						new vscode.TestMessage(errorMessage),
					);
					return "errored";
				}
				if (!state.sawSuiteCompleted) {
					run.skipped(rootItem);
					return "skipped";
				}
				return state.failedSuites > 0 ? "failed" : "passed";
			case "case":
				if (state.noSuitesFound) {
					run.skipped(rootItem);
					return "skipped";
				}
				if (!state.matchedCase && result.code !== 0) {
					run.errored(
						rootItem,
						new vscode.TestMessage(errorMessage),
					);
					return "errored";
				}
				if (!state.matchedCase) {
					run.skipped(rootItem);
					return "skipped";
				}
				return state.failedSuites > 0 ? "failed" : "passed";
			}
		}

	private async runSingleItem(
		run: vscode.TestRun,
		rootItem: vscode.TestItem,
		cancellation: vscode.CancellationToken,
	): Promise<RootRunStatus> {
		const meta = this.metaById.get(rootItem.id);
		if (!meta) {
			return "skipped";
		}

		const command = this.commandFor(meta);
		if (!command) {
			run.errored(rootItem, new vscode.TestMessage("Could not derive Riot test command."));
			return "errored";
		}

		const state: RunState = {
			startedItemIds: new Set<string>(),
			packageAggregates: new Map<string, PackageAggregate>(),
			totalSuites: 0,
			failedSuites: 0,
			skippedSuites: 0,
			sawSuiteCompleted: false,
			noSuitesFound: false,
			matchedCase: false,
			lastErrorMessage: undefined,
		};

		this.output.appendLine(
			`$ (cd ${quote(command.cwd)} && ${["riot", ...command.args].map(quote).join(" ")})`,
		);
		this.output.appendLine("");

		this.markStarted(run, state, rootItem);

		const result = await runRiotStreaming(this.context, command.args, {
			cwd: command.cwd,
			cancellation,
			onStdoutLine: (line) => {
				const event = jsonObjectFromLine(line);
				if (event) {
					this.handleEvent(rootItem, meta, run, state, event);
					return;
				}

				if (line.trim() === "") {
					return;
				}

				this.output.appendLine(line);
				this.appendRunOutput(run, `${line}\n`, rootItem);
			},
			onStderrLine: (line) => {
				if (line.trim() === "") {
					return;
				}

				this.output.appendLine(line);
				this.appendRunOutput(run, `${line}\n`, rootItem);
			},
		});

		this.finalizePackageAggregates(run, state, rootItem);
		return this.finalizeRootItem(run, state, rootItem, meta, result);
	}

	private async runWorkspaceItem(
		run: vscode.TestRun,
		workspaceItem: vscode.TestItem,
		cancellation: vscode.CancellationToken,
	): Promise<void> {
		const packageItems = collectionItems(workspaceItem.children)
			.filter((item) => this.metaById.get(item.id)?.kind === "package");
		if (packageItems.length === 0) {
			run.skipped(workspaceItem);
			return;
		}

		run.started(workspaceItem);
		for (const item of packageItems) {
			run.enqueued(item);
		}

		const outcomes: RootRunStatus[] = [];
		for (const item of packageItems) {
			if (cancellation.isCancellationRequested) {
				break;
			}

			const outcome = await this.runSingleItem(run, item, cancellation);
			outcomes.push(outcome);
		}

		const total = outcomes.length;
		const failed = outcomes.filter((outcome) => outcome === "failed").length;
		const errored = outcomes.filter((outcome) => outcome === "errored").length;
		const skipped = outcomes.filter((outcome) => outcome === "skipped").length;

		if (total === 0 || skipped === total) {
			run.skipped(workspaceItem);
			return;
		}

		if (errored > 0) {
			run.errored(
				workspaceItem,
				new vscode.TestMessage(
					`${errored} package run(s) errored${failed > 0 ? `, ${failed} failed` : ""}.`,
				),
			);
			return;
		}

		if (failed > 0) {
			run.failed(
				workspaceItem,
				new vscode.TestMessage(`${failed} package run(s) failed.`),
			);
			return;
		}

		run.passed(workspaceItem);
	}

	private async run(
		request: vscode.TestRunRequest,
		cancellation: vscode.CancellationToken,
	): Promise<void> {
		if (!(await ensureRiotAvailable(this.context))) {
			return;
		}

		const rootItems = this.rootItems(request);
		const run = this.controller.createTestRun(request);
		try {
			for (const item of rootItems) {
				if (cancellation.isCancellationRequested) {
					break;
				}

				run.enqueued(item);
			}

			for (const item of rootItems) {
				if (cancellation.isCancellationRequested) {
					break;
				}

				const meta = this.metaById.get(item.id);
				if (meta?.kind === "workspace") {
					await this.runWorkspaceItem(run, item, cancellation);
				} else {
					await this.runSingleItem(run, item, cancellation);
				}
			}
		} finally {
			run.end();
		}
	}
}
