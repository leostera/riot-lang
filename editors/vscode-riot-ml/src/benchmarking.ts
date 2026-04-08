import * as path from "node:path";
import * as vscode from "vscode";
import {
	type Json,
	type JsonObject,
	ensureRiotAvailable,
	quote,
	runRiotStreaming,
	stripAnsi,
} from "./riot";
import {
	type BenchmarkHistoryRecord,
	type BenchmarkResultsSelection,
	RiotBenchmarkResultsView,
} from "./benchmark_results";
import { discoverRiotRoots, riotPackageKey } from "./project_roots";

type RiotBenchmarkNodeKind = "workspace" | "package" | "suite" | "benchmark" | "comparison";

interface RiotBenchmarkMeta {
	kind: RiotBenchmarkNodeKind;
	workspaceRoot: vscode.Uri;
	packageRoot?: vscode.Uri;
	packageName?: string;
	suiteName?: string;
	suiteUri?: vscode.Uri;
	caseQuery?: string;
}

interface PackageAggregate {
	node: RiotBenchmarkNode;
	totalSuites: number;
	failedSuites: number;
	skippedSuites: number;
}

interface RunSummary {
	total: number;
	completed: number;
	skipped: number;
	failed: number;
}

interface RunState {
	packageAggregates: Map<string, PackageAggregate>;
	totalSuites: number;
	failedSuites: number;
	skippedSuites: number;
	sawSuiteCompleted: boolean;
	noSuitesFound: boolean;
	matchedCase: boolean;
	lastErrorMessage?: string;
	summary?: RunSummary;
}

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
	value !== null && value !== undefined && typeof value === "object" && !Array.isArray(value)
		? value as JsonObject
		: undefined;

const arrayField = (value: Json | undefined): Json[] =>
	Array.isArray(value) ? value : [];

const nodeLabel = (node: RiotBenchmarkNode): string =>
	typeof node.label === "string" ? node.label : node.label?.label ?? "";

const sortNodes = (left: RiotBenchmarkNode, right: RiotBenchmarkNode): number =>
	nodeLabel(left).localeCompare(nodeLabel(right));

const defaultIconForKind = (kind: RiotBenchmarkNodeKind): vscode.ThemeIcon => {
	switch (kind) {
		case "workspace":
			return new vscode.ThemeIcon("folder");
		case "package":
			return new vscode.ThemeIcon("package");
		case "suite":
			return new vscode.ThemeIcon("symbol-file");
		case "benchmark":
			return new vscode.ThemeIcon("symbol-event");
		case "comparison":
			return new vscode.ThemeIcon("symbol-operator");
	}
};

const runningIcon = new vscode.ThemeIcon("loading~spin");
const failedIcon = new vscode.ThemeIcon("error");
const skippedIcon = new vscode.ThemeIcon("circle-slash");

const cleanOutput = (value: string): string => stripAnsi(value).replace(/\r/g, "");

const formatRatio = (ratio: number): string => `${ratio.toFixed(2)}x`;

const formatDurationNanos = (nanos: number | undefined): string => {
	if (typeof nanos !== "number" || !Number.isFinite(nanos)) {
		return "unknown";
	}

	const abs = Math.abs(nanos);
	if (abs >= 1_000_000_000) {
		return `${(nanos / 1_000_000_000).toFixed(3)}s`;
	}

	if (abs >= 1_000_000) {
		return `${(nanos / 1_000_000).toFixed(3)}ms`;
	}

	if (abs >= 1_000) {
		return `${(nanos / 1_000).toFixed(3)}us`;
	}

	return `${nanos.toFixed(0)}ns`;
};

const benchmarkDescription = (benchmark: JsonObject): string => {
	const status = stringField(benchmark.status);
	if (status === "completed") {
		const statistics = objectField(benchmark.statistics);
		return `mean ${formatDurationNanos(numberField(statistics?.mean_nanos))}`;
	}

	if (status === "skipped") {
		return "skipped";
	}

	if (status === "failed") {
		return "failed";
	}

	return "unknown";
};

const benchmarkTooltip = (benchmark: JsonObject): string => {
	const name = stringField(benchmark.name) ?? "benchmark";
	const status = stringField(benchmark.status) ?? "unknown";
	if (status === "completed") {
		const statistics = objectField(benchmark.statistics);
		return [
			name,
			"",
			`iterations: ${numberField(statistics?.iterations) ?? 0}`,
			`mean: ${formatDurationNanos(numberField(statistics?.mean_nanos))}`,
			`median: ${formatDurationNanos(numberField(statistics?.median_nanos))}`,
			`min: ${formatDurationNanos(numberField(statistics?.min_nanos))}`,
			`max: ${formatDurationNanos(numberField(statistics?.max_nanos))}`,
			`std_dev: ${formatDurationNanos(numberField(statistics?.std_dev_nanos))}`,
			`total: ${formatDurationNanos(numberField(statistics?.total_time_nanos))}`,
		].join("\n");
	}

	if (status === "failed") {
		return [
			name,
			"",
			`failed: ${cleanOutput(stringField(benchmark.message) ?? "unknown error")}`,
		].join("\n");
	}

	if (status === "skipped") {
		return `${name}\n\nskipped`;
	}

	return `${name}\n\nunknown status`;
};

const comparisonDescription = (comparison: JsonObject): string => {
	const fastest = stringField(comparison.fastest);
	return fastest ? `fastest ${fastest}` : "comparison";
};

const comparisonTooltip = (comparison: JsonObject): string => {
	const description = stringField(comparison.description) ?? "comparison";
	const fastest = stringField(comparison.fastest) ?? "unknown";
	const lines = [
		description,
		"",
		`fastest: ${fastest}`,
	];

	for (const caseJson of arrayField(comparison.case_results)) {
		const caseResult = objectField(caseJson);
		const name = stringField(caseResult?.name);
		const statistics = objectField(caseResult?.statistics);
		if (!name || !statistics) {
			continue;
		}

		lines.push(`${name}: ${formatDurationNanos(numberField(statistics.mean_nanos))}`);
	}

	for (const ratioJson of arrayField(comparison.speedup_ratios)) {
		const ratio = objectField(ratioJson);
		const name = stringField(ratio?.name);
		const value = numberField(ratio?.ratio);
		if (!name || typeof value !== "number") {
			continue;
		}

		lines.push(`${fastest} vs ${name}: ${formatRatio(value)}`);
	}

	return lines.join("\n");
};

const suiteSummaryDescription = (summary: JsonObject): string => {
	const total = numberField(summary.total) ?? 0;
	const completed = numberField(summary.completed) ?? 0;
	const skipped = numberField(summary.skipped) ?? 0;
	const failed = numberField(summary.failed) ?? 0;
	if (failed > 0) {
		return `${failed} failed, ${completed} completed`;
	}

	if (total > 0 && skipped === total) {
		return "all skipped";
	}

	return `${completed}/${total} completed`;
};

const runSummaryDescription = (summary: RunSummary): string => {
	if (summary.failed > 0) {
		return `${summary.failed} failed, ${summary.completed} completed`;
	}

	if (summary.total > 0 && summary.skipped === summary.total) {
		return "all skipped";
	}

	return `${summary.completed}/${summary.total} completed`;
};

const suiteOutput = (label: string, stream: "stdout" | "stderr", output: string): string =>
	`[${label}] ${stream}\n${(() => {
		const cleaned = cleanOutput(output);
		return cleaned.endsWith("\n") ? cleaned : `${cleaned}\n`;
	})()}\n`;

const formatSuiteReport = (event: JsonObject): string => {
	const packageName = stringField(event.package) ?? "<unknown>";
	const suiteName = stringField(event.suite) ?? "<unknown>";
	const lines: string[] = [`${packageName}:${suiteName}`];

	for (const benchmarkJson of arrayField(event.benchmarks)) {
		const benchmark = objectField(benchmarkJson);
		const name = stringField(benchmark?.name);
		const status = stringField(benchmark?.status);
		if (!benchmark || !name || !status) {
			continue;
		}

		lines.push("");
		if (status === "completed") {
			const statistics = objectField(benchmark.statistics);
			lines.push(`${name}:`);
			lines.push(`  iterations: ${numberField(statistics?.iterations) ?? 0}`);
			lines.push(`  mean: ${formatDurationNanos(numberField(statistics?.mean_nanos))}`);
			lines.push(`  median: ${formatDurationNanos(numberField(statistics?.median_nanos))}`);
			lines.push(`  min: ${formatDurationNanos(numberField(statistics?.min_nanos))}`);
			lines.push(`  max: ${formatDurationNanos(numberField(statistics?.max_nanos))}`);
			lines.push(`  std_dev: ${formatDurationNanos(numberField(statistics?.std_dev_nanos))}`);
		} else if (status === "failed") {
			lines.push(`${name}: FAILED`);
			lines.push(`  ${stringField(benchmark.message) ?? "unknown error"}`);
		} else {
			lines.push(`${name}: SKIPPED`);
		}
	}

	for (const comparisonJson of arrayField(event.comparisons)) {
		const comparison = objectField(comparisonJson);
		const description = stringField(comparison?.description);
		const fastest = stringField(comparison?.fastest);
		if (!comparison || !description || !fastest) {
			continue;
		}

		lines.push("");
		lines.push(`Comparison: ${description}`);
		lines.push(`  Fastest: ${fastest}`);
		for (const caseJson of arrayField(comparison.case_results)) {
			const caseResult = objectField(caseJson);
			const name = stringField(caseResult?.name);
			const statistics = objectField(caseResult?.statistics);
			if (!name || !statistics) {
				continue;
			}

			lines.push(
				`  ${name}: ${formatDurationNanos(numberField(statistics.mean_nanos))} mean`,
			);
		}
		for (const ratioJson of arrayField(comparison.speedup_ratios)) {
			const ratio = objectField(ratioJson);
			const name = stringField(ratio?.name);
			const value = numberField(ratio?.ratio);
			if (!name || typeof value !== "number") {
				continue;
			}

			lines.push(`  ${fastest} ran ${formatRatio(value)} faster than ${name}`);
		}
	}

	const stdout = stringField(event.stdout);
	if (stdout && stdout.trim() !== "") {
		lines.push("");
		lines.push("[stdout]");
		lines.push(cleanOutput(stdout).trimEnd());
	}

	const stderr = stringField(event.stderr);
	if (stderr && stderr.trim() !== "") {
		lines.push("");
		lines.push("[stderr]");
		lines.push(cleanOutput(stderr).trimEnd());
	}

	return `${lines.join("\n")}\n`;
};

class RiotBenchmarkNode extends vscode.TreeItem {
	readonly children = new Map<string, RiotBenchmarkNode>();
	readonly defaultIcon: vscode.ThemeIcon;
	readonly baseDescription?: string;
	readonly baseTooltip?: string;
	private actionRunning = false;

	constructor(
		public readonly id: string,
		label: string,
		public readonly meta: RiotBenchmarkMeta,
		collapsibleState: vscode.TreeItemCollapsibleState,
		options: {
			description?: string;
			tooltip?: string;
			resourceUri?: vscode.Uri;
			icon?: vscode.ThemeIcon;
		} = {},
	) {
		super(label, collapsibleState);
		this.baseDescription = options.description;
		this.baseTooltip = options.tooltip;
		this.resourceUri = options.resourceUri;
		this.defaultIcon = options.icon ?? defaultIconForKind(meta.kind);
		this.setActionRunning(false);
		this.resetPresentation();
	}

	setActionRunning(running: boolean): void {
		this.actionRunning = running;
		this.contextValue = running ? "riotBenchmarks.running" : "riotBenchmarks.idle";
	}

	resetPresentation(): void {
		this.description = this.baseDescription;
		this.tooltip = this.baseTooltip;
		this.iconPath = this.defaultIcon;
	}
}

export class RiotBenchmarkController implements vscode.TreeDataProvider<RiotBenchmarkNode>, vscode.Disposable {
	private readonly treeView: vscode.TreeView<RiotBenchmarkNode>;
	private readonly onDidChangeTreeDataEmitter = new vscode.EventEmitter<RiotBenchmarkNode | undefined>();
	private readonly nodesById = new Map<string, RiotBenchmarkNode>();
	private readonly rootNodes: RiotBenchmarkNode[] = [];
	private readonly disposables: vscode.Disposable[] = [];
	private readonly cancellationByNodeId = new Map<string, vscode.CancellationTokenSource>();
	private selectedNode: RiotBenchmarkNode | undefined;

	readonly onDidChangeTreeData = this.onDidChangeTreeDataEmitter.event;

	constructor(
		private readonly context: vscode.ExtensionContext,
		private readonly output: vscode.OutputChannel,
		private readonly resultsView: RiotBenchmarkResultsView,
	) {
		this.treeView = vscode.window.createTreeView("riotBenchmarks", {
			treeDataProvider: this,
			showCollapseAll: true,
		});

		this.disposables.push(
			this.treeView,
			vscode.commands.registerCommand("riot.bench.refresh", async () => {
				await this.refresh();
			}),
			vscode.commands.registerCommand("riot.bench.run", async (node?: RiotBenchmarkNode) => {
				await this.run(node ?? this.selectedNode);
			}),
			vscode.commands.registerCommand("riot.bench.stop", async (node?: RiotBenchmarkNode) => {
				await this.stop(node ?? this.selectedNode);
			}),
			vscode.workspace.onDidSaveTextDocument((document) => {
				if (path.basename(document.uri.fsPath) === "riot.toml") {
					void this.refresh();
				}
			}),
			this.treeView.onDidChangeSelection((event) => {
				this.selectedNode = event.selection[0];
				this.resultsView.setSelection(this.selectionForNode(this.selectedNode));
			}),
		);

		void this.refresh();
	}

	dispose(): void {
		for (const source of this.cancellationByNodeId.values()) {
			source.cancel();
			source.dispose();
		}
		this.cancellationByNodeId.clear();
		this.onDidChangeTreeDataEmitter.dispose();
		while (this.disposables.length > 0) {
			this.disposables.pop()?.dispose();
		}
		this.nodesById.clear();
		this.rootNodes.length = 0;
	}

	getTreeItem(element: RiotBenchmarkNode): vscode.TreeItem {
		return element;
	}

	getChildren(element?: RiotBenchmarkNode): RiotBenchmarkNode[] {
		if (!element) {
			return [...this.rootNodes].sort(sortNodes);
		}

		return [...element.children.values()].sort(sortNodes);
	}

	private fire(node?: RiotBenchmarkNode): void {
		this.onDidChangeTreeDataEmitter.fire(node);
	}

	private workspaceId(root: vscode.Uri): string {
		return `benchmark-workspace:${root.toString()}`;
	}

	private selectionForNode(node?: RiotBenchmarkNode): BenchmarkResultsSelection | undefined {
		if (!node) {
			return undefined;
		}

		const meta = node.meta;
		switch (meta.kind) {
			case "workspace":
				return {
					kind: "workspace",
					workspaceRoot: meta.workspaceRoot.fsPath,
					label: `${nodeLabel(node)} workspace benchmarks`,
				};
			case "package":
				return {
					kind: "package",
					workspaceRoot: meta.workspaceRoot.fsPath,
					packageName: meta.packageName,
					label: `${nodeLabel(node)} benchmarks`,
				};
			case "suite":
				return {
					kind: "suite",
					workspaceRoot: meta.workspaceRoot.fsPath,
					packageName: meta.packageName,
					suiteName: meta.suiteName,
					label: `${meta.packageName}:${meta.suiteName}`,
				};
			case "benchmark":
			case "comparison":
				return {
					kind: meta.kind,
					workspaceRoot: meta.workspaceRoot.fsPath,
					packageName: meta.packageName,
					suiteName: meta.suiteName,
					itemName: meta.caseQuery,
					itemKind: meta.kind,
					label: `${meta.packageName}:${meta.suiteName}:${meta.caseQuery}`,
				};
		}
	}

	private packageId(workspaceRoot: vscode.Uri, packageName: string): string {
		return `benchmark-package:${workspaceRoot.toString()}:${packageName}`;
	}

	private suiteId(workspaceRoot: vscode.Uri, packageName: string, suiteName: string): string {
		return `benchmark-suite:${workspaceRoot.toString()}:${packageName}:${suiteName}`;
	}

	private itemId(
		workspaceRoot: vscode.Uri,
		packageName: string,
		suiteName: string,
		kind: "benchmark" | "comparison",
		name: string,
	): string {
		return `benchmark-item:${workspaceRoot.toString()}:${packageName}:${suiteName}:${kind}:${encodeURIComponent(name)}`;
	}

	private clearTree(): void {
		this.nodesById.clear();
		this.rootNodes.length = 0;
	}

	private registerNode(node: RiotBenchmarkNode): RiotBenchmarkNode {
		this.nodesById.set(node.id, node);
		return node;
	}

	private pushRootNode(node: RiotBenchmarkNode): void {
		if (!this.rootNodes.some((candidate) => candidate.id === node.id)) {
			this.rootNodes.push(node);
		}
	}

	private ensureWorkspaceNode(root: vscode.Uri): RiotBenchmarkNode {
		const id = this.workspaceId(root);
		const existing = this.nodesById.get(id);
		if (existing) {
			return existing;
		}

		const node = new RiotBenchmarkNode(
			id,
			path.basename(root.fsPath),
			{
				kind: "workspace",
				workspaceRoot: root,
			},
			vscode.TreeItemCollapsibleState.Expanded,
			{
				tooltip: root.fsPath,
			},
		);
		this.pushRootNode(node);
		return this.registerNode(node);
	}

	private ensurePackageNode(
		workspaceRoot: vscode.Uri,
		packageName: string,
		packageRoot?: vscode.Uri,
		description?: string,
	): RiotBenchmarkNode {
		const id = this.packageId(workspaceRoot, packageName);
		const existing = this.nodesById.get(id);
		if (existing) {
			return existing;
		}

		const node = new RiotBenchmarkNode(
			id,
			packageName,
			{
				kind: "package",
				workspaceRoot,
				packageRoot,
				packageName,
			},
			vscode.TreeItemCollapsibleState.Collapsed,
			{
				description,
				tooltip: packageRoot?.fsPath,
			},
		);

		const workspaceNode = this.nodesById.get(this.workspaceId(workspaceRoot));
		if (workspaceNode) {
			workspaceNode.children.set(node.id, node);
		} else {
			this.pushRootNode(node);
		}

		return this.registerNode(node);
	}

	private ensureSuiteNode(
		workspaceRoot: vscode.Uri,
		packageName: string,
		suiteName: string,
		suiteUri?: vscode.Uri,
	): RiotBenchmarkNode {
		const id = this.suiteId(workspaceRoot, packageName, suiteName);
		const existing = this.nodesById.get(id);
		if (existing) {
			return existing;
		}

		const packageNode = this.ensurePackageNode(workspaceRoot, packageName);
		const node = new RiotBenchmarkNode(
			id,
			suiteName,
			{
				kind: "suite",
				workspaceRoot,
				packageRoot: packageNode.meta.packageRoot,
				packageName,
				suiteName,
				suiteUri,
			},
			vscode.TreeItemCollapsibleState.Collapsed,
			{
				resourceUri: suiteUri,
				tooltip: suiteUri?.fsPath,
			},
		);
		packageNode.children.set(node.id, node);
		return this.registerNode(node);
	}

	private ensureItemNode(
		workspaceRoot: vscode.Uri,
		packageName: string,
		suiteName: string,
		kind: "benchmark" | "comparison",
		name: string,
	): RiotBenchmarkNode {
		const id = this.itemId(workspaceRoot, packageName, suiteName, kind, name);
		const existing = this.nodesById.get(id);
		if (existing) {
			return existing;
		}

		const suiteNode = this.ensureSuiteNode(workspaceRoot, packageName, suiteName);
		const node = new RiotBenchmarkNode(
			id,
			name,
			{
				kind,
				workspaceRoot,
				packageRoot: suiteNode.meta.packageRoot,
				packageName,
				suiteName,
				suiteUri: suiteNode.meta.suiteUri,
				caseQuery: name,
			},
			vscode.TreeItemCollapsibleState.None,
		);
		suiteNode.children.set(node.id, node);
		return this.registerNode(node);
	}

	private setRunning(node: RiotBenchmarkNode, description = "running"): void {
		node.description = description;
		node.iconPath = runningIcon;
	}

	private setFailed(node: RiotBenchmarkNode, description: string, tooltip?: string): void {
		node.description = description;
		node.iconPath = failedIcon;
		node.tooltip = tooltip ?? node.baseTooltip;
	}

	private setSkipped(node: RiotBenchmarkNode, description: string, tooltip?: string): void {
		node.description = description;
		node.iconPath = skippedIcon;
		node.tooltip = tooltip ?? node.baseTooltip;
	}

	private setCompleted(node: RiotBenchmarkNode, description?: string, tooltip?: string): void {
		node.description = description ?? node.baseDescription;
		node.iconPath = node.defaultIcon;
		node.tooltip = tooltip ?? node.baseTooltip;
	}

	private resetRunState(node: RiotBenchmarkNode): void {
		node.resetPresentation();
		for (const child of node.children.values()) {
			this.resetRunState(child);
		}
	}

	private clearPendingRunMarkers(node: RiotBenchmarkNode): void {
		if (node.iconPath === runningIcon) {
			node.resetPresentation();
		}
		for (const child of node.children.values()) {
			this.clearPendingRunMarkers(child);
		}
	}

	private beginNodeRun(node: RiotBenchmarkNode): vscode.CancellationTokenSource {
		const source = new vscode.CancellationTokenSource();
		this.cancellationByNodeId.set(node.id, source);
		node.setActionRunning(true);
		return source;
	}

	private endNodeRun(node: RiotBenchmarkNode): void {
		const source = this.cancellationByNodeId.get(node.id);
		if (source) {
			source.dispose();
			this.cancellationByNodeId.delete(node.id);
		}
		node.setActionRunning(false);
	}

	private async refresh(): Promise<void> {
		this.clearTree();
		this.fire();

		if (!(await ensureRiotAvailable(this.context, { prompt: false }))) {
			return;
		}

		const discovery = await discoverRiotRoots(this.context);
		if (discovery.manifests.length === 0) {
			return;
		}

		for (const workspace of discovery.workspaces) {
			this.ensureWorkspaceNode(workspace.root);
			for (const pkg of workspace.packages) {
				this.ensurePackageNode(workspace.root, pkg.name, pkg.root, pkg.relativePath);
			}
		}

		for (const standalone of discovery.standalonePackages) {
			for (const pkg of standalone.packages) {
				this.ensurePackageNode(standalone.root, pkg.name, pkg.root, pkg.relativePath);
			}
		}

		this.fire();

		const listedRoots = [
			...discovery.workspaces.map((manifest) => manifest.root),
			...discovery.standalonePackages.map((manifest) => manifest.root),
		];
		await Promise.all(
			listedRoots.map((root) => this.discoverSuitesForRoot(root, discovery.packageRoots)),
		);
		this.fire();
	}

	private async discoverSuitesForRoot(
		root: vscode.Uri,
		packageRoots: Map<string, vscode.Uri>,
	): Promise<void> {
		let errorMessage: string | undefined;
		const result = await runRiotStreaming(this.context, ["bench", "--list", "--json"], {
			cwd: root.fsPath,
			onStdoutLine: (line) => {
				const event = jsonObjectFromLine(line);
				if (!event) {
					return;
				}

				const eventType = stringField(event.type);
				switch (eventType) {
					case "BenchSuiteListed": {
						const packageName = stringField(event.package);
						const suiteName = stringField(event.suite);
						if (!packageName || !suiteName) {
							return;
						}

						const packageRoot = packageRoots.get(riotPackageKey(root, packageName));
						const suitePath = stringField(event.path);
						const suiteUri = suitePath
							? vscode.Uri.file(path.join(root.fsPath, suitePath))
							: packageRoot;
						this.ensurePackageNode(root, packageName, packageRoot);
						this.ensureSuiteNode(root, packageName, suiteName, suiteUri);
						return;
					}
					case "BenchItemListed": {
						const packageName = stringField(event.package);
						const suiteName = stringField(event.suite);
						const benchmark = objectField(event.benchmark);
						const name = stringField(benchmark?.name ?? event.name);
						const kindName = stringField(benchmark?.kind);
						if (!packageName || !suiteName || !name) {
							return;
						}

						const kind = kindName === "comparison" ? "comparison" : "benchmark";
						this.ensureItemNode(root, packageName, suiteName, kind, name);
						return;
					}
					case "BenchSuiteListFailed": {
						const packageName = stringField(event.package) ?? "<unknown>";
						const suiteName = stringField(event.suite) ?? "<unknown>";
						const message = cleanOutput(stringField(event.message) ?? "unknown failure");
						this.output.appendLine(
							`Failed to list Riot benchmark suite ${packageName}:${suiteName} in ${root.fsPath}: ${message}`,
						);
						return;
					}
					case "bench.error":
						errorMessage = stringField(event.message);
						return;
					default:
						return;
				}
			},
			onStderrLine: (line) => {
				if (line.trim() !== "") {
					this.output.appendLine(cleanOutput(line));
				}
			},
		});

		if (result.code !== 0) {
			this.output.appendLine(
				`Failed to list Riot benchmarks in ${root.fsPath}: ${cleanOutput(errorMessage || result.stderr.trim() || result.stdout.trim() || `exit ${result.code}`)}`,
			);
		}
	}

	private commandFor(node: RiotBenchmarkNode): { cwd: string; args: string[] } | undefined {
		const meta = node.meta;
		const cwd = meta.workspaceRoot.fsPath;
		switch (meta.kind) {
			case "workspace":
				return { cwd, args: ["bench", "--json"] };
			case "package":
				if (!meta.packageName) {
					return undefined;
				}
				if (meta.packageRoot && meta.packageRoot.fsPath === meta.workspaceRoot.fsPath) {
					return { cwd, args: ["bench", "--json"] };
				}
				return { cwd, args: ["bench", "--json", "-p", meta.packageName] };
			case "suite":
				if (!meta.packageName || !meta.suiteName) {
					return undefined;
				}
				return { cwd, args: ["bench", "--json", `${meta.packageName}:${meta.suiteName}`] };
			case "benchmark":
			case "comparison":
				if (!meta.packageName || !meta.suiteName || !meta.caseQuery) {
					return undefined;
				}
				return {
					cwd,
					args: ["bench", "--json", `${meta.packageName}:${meta.suiteName}:${meta.caseQuery}`],
				};
		}
	}

	private packageAggregateFor(state: RunState, node: RiotBenchmarkNode): PackageAggregate {
		const existing = state.packageAggregates.get(node.id);
		if (existing) {
			return existing;
		}

		const aggregate: PackageAggregate = {
			node,
			totalSuites: 0,
			failedSuites: 0,
			skippedSuites: 0,
		};
		state.packageAggregates.set(node.id, aggregate);
		return aggregate;
	}

	private recordBenchmarkHistory(
		rootNode: RiotBenchmarkNode,
		packageName: string,
		suiteName: string,
		benchmark: JsonObject,
		recordedAt: number,
	): void {
		const name = stringField(benchmark.name);
		const status = stringField(benchmark.status);
		if (!name || (status !== "completed" && status !== "failed" && status !== "skipped")) {
			return;
		}

		const statistics = objectField(benchmark.statistics);
		const selector = `${packageName}:${suiteName}:${name}`;
		const record: BenchmarkHistoryRecord = {
			id: `${selector}:${recordedAt}`,
			recordedAt,
			workspaceRoot: rootNode.meta.workspaceRoot.fsPath,
			packageName,
			suiteName,
			itemName: name,
			itemKind: "benchmark",
			selector,
			status,
			meanNanos: numberField(statistics?.mean_nanos),
			medianNanos: numberField(statistics?.median_nanos),
			minNanos: numberField(statistics?.min_nanos),
			maxNanos: numberField(statistics?.max_nanos),
			stdDevNanos: numberField(statistics?.std_dev_nanos),
			totalTimeNanos: numberField(statistics?.total_time_nanos),
			iterations: numberField(statistics?.iterations),
			message: cleanOutput(stringField(benchmark.message) ?? ""),
		};
		this.resultsView.record(record);
	}

	private recordComparisonHistory(
		rootNode: RiotBenchmarkNode,
		packageName: string,
		suiteName: string,
		comparison: JsonObject,
		recordedAt: number,
	): void {
		const description = stringField(comparison.description);
		if (!description) {
			return;
		}

		const selector = `${packageName}:${suiteName}:${description}`;
		const record: BenchmarkHistoryRecord = {
			id: `${selector}:${recordedAt}`,
			recordedAt,
			workspaceRoot: rootNode.meta.workspaceRoot.fsPath,
			packageName,
			suiteName,
			itemName: description,
			itemKind: "comparison",
			selector,
			status: "completed",
			fastest: stringField(comparison.fastest),
			cases: arrayField(comparison.case_results)
				.map((caseJson) => {
					const caseResult = objectField(caseJson);
					const statistics = objectField(caseResult?.statistics);
					const name = stringField(caseResult?.name);
					const meanNanos = numberField(statistics?.mean_nanos);
					return name && typeof meanNanos === "number" ? { name, meanNanos } : undefined;
				})
				.filter((value): value is { name: string; meanNanos: number } => value !== undefined),
			ratios: arrayField(comparison.speedup_ratios)
				.map((ratioJson) => {
					const ratio = objectField(ratioJson);
					const name = stringField(ratio?.name);
					const value = numberField(ratio?.ratio);
					return name && typeof value === "number" ? { name, ratio: value } : undefined;
				})
				.filter((value): value is { name: string; ratio: number } => value !== undefined),
		};
		this.resultsView.record(record);
	}

	private applyBenchmarkResult(
		rootNode: RiotBenchmarkNode,
		itemNode: RiotBenchmarkNode,
		benchmark: JsonObject,
		state: RunState,
	): void {
		const status = stringField(benchmark.status);
		if (rootNode.id === itemNode.id) {
			state.matchedCase = true;
		}

		switch (status) {
			case "completed":
				this.setCompleted(itemNode, benchmarkDescription(benchmark), benchmarkTooltip(benchmark));
				return;
			case "skipped":
				this.setSkipped(itemNode, "skipped", benchmarkTooltip(benchmark));
				return;
			case "failed":
				this.setFailed(
					itemNode,
					"failed",
					benchmarkTooltip(benchmark),
				);
				return;
			default:
				this.setFailed(itemNode, "unknown", benchmarkTooltip(benchmark));
		}
	}

	private applyComparisonResult(
		rootNode: RiotBenchmarkNode,
		itemNode: RiotBenchmarkNode,
		comparison: JsonObject,
		state: RunState,
	): void {
		if (rootNode.id === itemNode.id) {
			state.matchedCase = true;
		}

		this.setCompleted(
			itemNode,
			comparisonDescription(comparison),
			comparisonTooltip(comparison),
		);
	}

	private handleEvent(rootNode: RiotBenchmarkNode, state: RunState, event: JsonObject): void {
		const eventType = stringField(event.type);
		switch (eventType) {
			case "NoBenchSuitesFound":
				state.noSuitesFound = true;
				return;
			case "RunningBenchSuite": {
				const packageName = stringField(event.package);
				const suiteName = stringField(event.suite);
				if (!packageName || !suiteName) {
					return;
				}

				const packageNode = this.ensurePackageNode(rootNode.meta.workspaceRoot, packageName);
				const suiteNode = this.ensureSuiteNode(rootNode.meta.workspaceRoot, packageName, suiteName);
				this.setRunning(packageNode);
				this.setRunning(suiteNode);
				return;
			}
			case "BenchSuiteCompleted": {
				const packageName = stringField(event.package);
				const suiteName = stringField(event.suite);
				const summary = objectField(event.summary);
				if (!packageName || !suiteName || !summary) {
					return;
				}
				const recordedAt = Date.now();

				state.sawSuiteCompleted = true;
				state.totalSuites += 1;

				const packageNode = this.ensurePackageNode(rootNode.meta.workspaceRoot, packageName);
				const suiteNode = this.ensureSuiteNode(rootNode.meta.workspaceRoot, packageName, suiteName);
				const aggregate = this.packageAggregateFor(state, packageNode);
				aggregate.totalSuites += 1;

				const failed = numberField(summary.failed) ?? 0;
				const skipped = numberField(summary.skipped) ?? 0;
				const total = numberField(summary.total) ?? 0;

				if (failed > 0) {
					state.failedSuites += 1;
					aggregate.failedSuites += 1;
					this.setFailed(suiteNode, suiteSummaryDescription(summary), formatSuiteReport(event));
				} else if (total > 0 && skipped === total) {
					state.skippedSuites += 1;
					aggregate.skippedSuites += 1;
					this.setSkipped(suiteNode, "all skipped", formatSuiteReport(event));
				} else {
					this.setCompleted(suiteNode, suiteSummaryDescription(summary), formatSuiteReport(event));
				}

				for (const benchmarkJson of arrayField(event.benchmarks)) {
					const benchmark = objectField(benchmarkJson);
					const name = stringField(benchmark?.name);
					if (!benchmark || !name) {
						continue;
					}

					const itemNode = this.ensureItemNode(
						rootNode.meta.workspaceRoot,
						packageName,
						suiteName,
						"benchmark",
						name,
					);
					this.applyBenchmarkResult(rootNode, itemNode, benchmark, state);
					this.recordBenchmarkHistory(rootNode, packageName, suiteName, benchmark, recordedAt);
				}

				for (const comparisonJson of arrayField(event.comparisons)) {
					const comparison = objectField(comparisonJson);
					const description = stringField(comparison?.description);
					if (!comparison || !description) {
						continue;
					}

					const itemNode = this.ensureItemNode(
						rootNode.meta.workspaceRoot,
						packageName,
						suiteName,
						"comparison",
						description,
					);
					this.applyComparisonResult(rootNode, itemNode, comparison, state);
					this.recordComparisonHistory(rootNode, packageName, suiteName, comparison, recordedAt);
				}

				this.output.appendLine(formatSuiteReport(event).trimEnd());
				this.output.appendLine("");

				const stdout = stringField(event.stdout);
				if (stdout && stdout.trim() !== "") {
					this.output.append(suiteOutput(`${packageName}:${suiteName}`, "stdout", stdout));
				}

				const stderr = stringField(event.stderr);
				if (stderr && stderr.trim() !== "") {
					this.output.append(suiteOutput(`${packageName}:${suiteName}`, "stderr", stderr));
				}
				return;
			}
			case "BenchSummary":
				state.summary = {
					total: numberField(event.total) ?? 0,
					completed: numberField(event.completed) ?? 0,
					skipped: numberField(event.skipped) ?? 0,
					failed: numberField(event.failed) ?? 0,
				};
				return;
			case "bench.error":
				state.lastErrorMessage = cleanOutput(stringField(event.message) ?? "Riot benchmark failed.");
				this.output.appendLine("[bench.error]");
				this.output.appendLine(state.lastErrorMessage);
				this.output.appendLine("");
				return;
			default:
				return;
		}
	}

	private finalizePackageAggregates(state: RunState): void {
		for (const aggregate of state.packageAggregates.values()) {
			if (aggregate.failedSuites > 0) {
				this.setFailed(
					aggregate.node,
					`${aggregate.failedSuites} suite(s) failed`,
					aggregate.node.baseTooltip,
				);
				continue;
			}

			if (aggregate.totalSuites > 0 && aggregate.skippedSuites === aggregate.totalSuites) {
				this.setSkipped(aggregate.node, "all skipped", aggregate.node.baseTooltip);
				continue;
			}

			if (aggregate.totalSuites > 0) {
				this.setCompleted(
					aggregate.node,
					`${aggregate.totalSuites} suite(s)`,
					aggregate.node.baseTooltip,
				);
			}
		}
	}

	private finalizeRootNode(
		rootNode: RiotBenchmarkNode,
		state: RunState,
		result: { code: number; stdout: string; stderr: string },
	): void {
		const errorMessage =
			state.lastErrorMessage
			|| result.stderr.trim()
			|| result.stdout.trim()
			|| "Riot benchmark failed.";
		const summary = state.summary;

		switch (rootNode.meta.kind) {
			case "workspace":
			case "package":
				if (!state.sawSuiteCompleted && result.code !== 0) {
					this.setFailed(rootNode, "failed", errorMessage);
					return;
				}

				if (state.noSuitesFound || !state.sawSuiteCompleted) {
					this.setSkipped(rootNode, "no benchmarks", rootNode.baseTooltip);
					return;
				}

				if (summary) {
					if (summary.failed > 0) {
						this.setFailed(rootNode, runSummaryDescription(summary), rootNode.baseTooltip);
						return;
					}

					if (summary.total > 0 && summary.skipped === summary.total) {
						this.setSkipped(rootNode, "all skipped", rootNode.baseTooltip);
						return;
					}

					this.setCompleted(rootNode, runSummaryDescription(summary), rootNode.baseTooltip);
					return;
				}

				if (state.failedSuites > 0) {
					this.setFailed(rootNode, `${state.failedSuites} suite(s) failed`, rootNode.baseTooltip);
				} else if (state.totalSuites > 0 && state.skippedSuites === state.totalSuites) {
					this.setSkipped(rootNode, "all skipped", rootNode.baseTooltip);
				} else {
					this.setCompleted(rootNode, `${state.totalSuites} suite(s)`, rootNode.baseTooltip);
				}
				return;
			case "suite":
				if (state.noSuitesFound) {
					this.setSkipped(rootNode, "no benchmarks", rootNode.baseTooltip);
					return;
				}

				if (!state.sawSuiteCompleted && result.code !== 0) {
					this.setFailed(rootNode, "failed", errorMessage);
				}
				return;
			case "benchmark":
			case "comparison":
				if (state.noSuitesFound) {
					this.setSkipped(rootNode, "not found", rootNode.baseTooltip);
					return;
				}

				if (!state.matchedCase && result.code !== 0) {
					this.setFailed(rootNode, "failed", errorMessage);
					return;
				}

				if (!state.matchedCase) {
					this.setSkipped(rootNode, "not matched", rootNode.baseTooltip);
				}
		}
	}

	private async stop(node?: RiotBenchmarkNode): Promise<void> {
		if (!node) {
			void vscode.window.showWarningMessage("Select a running Riot benchmark to stop.");
			return;
		}

		const source = this.cancellationByNodeId.get(node.id);
		if (!source) {
			void vscode.window.showWarningMessage("That Riot benchmark is not currently running.");
			return;
		}

		this.clearPendingRunMarkers(node);
		this.setSkipped(node, "stopping", node.baseTooltip);
		this.fire();
		source.cancel();
		this.output.appendLine(`Stopped benchmark run for ${node.label?.toString() ?? "selection"}.`);
		this.output.appendLine("");
	}

	private async run(node?: RiotBenchmarkNode): Promise<void> {
		if (!node) {
			void vscode.window.showWarningMessage("Select a Riot benchmark to run.");
			return;
		}

		if (!(await ensureRiotAvailable(this.context))) {
			return;
		}

		const command = this.commandFor(node);
		if (!command) {
			void vscode.window.showErrorMessage("Could not derive Riot benchmark command.");
			return;
		}

		if (this.cancellationByNodeId.has(node.id)) {
			void vscode.window.showWarningMessage("That Riot benchmark is already running.");
			return;
		}

		this.resetRunState(node);
		this.setRunning(node);
		const cancellation = this.beginNodeRun(node);
		this.fire(node);

		this.output.appendLine(
			`$ (cd ${quote(command.cwd)} && ${["riot", ...command.args].map(quote).join(" ")})`,
		);
		this.output.appendLine("");
		this.output.show(true);
		void this.resultsView.reveal();

		const state: RunState = {
			packageAggregates: new Map<string, PackageAggregate>(),
			totalSuites: 0,
			failedSuites: 0,
			skippedSuites: 0,
			sawSuiteCompleted: false,
			noSuitesFound: false,
			matchedCase: false,
			lastErrorMessage: undefined,
			summary: undefined,
		};

		const result = await runRiotStreaming(this.context, command.args, {
			cwd: command.cwd,
			cancellation: cancellation.token,
			onStdoutLine: (line) => {
				const event = jsonObjectFromLine(line);
				if (event) {
					this.handleEvent(node, state, event);
					this.fire();
					return;
				}

				if (line.trim() !== "") {
					this.output.appendLine(cleanOutput(line));
				}
			},
			onStderrLine: (line) => {
				if (line.trim() !== "") {
					this.output.appendLine(cleanOutput(line));
				}
			},
		});

		this.endNodeRun(node);
		if (cancellation.token.isCancellationRequested) {
			this.clearPendingRunMarkers(node);
			this.setSkipped(node, "stopped", node.baseTooltip);
			this.fire();
			return;
		}
		this.finalizePackageAggregates(state);
		this.finalizeRootNode(node, state, result);
		this.fire();
	}
}
