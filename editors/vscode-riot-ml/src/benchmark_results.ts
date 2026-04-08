import * as vscode from "vscode";

export type BenchmarkSelectionKind = "workspace" | "package" | "suite" | "benchmark" | "comparison";
export type BenchmarkRecordKind = "benchmark" | "comparison";
export type BenchmarkRecordStatus = "completed" | "failed" | "skipped";

export interface BenchmarkResultsSelection {
	kind: BenchmarkSelectionKind;
	workspaceRoot: string;
	packageName?: string;
	suiteName?: string;
	itemName?: string;
	itemKind?: BenchmarkRecordKind;
	label: string;
}

export interface BenchmarkHistoryCase {
	name: string;
	meanNanos: number;
}

export interface BenchmarkHistoryRatio {
	name: string;
	ratio: number;
}

export interface BenchmarkHistoryRecord {
	id: string;
	recordedAt: number;
	workspaceRoot: string;
	packageName: string;
	suiteName: string;
	itemName: string;
	itemKind: BenchmarkRecordKind;
	selector: string;
	status: BenchmarkRecordStatus;
	meanNanos?: number;
	medianNanos?: number;
	minNanos?: number;
	maxNanos?: number;
	stdDevNanos?: number;
	totalTimeNanos?: number;
	iterations?: number;
	message?: string;
	fastest?: string;
	cases?: BenchmarkHistoryCase[];
	ratios?: BenchmarkHistoryRatio[];
}

const storageKey = "riot.benchmarkResults.history.v1";
const maxRecordsPerSelector = 40;

const escapeHtml = (value: string): string =>
	value
		.replace(/&/g, "&amp;")
		.replace(/</g, "&lt;")
		.replace(/>/g, "&gt;")
		.replace(/\"/g, "&quot;")
		.replace(/'/g, "&#39;");

const formatTimestamp = (value: number): string =>
	new Date(value).toLocaleString();

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

const formatRatio = (value: number | undefined): string =>
	typeof value === "number" && Number.isFinite(value) ? `${value.toFixed(2)}x` : "unknown";

const selectorTitle = (selection?: BenchmarkResultsSelection): string => {
	if (!selection) {
		return "Recent Benchmark Runs";
	}

	return selection.label;
};

const inSelectionScope = (
	selection: BenchmarkResultsSelection | undefined,
	record: BenchmarkHistoryRecord,
): boolean => {
	if (!selection) {
		return true;
	}

	if (record.workspaceRoot !== selection.workspaceRoot) {
		return false;
	}

	switch (selection.kind) {
		case "workspace":
			return true;
		case "package":
			return record.packageName === selection.packageName;
		case "suite":
			return record.packageName === selection.packageName && record.suiteName === selection.suiteName;
		case "benchmark":
		case "comparison":
			return (
				record.packageName === selection.packageName
				&& record.suiteName === selection.suiteName
				&& record.itemName === selection.itemName
				&& record.itemKind === selection.itemKind
			);
	}
};

const isItemSelection = (selection?: BenchmarkResultsSelection): boolean =>
	selection?.kind === "benchmark" || selection?.kind === "comparison";

const historyGraph = (records: BenchmarkHistoryRecord[]): string => {
	const points = records
		.filter((record) => record.status === "completed" && typeof record.meanNanos === "number")
		.slice(-20);
	if (points.length < 2) {
		return `<div class="empty-graph">Run this benchmark a few times to see a history graph.</div>`;
	}

	const width = 640;
	const height = 220;
	const padding = 24;
	const values = points.map((record) => record.meanNanos as number);
	const min = Math.min(...values);
	const max = Math.max(...values);
	const span = Math.max(1, max - min);
	const plotWidth = width - padding * 2;
	const plotHeight = height - padding * 2;
	const polyline = points
		.map((record, index) => {
			const x = padding + (index / Math.max(1, points.length - 1)) * plotWidth;
			const normalized = ((record.meanNanos as number) - min) / span;
			const y = height - padding - normalized * plotHeight;
			return `${x.toFixed(2)},${y.toFixed(2)}`;
		})
		.join(" ");
	const dots = points
		.map((record, index) => {
			const x = padding + (index / Math.max(1, points.length - 1)) * plotWidth;
			const normalized = ((record.meanNanos as number) - min) / span;
			const y = height - padding - normalized * plotHeight;
			return `<circle cx="${x.toFixed(2)}" cy="${y.toFixed(2)}" r="3.5"><title>${escapeHtml(`${formatTimestamp(record.recordedAt)}\nmean ${formatDurationNanos(record.meanNanos)}`)}</title></circle>`;
		})
		.join("");

	return `
		<div class="graph-card">
			<div class="graph-header">
				<span>Mean over time</span>
				<span>${escapeHtml(formatDurationNanos(min))} to ${escapeHtml(formatDurationNanos(max))}</span>
			</div>
			<svg viewBox="0 0 ${width} ${height}" class="graph" aria-label="Benchmark history graph">
				<line x1="${padding}" y1="${height - padding}" x2="${width - padding}" y2="${height - padding}" class="axis" />
				<line x1="${padding}" y1="${padding}" x2="${padding}" y2="${height - padding}" class="axis" />
				<polyline points="${polyline}" class="line" />
				${dots}
			</svg>
		</div>
	`;
};

const benchmarkStatsPanel = (record: BenchmarkHistoryRecord | undefined): string => {
	if (!record) {
		return `<div class="empty-state">Run this benchmark once to populate the overall stats card.</div>`;
	}

	return `
		<div class="stats-list">
			<div class="stats-row"><span class="label">Mean</span><span class="value">${escapeHtml(formatDurationNanos(record.meanNanos))}</span></div>
			<div class="stats-row"><span class="label">Median</span><span class="value">${escapeHtml(formatDurationNanos(record.medianNanos))}</span></div>
			<div class="stats-row"><span class="label">Min</span><span class="value">${escapeHtml(formatDurationNanos(record.minNanos))}</span></div>
			<div class="stats-row"><span class="label">Max</span><span class="value">${escapeHtml(formatDurationNanos(record.maxNanos))}</span></div>
			<div class="stats-row"><span class="label">Std Dev</span><span class="value">${escapeHtml(formatDurationNanos(record.stdDevNanos))}</span></div>
			<div class="stats-row"><span class="label">Iterations</span><span class="value">${escapeHtml(String(record.iterations ?? 0))}</span></div>
			<div class="stats-row"><span class="label">Total</span><span class="value">${escapeHtml(formatDurationNanos(record.totalTimeNanos))}</span></div>
			<div class="stats-row"><span class="label">Recorded</span><span class="value">${escapeHtml(formatTimestamp(record.recordedAt))}</span></div>
		</div>
	`;
};

const benchmarkHistoryTable = (records: BenchmarkHistoryRecord[]): string => {
	if (records.length === 0) {
		return `<div class="empty-state">No benchmark runs recorded for this selection yet.</div>`;
	}

	const rows = records
		.slice()
		.sort((left, right) => right.recordedAt - left.recordedAt)
		.slice(0, 20)
		.map((record) => `
			<tr>
				<td>${escapeHtml(formatTimestamp(record.recordedAt))}</td>
				<td>${escapeHtml(record.status)}</td>
				<td>${escapeHtml(formatDurationNanos(record.meanNanos))}</td>
				<td>${escapeHtml(formatDurationNanos(record.medianNanos))}</td>
				<td>${escapeHtml(formatDurationNanos(record.minNanos))}</td>
				<td>${escapeHtml(formatDurationNanos(record.maxNanos))}</td>
				<td>${escapeHtml(String(record.iterations ?? 0))}</td>
			</tr>
		`)
		.join("");

	return `
		<div class="table-scroll">
			<table>
				<thead>
					<tr>
						<th>Recorded</th>
						<th>Status</th>
						<th>Mean</th>
						<th>Median</th>
						<th>Min</th>
						<th>Max</th>
						<th>Iterations</th>
					</tr>
				</thead>
				<tbody>${rows}</tbody>
			</table>
		</div>
	`;
};

const comparisonDetails = (record: BenchmarkHistoryRecord | undefined): string => {
	if (!record || record.itemKind !== "comparison") {
		return `<div class="empty-state">Select a comparison benchmark to inspect its latest case breakdown.</div>`;
	}

	const cases = record.cases ?? [];
	const ratios = record.ratios ?? [];

	const caseRows = cases.length === 0
		? `<tr><td colspan="2">No case measurements recorded.</td></tr>`
		: cases
			.map((item) => `
				<tr>
					<td>${escapeHtml(item.name)}</td>
					<td>${escapeHtml(formatDurationNanos(item.meanNanos))}</td>
				</tr>
			`)
			.join("");

	const ratioRows = ratios.length === 0
		? ""
		: `
			<h3>Relative Speed</h3>
			<table>
				<thead>
					<tr>
						<th>Case</th>
						<th>Fastest vs case</th>
					</tr>
				</thead>
				<tbody>
					${ratios.map((ratio) => `
						<tr>
							<td>${escapeHtml(ratio.name)}</td>
							<td>${escapeHtml(formatRatio(ratio.ratio))}</td>
						</tr>
					`).join("")}
				</tbody>
			</table>
		`;

	return `
		<h3>Latest Comparison</h3>
		<div class="table-scroll">
			<table>
				<thead>
					<tr>
						<th>Case</th>
						<th>Mean</th>
					</tr>
				</thead>
				<tbody>${caseRows}</tbody>
			</table>
		</div>
		${ratioRows}
	`;
};

const scopeTable = (records: BenchmarkHistoryRecord[]): string => {
	if (records.length === 0) {
		return `<div class="empty-state">No benchmark runs recorded for this scope yet.</div>`;
	}

	const rows = records
		.slice()
		.sort((left, right) => right.recordedAt - left.recordedAt)
		.slice(0, 30)
		.map((record) => `
			<tr>
				<td>${escapeHtml(formatTimestamp(record.recordedAt))}</td>
				<td>${escapeHtml(record.packageName)}</td>
				<td>${escapeHtml(record.suiteName)}</td>
				<td>${escapeHtml(record.itemName)}</td>
				<td>${escapeHtml(record.status)}</td>
				<td>${escapeHtml(record.itemKind === "comparison" ? record.fastest ?? "comparison" : formatDurationNanos(record.meanNanos))}</td>
			</tr>
		`)
		.join("");

	return `
		<div class="table-scroll">
			<table>
				<thead>
					<tr>
						<th>Recorded</th>
						<th>Package</th>
						<th>Suite</th>
						<th>Item</th>
						<th>Status</th>
						<th>Summary</th>
					</tr>
				</thead>
				<tbody>${rows}</tbody>
			</table>
		</div>
	`;
};

const renderHtml = (
	selection: BenchmarkResultsSelection | undefined,
	records: BenchmarkHistoryRecord[],
): string => {
	const scoped = records.filter((record) => inSelectionScope(selection, record));
	const exactRecords = isItemSelection(selection)
		? scoped.filter((record) => record.itemKind === selection?.itemKind && record.itemName === selection?.itemName)
		: scoped;
	const latest = exactRecords
		.slice()
		.sort((left, right) => right.recordedAt - left.recordedAt)[0];
	const content = !selection || selection.kind === "workspace" || selection.kind === "package" || selection.kind === "suite"
		? `
			<div class="section note">
				Select an individual benchmark item in the Riot Benchmarks tree to see its history graph.
			</div>
			<div class="section">
				<h2>Recent Runs</h2>
				${scopeTable(scoped)}
			</div>
		`
		: selection.kind === "comparison"
			? `
				<div class="metrics">
					<div class="metric"><span class="label">Fastest</span><span class="value">${escapeHtml(latest?.fastest ?? "unknown")}</span></div>
					<div class="metric"><span class="label">Recorded</span><span class="value">${escapeHtml(latest ? formatTimestamp(latest.recordedAt) : "unknown")}</span></div>
				</div>
				<div class="section">
					${comparisonDetails(latest)}
				</div>
				<div class="section">
					<h3>History</h3>
					${scopeTable(exactRecords)}
				</div>
			`
			: `
				<div class="benchmark-dashboard">
					<section class="panel-card panel-stats">
						<h2>Overall Stats</h2>
						${benchmarkStatsPanel(latest)}
					</section>
					<section class="panel-card panel-graph">
						<h2>Graph</h2>
						${historyGraph(exactRecords)}
					</section>
					<section class="panel-card panel-history">
						<h2>Run History</h2>
						${benchmarkHistoryTable(exactRecords)}
					</section>
				</div>
			`;

	return `<!DOCTYPE html>
	<html lang="en">
	<head>
		<meta charset="UTF-8" />
		<meta name="viewport" content="width=device-width, initial-scale=1.0" />
		<title>Riot Benchmark Results</title>
		<style>
			:root {
				color-scheme: light dark;
			}
			body {
				font-family: var(--vscode-font-family);
				font-size: var(--vscode-font-size);
				color: var(--vscode-foreground);
				background: var(--vscode-editor-background);
				margin: 0;
				padding: 10px 14px 14px;
				box-sizing: border-box;
				min-height: 100vh;
				overflow: hidden;
			}
			h1, h2, h3 {
				font-weight: 600;
				margin: 0 0 8px;
			}
			h1 {
				font-size: 1.1rem;
			}
			h2 {
				font-size: 1rem;
				margin-top: 0;
			}
			h3 {
				font-size: 0.95rem;
				margin-top: 0;
			}
			.section {
				margin-top: 12px;
				padding: 12px 14px;
				border: 1px solid var(--vscode-widget-border);
				border-radius: 10px;
				background: color-mix(in srgb, var(--vscode-sideBar-background) 82%, transparent);
				min-height: 0;
			}
			.note {
				color: var(--vscode-descriptionForeground);
			}
			.benchmark-dashboard {
				display: grid;
				grid-template-columns: 220px minmax(460px, 2fr) minmax(320px, 1fr);
				gap: 12px;
				margin-top: 10px;
				align-items: start;
				height: calc(100vh - 54px);
			}
			.panel-card {
				min-height: 0;
				height: 100%;
				padding: 12px 14px;
				border: 1px solid var(--vscode-widget-border);
				border-radius: 10px;
				background: color-mix(in srgb, var(--vscode-sideBar-background) 82%, transparent);
				display: flex;
				flex-direction: column;
				overflow: hidden;
			}
			.panel-card h2 {
				margin-bottom: 8px;
			}
			.panel-history {
				overflow: hidden;
			}
			.metrics {
				display: grid;
				grid-template-columns: repeat(auto-fit, minmax(140px, 1fr));
				gap: 10px;
				margin-top: 14px;
			}
			.metric {
				padding: 12px;
				border: 1px solid var(--vscode-widget-border);
				border-radius: 10px;
				background: color-mix(in srgb, var(--vscode-editorWidget-background) 82%, transparent);
			}
			.metric .label {
				display: block;
				color: var(--vscode-descriptionForeground);
				font-size: 0.85rem;
				margin-bottom: 6px;
			}
			.metric .value {
				font-size: 1rem;
				font-weight: 600;
			}
			.stats-list {
				display: grid;
				gap: 2px;
			}
			.stats-row {
				display: flex;
				justify-content: space-between;
				gap: 12px;
				padding: 6px 0;
				border-bottom: 1px solid var(--vscode-widget-border);
			}
			.stats-row:last-child {
				border-bottom: none;
			}
			.stats-row .label {
				color: var(--vscode-descriptionForeground);
			}
			.stats-row .value {
				font-weight: 600;
				text-align: right;
			}
			table {
				width: 100%;
				border-collapse: collapse;
			}
			th, td {
				text-align: left;
				padding: 8px 10px;
				border-bottom: 1px solid var(--vscode-widget-border);
				vertical-align: top;
			}
			th {
				color: var(--vscode-descriptionForeground);
				font-weight: 600;
				font-size: 0.9rem;
			}
			.graph-card {
				display: flex;
				flex-direction: column;
				gap: 8px;
				height: 100%;
				min-height: 0;
			}
			.graph-header {
				display: flex;
				justify-content: space-between;
				gap: 12px;
				color: var(--vscode-descriptionForeground);
			}
			.graph {
				width: 100%;
				height: 170px;
				background: color-mix(in srgb, var(--vscode-editorWidget-background) 84%, transparent);
				border: 1px solid var(--vscode-widget-border);
				border-radius: 10px;
				flex: 0 0 auto;
			}
			.table-scroll {
				min-height: 0;
				overflow: auto;
				flex: 1;
			}
			.axis {
				stroke: var(--vscode-widget-border);
				stroke-width: 1;
			}
			.line {
				fill: none;
				stroke: var(--vscode-charts-blue);
				stroke-width: 3;
				stroke-linejoin: round;
				stroke-linecap: round;
			}
			circle {
				fill: var(--vscode-charts-blue);
			}
			.empty-state,
			.empty-graph {
				color: var(--vscode-descriptionForeground);
			}
			@media (max-width: 1180px) {
				body {
					overflow: auto;
				}
				.benchmark-dashboard {
					grid-template-columns: 1fr;
					height: auto;
				}
				.graph {
					height: 190px;
				}
			}
		</style>
	</head>
	<body>
		<h1>${escapeHtml(selectorTitle(selection))}</h1>
		${content}
	</body>
	</html>`;
};

export class RiotBenchmarkResultsView implements vscode.WebviewViewProvider, vscode.Disposable {
	private view: vscode.WebviewView | undefined;
	private selection: BenchmarkResultsSelection | undefined;
	private history: BenchmarkHistoryRecord[];

	constructor(private readonly context: vscode.ExtensionContext) {
		this.history = context.workspaceState.get<BenchmarkHistoryRecord[]>(storageKey, []);
	}

	dispose(): void {
		this.view = undefined;
	}

	resolveWebviewView(webviewView: vscode.WebviewView): void {
		this.view = webviewView;
		webviewView.webview.options = {
			enableScripts: false,
		};
		this.render();
	}

	async reveal(): Promise<void> {
		await vscode.commands.executeCommand("workbench.view.extension.riotBenchmarkResultsPanel");
		this.view?.show?.(true);
	}

	setSelection(selection: BenchmarkResultsSelection | undefined): void {
		this.selection = selection;
		this.render();
	}

	record(record: BenchmarkHistoryRecord): void {
		this.history = this.pruneHistory([...this.history, record]);
		void this.context.workspaceState.update(storageKey, this.history);
		this.render();
	}

	private pruneHistory(records: BenchmarkHistoryRecord[]): BenchmarkHistoryRecord[] {
		const keptCounts = new Map<string, number>();
		return records
			.slice()
			.sort((left, right) => right.recordedAt - left.recordedAt)
			.filter((record) => {
				const current = keptCounts.get(record.selector) ?? 0;
				if (current >= maxRecordsPerSelector) {
					return false;
				}

				keptCounts.set(record.selector, current + 1);
				return true;
			})
			.sort((left, right) => left.recordedAt - right.recordedAt);
	}

	private render(): void {
		if (!this.view) {
			return;
		}

		this.view.webview.html = renderHtml(this.selection, this.history);
	}
}
