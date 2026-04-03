import * as vscode from "vscode";
import * as fs from "node:fs/promises";
import * as path from "node:path";
import {
	ensureRiotAvailable,
	resolveRiotCommand,
	workspaceRootFor,
} from "./riot";

type RiotTaskCommand = "build" | "test";

interface RiotTaskDefinition extends vscode.TaskDefinition {
	command: RiotTaskCommand;
	cwd: string;
}

const taskType = "riot";

const hasRiotManifest = async (cwd: string): Promise<boolean> => {
	try {
		await fs.access(path.join(cwd, "riot.toml"));
		return true;
	} catch {
		return false;
	}
};

const taskLabel = (command: RiotTaskCommand): string => `riot ${command}`;

const createRiotTask = async (
	context: vscode.ExtensionContext,
	command: RiotTaskCommand,
	cwd: string,
): Promise<vscode.Task> => {
	const riot = await resolveRiotCommand(context);
	const folder = vscode.workspace.getWorkspaceFolder(vscode.Uri.file(cwd));
	return new vscode.Task(
		{
			type: taskType,
			command,
			cwd,
		} satisfies RiotTaskDefinition,
		folder ?? vscode.TaskScope.Workspace,
		taskLabel(command),
		"riot",
		new vscode.ProcessExecution(riot, [command], { cwd }),
	);
};

export class RiotTaskProvider implements vscode.TaskProvider {
	constructor(private readonly context: vscode.ExtensionContext) {}

	async provideTasks(): Promise<vscode.Task[]> {
		const folders = vscode.workspace.workspaceFolders ?? [];
		const tasks: vscode.Task[] = [];

		for (const folder of folders) {
			if (!(await hasRiotManifest(folder.uri.fsPath))) {
				continue;
			}

			tasks.push(await createRiotTask(this.context, "build", folder.uri.fsPath));
			tasks.push(await createRiotTask(this.context, "test", folder.uri.fsPath));
		}

		return tasks;
	}

	async resolveTask(task: vscode.Task): Promise<vscode.Task | undefined> {
		const definition = task.definition as Partial<RiotTaskDefinition>;
		if ((definition.command !== "build" && definition.command !== "test") || !definition.cwd) {
			return undefined;
		}

		if (!(await hasRiotManifest(definition.cwd))) {
			return undefined;
		}

		return createRiotTask(this.context, definition.command, definition.cwd);
	}
}

export const runWorkspaceTask = async (
	context: vscode.ExtensionContext,
	command: RiotTaskCommand,
	uri?: vscode.Uri,
): Promise<void> => {
	const root = await workspaceRootFor(uri);
	if (!root || !(await hasRiotManifest(root.fsPath))) {
		void vscode.window.showWarningMessage(`Open a Riot workspace to ${command}.`);
		return;
	}

	if (!(await ensureRiotAvailable(context))) {
		return;
	}

	await vscode.tasks.executeTask(await createRiotTask(context, command, root.fsPath));
};
