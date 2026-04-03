"use strict";
var __createBinding = (this && this.__createBinding) || (Object.create ? (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    var desc = Object.getOwnPropertyDescriptor(m, k);
    if (!desc || ("get" in desc ? !m.__esModule : desc.writable || desc.configurable)) {
      desc = { enumerable: true, get: function() { return m[k]; } };
    }
    Object.defineProperty(o, k2, desc);
}) : (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    o[k2] = m[k];
}));
var __setModuleDefault = (this && this.__setModuleDefault) || (Object.create ? (function(o, v) {
    Object.defineProperty(o, "default", { enumerable: true, value: v });
}) : function(o, v) {
    o["default"] = v;
});
var __importStar = (this && this.__importStar) || (function () {
    var ownKeys = function(o) {
        ownKeys = Object.getOwnPropertyNames || function (o) {
            var ar = [];
            for (var k in o) if (Object.prototype.hasOwnProperty.call(o, k)) ar[ar.length] = k;
            return ar;
        };
        return ownKeys(o);
    };
    return function (mod) {
        if (mod && mod.__esModule) return mod;
        var result = {};
        if (mod != null) for (var k = ownKeys(mod), i = 0; i < k.length; i++) if (k[i] !== "default") __createBinding(result, mod, k[i]);
        __setModuleDefault(result, mod);
        return result;
    };
})();
Object.defineProperty(exports, "__esModule", { value: true });
exports.runWorkspaceTask = exports.RiotTaskProvider = void 0;
const vscode = __importStar(require("vscode"));
const fs = __importStar(require("node:fs/promises"));
const path = __importStar(require("node:path"));
const riot_1 = require("./riot");
const taskType = "riot";
const hasRiotManifest = async (cwd) => {
    try {
        await fs.access(path.join(cwd, "riot.toml"));
        return true;
    }
    catch {
        return false;
    }
};
const taskLabel = (command) => `riot ${command}`;
const createRiotTask = async (context, command, cwd) => {
    const riot = await (0, riot_1.resolveRiotCommand)(context);
    const folder = vscode.workspace.getWorkspaceFolder(vscode.Uri.file(cwd));
    return new vscode.Task({
        type: taskType,
        command,
        cwd,
    }, folder ?? vscode.TaskScope.Workspace, taskLabel(command), "riot", new vscode.ProcessExecution(riot, [command], { cwd }));
};
class RiotTaskProvider {
    context;
    constructor(context) {
        this.context = context;
    }
    async provideTasks() {
        const folders = vscode.workspace.workspaceFolders ?? [];
        const tasks = [];
        for (const folder of folders) {
            if (!(await hasRiotManifest(folder.uri.fsPath))) {
                continue;
            }
            tasks.push(await createRiotTask(this.context, "build", folder.uri.fsPath));
            tasks.push(await createRiotTask(this.context, "test", folder.uri.fsPath));
        }
        return tasks;
    }
    async resolveTask(task) {
        const definition = task.definition;
        if ((definition.command !== "build" && definition.command !== "test") || !definition.cwd) {
            return undefined;
        }
        if (!(await hasRiotManifest(definition.cwd))) {
            return undefined;
        }
        return createRiotTask(this.context, definition.command, definition.cwd);
    }
}
exports.RiotTaskProvider = RiotTaskProvider;
const runWorkspaceTask = async (context, command, uri) => {
    const root = await (0, riot_1.workspaceRootFor)(uri);
    if (!root || !(await hasRiotManifest(root.fsPath))) {
        void vscode.window.showWarningMessage(`Open a Riot workspace to ${command}.`);
        return;
    }
    if (!(await (0, riot_1.ensureRiotAvailable)(context))) {
        return;
    }
    await vscode.tasks.executeTask(await createRiotTask(context, command, root.fsPath));
};
exports.runWorkspaceTask = runWorkspaceTask;
//# sourceMappingURL=tasks.js.map