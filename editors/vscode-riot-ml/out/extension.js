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
exports.activate = activate;
exports.deactivate = deactivate;
const vscode = __importStar(require("vscode"));
const diagnostics_1 = require("./diagnostics");
const format_1 = require("./format");
const riot_1 = require("./riot");
const tasks_1 = require("./tasks");
const ocamlDocumentSelector = [
    { scheme: "file", pattern: "**/*.ml" },
    { scheme: "file", pattern: "**/*.mli" },
];
const activeOcamlDocument = () => {
    const document = vscode.window.activeTextEditor?.document;
    if (!document || document.uri.scheme !== "file") {
        return undefined;
    }
    if (!document.uri.fsPath.endsWith(".ml") && !document.uri.fsPath.endsWith(".mli")) {
        return undefined;
    }
    return document;
};
function activate(context) {
    const diagnostics = new diagnostics_1.RiotDiagnostics(context);
    const formatter = new format_1.RiotFormattingProvider(context, diagnostics);
    context.subscriptions.push(...diagnostics.register(), vscode.languages.registerDocumentFormattingEditProvider(ocamlDocumentSelector, formatter), (0, format_1.registerFormatOnSave)(formatter), vscode.tasks.registerTaskProvider("riot", new tasks_1.RiotTaskProvider(context)), vscode.commands.registerCommand("riot.install", async () => {
        const metadata = await vscode.window.withProgress({
            location: vscode.ProgressLocation.Notification,
            title: "Installing Riot",
        }, async () => (0, riot_1.installManagedRiot)(context));
        void vscode.window.showInformationMessage(`Installed Riot ${metadata.release_id} (${metadata.build_sha}).`);
    }), vscode.commands.registerCommand("riot.buildWorkspace", async () => {
        await (0, tasks_1.runWorkspaceTask)(context, "build", activeOcamlDocument()?.uri);
    }), vscode.commands.registerCommand("riot.testWorkspace", async () => {
        await (0, tasks_1.runWorkspaceTask)(context, "test", activeOcamlDocument()?.uri);
    }), vscode.commands.registerCommand("riot.formatDocument", async () => {
        if (!(await (0, riot_1.ensureRiotAvailable)(context))) {
            return;
        }
        await vscode.commands.executeCommand("editor.action.formatDocument");
    }), vscode.commands.registerCommand("riot.refreshDiagnostics", async () => {
        const document = activeOcamlDocument();
        if (!document) {
            return;
        }
        await diagnostics.refresh(document);
    }));
}
function deactivate() { }
//# sourceMappingURL=extension.js.map