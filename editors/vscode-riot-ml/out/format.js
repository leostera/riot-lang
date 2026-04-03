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
exports.registerFormatOnSave = exports.RiotFormattingProvider = void 0;
const vscode = __importStar(require("vscode"));
const fs = __importStar(require("node:fs/promises"));
const path = __importStar(require("node:path"));
const os = __importStar(require("node:os"));
const riot_1 = require("./riot");
const fullDocumentRange = (document) => {
    const start = new vscode.Position(0, 0);
    const end = document.lineCount === 0
        ? start
        : document.lineAt(document.lineCount - 1).range.end;
    return new vscode.Range(start, end);
};
class RiotFormattingProvider {
    context;
    diagnostics;
    constructor(context, diagnostics) {
        this.context = context;
        this.diagnostics = diagnostics;
    }
    async provideDocumentFormattingEdits(document) {
        if (!(0, riot_1.isOcamlUri)(document.uri)) {
            return [];
        }
        if (!(await (0, riot_1.ensureRiotAvailable)(this.context, { prompt: false }))) {
            return [];
        }
        const root = await (0, riot_1.workspaceRootFor)(document.uri);
        const tempDir = await fs.mkdtemp(path.join(os.tmpdir(), "riot-vscode-format-"));
        const tempFile = path.join(tempDir, `document${path.extname(document.uri.fsPath) || ".ml"}`);
        try {
            await fs.writeFile(tempFile, document.getText(), "utf8");
            const result = await (0, riot_1.runRiot)(this.context, ["fmt", tempFile], { cwd: root?.fsPath });
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
        }
        finally {
            await fs.rm(tempDir, { recursive: true, force: true });
        }
    }
}
exports.RiotFormattingProvider = RiotFormattingProvider;
const registerFormatOnSave = (_provider) => vscode.workspace.onWillSaveTextDocument((event) => {
    const document = event.document;
    if (!(0, riot_1.isOcamlUri)(document.uri)) {
        return;
    }
    if (!vscode.workspace.getConfiguration("riot").get("formatOnSave", true)) {
        return;
    }
    event.waitUntil(vscode.commands.executeCommand("vscode.executeFormatDocumentProvider", document.uri).then((edits) => edits ?? [], () => []));
});
exports.registerFormatOnSave = registerFormatOnSave;
//# sourceMappingURL=format.js.map