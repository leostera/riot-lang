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
exports.RiotDiagnostics = void 0;
const vscode = __importStar(require("vscode"));
const riot_1 = require("./riot");
const asObject = (value) => {
    if (value !== null && value !== undefined && typeof value === "object" && !Array.isArray(value)) {
        return value;
    }
    return undefined;
};
const asArray = (value) => {
    return Array.isArray(value) ? value : [];
};
const asString = (value) => {
    return typeof value === "string" ? value : undefined;
};
const spanRange = (document, span) => {
    const start = Math.max(span.start ?? 0, 0);
    const end = Math.max(span.end ?? start, start);
    return new vscode.Range(document.positionAt(start), document.positionAt(end));
};
const synMessage = (diagnostic) => {
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
const synItems = (document, diagnostics) => diagnostics.flatMap((value) => {
    const object = asObject(value);
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
const fixSeverity = (severity) => {
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
const fixItems = (document, diagnostics) => diagnostics.flatMap((value) => {
    const object = asObject(value);
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
const findFmtFileEvent = (events, document, cwd) => {
    const target = (0, riot_1.normalizeFilePath)(document.uri.fsPath);
    return events.find((event) => {
        if (asString(event.type) !== "file") {
            return false;
        }
        const file = asString(event.file);
        if (!file) {
            return false;
        }
        return (0, riot_1.normalizeFilePath)(file, cwd) === target;
    });
};
const findFixFileResult = (events, document) => {
    const target = (0, riot_1.normalizeFilePath)(document.uri.fsPath);
    for (const event of events) {
        const files = asArray(event.files);
        for (const file of files) {
            const object = asObject(file);
            if (!object) {
                continue;
            }
            const filePath = asString(object.file);
            if (filePath && (0, riot_1.normalizeFilePath)(filePath) === target) {
                return object;
            }
        }
    }
    return undefined;
};
class RiotDiagnostics {
    context;
    collection;
    constructor(context) {
        this.context = context;
        this.collection = vscode.languages.createDiagnosticCollection("riot");
    }
    register() {
        const subscriptions = [this.collection];
        subscriptions.push(vscode.workspace.onDidOpenTextDocument((document) => {
            void this.refresh(document);
        }), vscode.workspace.onDidSaveTextDocument((document) => {
            void this.refresh(document);
        }), vscode.workspace.onDidCloseTextDocument((document) => {
            this.collection.delete(document.uri);
        }));
        for (const document of vscode.workspace.textDocuments) {
            void this.refresh(document);
        }
        return subscriptions;
    }
    async refresh(document) {
        if (!(0, riot_1.isOcamlUri)(document.uri)) {
            return;
        }
        if (!vscode.workspace.getConfiguration("riot").get("diagnostics.enabled", true)) {
            this.collection.delete(document.uri);
            return;
        }
        if (!(await (0, riot_1.ensureRiotAvailable)(this.context, { prompt: false }))) {
            return;
        }
        const root = await (0, riot_1.workspaceRootFor)(document.uri);
        const cwd = root?.fsPath;
        const diagnostics = [];
        const fmtResult = await (0, riot_1.runRiot)(this.context, ["fmt", "--json", document.uri.fsPath], { cwd });
        const fmtEvents = (0, riot_1.parseJsonLines)(fmtResult.stdout);
        const fmtFile = findFmtFileEvent(fmtEvents, document, cwd);
        if (fmtFile) {
            diagnostics.push(...synItems(document, asArray(fmtFile.diagnostics)));
        }
        if (vscode.workspace.getConfiguration("riot").get("diagnostics.runFix", true)) {
            const fixResult = await (0, riot_1.runRiot)(this.context, ["fix", "--json", document.uri.fsPath], { cwd });
            const fixEvents = (0, riot_1.parseJsonLines)(fixResult.stdout);
            const fixFile = findFixFileResult(fixEvents, document);
            if (fixFile) {
                diagnostics.push(...synItems(document, asArray(fixFile.parse_diagnostics)));
                diagnostics.push(...fixItems(document, asArray(fixFile.diagnostics)));
            }
        }
        this.collection.set(document.uri, diagnostics);
    }
    clear(document) {
        this.collection.delete(document.uri);
    }
}
exports.RiotDiagnostics = RiotDiagnostics;
//# sourceMappingURL=diagnostics.js.map