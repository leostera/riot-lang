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
exports.ensureRiotAvailable = exports.installManagedRiot = exports.workspaceRootFor = exports.nearestRiotRoot = exports.normalizeFilePath = exports.parseJsonLines = exports.runRiot = exports.runCommand = exports.resolveRiotCommand = exports.managedReleaseMetadataPath = exports.managedRiotBinaryPath = exports.riotHomePath = exports.managedInstallHomePath = exports.configuredRiotPath = exports.getConfig = exports.quote = exports.isOcamlUri = void 0;
const vscode = __importStar(require("vscode"));
const fs = __importStar(require("node:fs/promises"));
const path = __importStar(require("node:path"));
const node_child_process_1 = require("node:child_process");
const defaultInstallUrl = "https://get.riot.ml";
const isOcamlUri = (uri) => {
    return uri.scheme === "file" && (uri.fsPath.endsWith(".ml") || uri.fsPath.endsWith(".mli"));
};
exports.isOcamlUri = isOcamlUri;
const quote = (value) => {
    return JSON.stringify(value);
};
exports.quote = quote;
const getConfig = () => vscode.workspace.getConfiguration("riot");
exports.getConfig = getConfig;
const configuredRiotPath = () => {
    const value = (0, exports.getConfig)().get("path");
    if (typeof value === "string" && value.trim() !== "") {
        return value.trim();
    }
    return undefined;
};
exports.configuredRiotPath = configuredRiotPath;
const managedInstallHomePath = (context) => path.join(context.globalStorageUri.fsPath, "managed-home");
exports.managedInstallHomePath = managedInstallHomePath;
const riotHomePath = (context) => path.join((0, exports.managedInstallHomePath)(context), ".riot");
exports.riotHomePath = riotHomePath;
const managedRiotBinaryPath = (context) => path.join((0, exports.riotHomePath)(context), "bin", "riot");
exports.managedRiotBinaryPath = managedRiotBinaryPath;
const managedReleaseMetadataPath = (context) => path.join((0, exports.riotHomePath)(context), "release.json");
exports.managedReleaseMetadataPath = managedReleaseMetadataPath;
const exists = async (filePath) => {
    try {
        await fs.access(filePath);
        return true;
    }
    catch {
        return false;
    }
};
const resolveRiotCommand = async (context) => {
    const configured = (0, exports.configuredRiotPath)();
    if (configured) {
        return configured;
    }
    const managed = (0, exports.managedRiotBinaryPath)(context);
    if (await exists(managed)) {
        return managed;
    }
    return "riot";
};
exports.resolveRiotCommand = resolveRiotCommand;
const runCommand = async (command, args, options = {}) => new Promise((resolve, reject) => {
    const child = (0, node_child_process_1.spawn)(command, args, {
        cwd: options.cwd,
        env: { ...process.env, ...options.env },
        stdio: ["pipe", "pipe", "pipe"],
    });
    let stdout = "";
    let stderr = "";
    child.stdout.setEncoding("utf8");
    child.stderr.setEncoding("utf8");
    child.stdout.on("data", (chunk) => {
        stdout += chunk;
    });
    child.stderr.on("data", (chunk) => {
        stderr += chunk;
    });
    child.on("error", (error) => {
        reject(error);
    });
    child.on("close", (code) => {
        resolve({
            code: code ?? 1,
            stdout,
            stderr,
        });
    });
    child.stdin.setDefaultEncoding("utf8");
    child.stdin.end(options.stdin ?? "");
});
exports.runCommand = runCommand;
const runRiot = async (context, args, options = {}) => {
    const riot = await (0, exports.resolveRiotCommand)(context);
    return (0, exports.runCommand)(riot, args, options);
};
exports.runRiot = runRiot;
const parseJsonLines = (stdout) => {
    const results = [];
    for (const line of stdout.split(/\r?\n/)) {
        const trimmed = line.trim();
        if (trimmed.length === 0) {
            continue;
        }
        try {
            const parsed = JSON.parse(trimmed);
            if (parsed !== null && typeof parsed === "object" && !Array.isArray(parsed)) {
                results.push(parsed);
            }
        }
        catch {
            // Ignore non-JSON lines.
        }
    }
    return results;
};
exports.parseJsonLines = parseJsonLines;
const normalizeFilePath = (filePath, cwd) => {
    if (path.isAbsolute(filePath)) {
        return path.normalize(filePath);
    }
    if (cwd) {
        return path.normalize(path.join(cwd, filePath));
    }
    return path.normalize(filePath);
};
exports.normalizeFilePath = normalizeFilePath;
const nearestRiotRoot = async (uri) => {
    if (uri.scheme !== "file") {
        return undefined;
    }
    let current = path.dirname(uri.fsPath);
    while (true) {
        const manifest = path.join(current, "riot.toml");
        if (await exists(manifest)) {
            return vscode.Uri.file(current);
        }
        const parent = path.dirname(current);
        if (parent === current) {
            return undefined;
        }
        current = parent;
    }
};
exports.nearestRiotRoot = nearestRiotRoot;
const workspaceRootFor = async (uri) => {
    if (uri) {
        const root = await (0, exports.nearestRiotRoot)(uri);
        if (root) {
            return root;
        }
    }
    const folder = uri ? vscode.workspace.getWorkspaceFolder(uri) : vscode.workspace.workspaceFolders?.[0];
    return folder?.uri;
};
exports.workspaceRootFor = workspaceRootFor;
const readInstalledReleaseMetadata = async (context) => {
    try {
        const contents = await fs.readFile((0, exports.managedReleaseMetadataPath)(context), "utf8");
        return JSON.parse(contents);
    }
    catch {
        return undefined;
    }
};
const readInstallerScript = async () => {
    const installerUrl = (0, exports.getConfig)().get("installUrl") || defaultInstallUrl;
    const response = await fetch(installerUrl, {
        headers: {
            accept: "text/plain, */*",
        },
    });
    if (!response.ok) {
        throw new Error(`Failed to download Riot installer from ${installerUrl}`);
    }
    return await response.text();
};
const installManagedRiot = async (context) => {
    await fs.mkdir(context.globalStorageUri.fsPath, { recursive: true });
    const managedHome = (0, exports.managedInstallHomePath)(context);
    await fs.mkdir(managedHome, { recursive: true });
    const script = await readInstallerScript();
    const install = await (0, exports.runCommand)("sh", [], {
        cwd: managedHome,
        env: {
            HOME: managedHome,
            SHELL: "riot-vscode-extension",
        },
        stdin: script,
    });
    if (install.code !== 0) {
        throw new Error(install.stderr.trim() || install.stdout.trim() || "Riot installer exited unsuccessfully");
    }
    const installedBinary = (0, exports.managedRiotBinaryPath)(context);
    if (!(await exists(installedBinary))) {
        throw new Error("Riot installer finished without producing a managed riot binary");
    }
    const metadata = await readInstalledReleaseMetadata(context);
    if (metadata) {
        return metadata;
    }
    const version = await (0, exports.runCommand)(installedBinary, ["--version"]);
    const releaseId = version.stdout.trim() || "latest";
    return {
        release_id: releaseId,
        build_sha: "unknown",
    };
};
exports.installManagedRiot = installManagedRiot;
const ensureRiotAvailable = async (context, options = {}) => {
    const riot = await (0, exports.resolveRiotCommand)(context);
    try {
        const result = await (0, exports.runCommand)(riot, ["--version"]);
        if (result.code === 0) {
            return riot;
        }
    }
    catch {
        // fall through
    }
    if (options.prompt === false) {
        return undefined;
    }
    const choice = await vscode.window.showWarningMessage("Riot is not available. Install it for this extension?", "Install Riot");
    if (choice !== "Install Riot") {
        return undefined;
    }
    const metadata = await vscode.window.withProgress({
        location: vscode.ProgressLocation.Notification,
        title: "Installing Riot",
    }, async () => (0, exports.installManagedRiot)(context));
    vscode.window.showInformationMessage(`Installed Riot ${metadata.release_id}.`);
    return (0, exports.managedRiotBinaryPath)(context);
};
exports.ensureRiotAvailable = ensureRiotAvailable;
//# sourceMappingURL=riot.js.map