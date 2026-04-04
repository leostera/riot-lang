import { initialize, registerFile, updateUserConfiguration } from "@codingame/monaco-editor-wrapper";
import "@codingame/monaco-editor-wrapper/features/notifications";
import "@codingame/monaco-editor-wrapper/features/viewPanels";
import "@codingame/monaco-editor-wrapper/features/workbench";
import { RegisteredMemoryFile } from "@codingame/monaco-vscode-files-service-override";
import * as monaco from "monaco-editor";
import * as vscode from "vscode";
import { useEffect, useMemo, useRef, useState } from "react";
import { ensureRiotLanguageExtension } from "@/lib/riot-language-extension.ts";
import { ensureTomlLanguage } from "@/lib/toml-language.ts";
import { ensurePlayRunAction } from "@/lib/workbench-actions.ts";
import type { WorkspaceFile } from "@/lib/workspace.ts";

interface Props {
  files: WorkspaceFile[];
  activePath: string;
  authenticated?: boolean;
  loginUrl?: string;
}

const workspaceRoot = monaco.Uri.file("/workspace");
let initPromise: Promise<void> | null = null;

function ensureWorkbench(container: HTMLElement) {
  if (initPromise === null) {
    initPromise = initialize(
      {
        workspaceProvider: {
          trusted: true,
          workspace: {
            folderUri: workspaceRoot,
          },
          open: async () => false,
        },
        windowIndicator: {
          label: "play.riot.ml",
          tooltip: "Riot Playground",
          command: "",
        },
      },
      { container },
    ).then(async () => {
      await updateUserConfiguration(`{
  "workbench.colorTheme": "Default Dark Modern",
  "workbench.activityBar.location": "left",
  "workbench.sideBar.location": "left",
  "window.commandCenter": false,
  "window.menuBarVisibility": "compact",
  "breadcrumbs.enabled": true,
  "editor.minimap.enabled": false,
  "editor.fontSize": 14,
  "editor.lineNumbersMinChars": 3,
  "editor.scrollBeyondLastLine": false,
  "editor.wordWrap": "on",
  "editor.stickyScroll.enabled": false,
  "workbench.startupEditor": "none"
}`);
    });
  }

  return initPromise;
}

export default function VscodeWorkbench({ files, activePath, authenticated = false, loginUrl }: Props) {
  const containerRef = useRef<HTMLDivElement | null>(null);
  const [ready, setReady] = useState(false);

  const normalizedFiles = useMemo(
    () =>
      files.map((file) => ({
        ...file,
        path: file.path.startsWith("/") ? file.path : `/workspace/${file.path.replace(/^\/+/, "")}`,
      })),
    [files],
  );

  const activeFilePath = activePath.startsWith("/") ? activePath : `/workspace/${activePath.replace(/^\/+/, "")}`;

  useEffect(() => {
    ensurePlayRunAction({
      authenticated,
      loginUrl,
    });
  }, [authenticated, loginUrl]);

  useEffect(() => {
    const container = containerRef.current;
    if (container === null) {
      return;
    }

    let cancelled = false;
    const disposables = normalizedFiles.map((file) =>
      registerFile(new RegisteredMemoryFile(monaco.Uri.file(file.path), file.sourceCode)),
    );

    async function openActiveFile() {
      await ensureWorkbench(container!);
      ensureTomlLanguage();
      await ensureRiotLanguageExtension();
      if (cancelled) {
        return;
      }

      const document = await vscode.workspace.openTextDocument(vscode.Uri.file(activeFilePath));
      if (cancelled) {
        return;
      }

      await vscode.window.showTextDocument(document, { preview: false });
      if (!cancelled) {
        setReady(true);
      }
    }

    void openActiveFile().catch((error) => {
      console.error("Failed to initialize VS Code workbench", error);
    });

    return () => {
      cancelled = true;
      for (const disposable of disposables) {
        disposable.dispose();
      }
    };
  }, [activeFilePath, normalizedFiles]);

  return (
    <div className="relative h-screen overflow-hidden bg-[#1e1e1e]">
      {!ready ? (
        <div className="absolute inset-0 z-10 flex items-center justify-center bg-[#1e1e1e] text-sm text-[#8f9aab]">
          Loading VS Code workbench…
        </div>
      ) : null}
      <div ref={containerRef} className="h-full w-full" />
    </div>
  );
}
