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
  const [isCompactViewport, setIsCompactViewport] = useState(false);
  const [dismissedCompactNotice, setDismissedCompactNotice] = useState(false);

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
    const mediaQuery = window.matchMedia("(max-width: 767px)");
    const syncViewport = () => {
      setIsCompactViewport(mediaQuery.matches);
    };

    syncViewport();
    mediaQuery.addEventListener("change", syncViewport);
    return () => {
      mediaQuery.removeEventListener("change", syncViewport);
    };
  }, []);

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
      {isCompactViewport && !dismissedCompactNotice ? (
        <div className="absolute inset-0 z-20 flex items-center justify-center bg-[#111111]/85 p-4 backdrop-blur-sm">
          <div className="grid max-w-sm gap-4 rounded-2xl border border-white/10 bg-[#161616] px-5 py-5 text-left shadow-[0_24px_80px_rgba(0,0,0,0.45)]">
            <div className="grid gap-2">
              <p className="text-[11px] font-semibold uppercase tracking-[0.22em] text-[#8f9aab]">
                Riot Playground
              </p>
              <h1 className="text-xl font-semibold tracking-tight text-white">
                Better on a wider screen
              </h1>
              <p className="text-sm leading-6 text-[#b7c1cf]">
                The full VS Code workbench fits much better on desktop or tablet. You can still open it here, but the editor chrome gets cramped quickly on phones.
              </p>
            </div>

            <div className="flex flex-col gap-2 sm:flex-row">
              <button
                type="button"
                onClick={() => setDismissedCompactNotice(true)}
                className="rounded-md bg-[#f5344d] px-4 py-2 text-sm font-medium text-white transition hover:bg-[#ff4d65]"
              >
                Continue anyway
              </button>
              <a
                href="https://riot.ml"
                className="rounded-md border border-white/10 px-4 py-2 text-center text-sm text-[#d8dee8] transition hover:border-[#f5344d]"
              >
                Back to Riot
              </a>
            </div>
          </div>
        </div>
      ) : null}

      {!ready ? (
        <div className="absolute inset-0 z-10 flex items-center justify-center bg-[#1e1e1e] text-sm text-[#8f9aab]">
          Loading VS Code workbench…
        </div>
      ) : null}
      <div
        ref={containerRef}
        className={isCompactViewport && !dismissedCompactNotice ? "pointer-events-none h-full w-full blur-[1px]" : "h-full w-full"}
      />
    </div>
  );
}
