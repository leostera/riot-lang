import * as vscode from "vscode";
import { CommandsRegistry, MenuId, MenuRegistry, ContextKeyExpr } from "@codingame/monaco-vscode-api/monaco";

const PLAY_RUN_COMMAND_ID = "riot.play.run";
const PLAY_RUN_MENU_ID = MenuId.for("riot.play.editorTitleRun");

let runActionRegistered = false;
let latestRunHandler: (() => Promise<void> | void) | null = null;

export function ensurePlayRunAction(options: {
  authenticated: boolean;
  loginUrl?: string;
}): void {
  latestRunHandler = async () => {
    if (!options.authenticated) {
      if (typeof options.loginUrl === "string" && options.loginUrl.length > 0) {
        window.location.href = options.loginUrl;
      }
      return;
    }

    await vscode.window.showInformationMessage("Run is the next step. The execution sandbox is not wired yet.");
  };

  if (runActionRegistered) {
    return;
  }

  CommandsRegistry.registerCommand({
    id: PLAY_RUN_COMMAND_ID,
    handler: async () => {
      await latestRunHandler?.();
    },
  });

  MenuRegistry.appendMenuItem(PLAY_RUN_MENU_ID, {
    command: {
      id: PLAY_RUN_COMMAND_ID,
      title: "Run",
    },
    group: "1_play",
    order: 1,
    when: ContextKeyExpr.equals("resourceLangId", "riot-ocaml"),
  });

  MenuRegistry.appendMenuItem(MenuId.CommandPalette, {
    command: {
      id: PLAY_RUN_COMMAND_ID,
      title: "Riot Playground: Run",
    },
    when: ContextKeyExpr.equals("resourceLangId", "riot-ocaml"),
  });

  MenuRegistry.appendMenuItem(MenuId.EditorTitle, {
    submenu: PLAY_RUN_MENU_ID,
    title: "Run",
    group: "navigation",
    order: -1,
    when: ContextKeyExpr.equals("resourceLangId", "riot-ocaml"),
    isSplitButton: {
      togglePrimaryAction: true,
    },
  });

  runActionRegistered = true;
}
