import { ExtensionHostKind, registerExtension } from "@codingame/monaco-vscode-api/extensions";

let readyPromise: Promise<void> | null = null;

export function ensureRiotLanguageExtension(): Promise<void> {
  if (readyPromise !== null) {
    return readyPromise;
  }

  const { registerFileUrl, whenReady } = registerExtension(
    {
      name: "riot-ml",
      displayName: "Riot ML",
      description: "Riot language support for the in-browser playground.",
      version: "0.0.0",
      publisher: "riot",
      engines: {
        vscode: "*",
      },
      categories: ["Programming Languages"],
      contributes: {
        languages: [
          {
            id: "riot-ocaml",
            aliases: ["Riot OCaml", "OCaml"],
            extensions: [".ml", ".mli"],
            configuration: "./language-configuration.json",
          },
        ],
        grammars: [
          {
            language: "riot-ocaml",
            scopeName: "source.ocaml.riot",
            path: "./syntaxes/riot-ocaml.tmLanguage.json",
          },
        ],
      },
    },
    ExtensionHostKind.LocalWebWorker,
    {
      system: true,
    },
  );

  registerFileUrl(
    "language-configuration.json",
    new URL("../../../../editors/vscode-riot-ml/language-configuration.json", import.meta.url).toString(),
  );
  registerFileUrl(
    "syntaxes/riot-ocaml.tmLanguage.json",
    new URL("../../../../editors/vscode-riot-ml/syntaxes/riot-ocaml.tmLanguage.json", import.meta.url).toString(),
  );

  readyPromise = whenReady();
  return readyPromise;
}
