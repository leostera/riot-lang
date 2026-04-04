import * as monaco from "monaco-editor";

let registered = false;
let attached = false;

function isTomlModel(model: monaco.editor.ITextModel): boolean {
  return model.uri.path.toLowerCase().endsWith(".toml");
}

export function ensureTomlLanguage(): void {
  if (!registered) {
    monaco.languages.register({
      id: "toml",
      extensions: [".toml"],
      aliases: ["TOML", "toml"],
    });

    monaco.languages.setLanguageConfiguration("toml", {
      comments: {
        lineComment: "#",
      },
      brackets: [["[", "]"], ["{", "}"], ["(", ")"]],
      autoClosingPairs: [
        { open: "[", close: "]" },
        { open: "{", close: "}" },
        { open: "(", close: ")" },
        { open: '"', close: '"' },
      ],
      surroundingPairs: [
        { open: "[", close: "]" },
        { open: "{", close: "}" },
        { open: "(", close: ")" },
        { open: '"', close: '"' },
      ],
    });

    monaco.languages.setMonarchTokensProvider("toml", {
      tokenizer: {
        root: [
          [/^\s*#.*$/, "comment"],
          [/^\s*\[\[?[^\]]+\]\]?\s*$/, "keyword"],
          [/\b(true|false)\b/, "keyword"],
          [/\b\d+(?:\.\d+)?\b/, "number"],
          [/"([^"\\]|\\.)*"/, "string"],
          [/'([^'\\]|\\.)*'/, "string"],
          [/\b\d{4}-\d{2}-\d{2}(?:[Tt ][0-9:.+-Zz]+)?\b/, "number"],
          [/[A-Za-z0-9_.-]+(?=\s*=)/, "key"],
          [/=+/, "delimiter"],
          [/[{},[\]]/, "@brackets"],
        ],
      },
    });

    registered = true;
  }

  if (!attached) {
    for (const model of monaco.editor.getModels()) {
      if (isTomlModel(model)) {
        monaco.editor.setModelLanguage(model, "toml");
      }
    }

    monaco.editor.onDidCreateModel((model) => {
      if (isTomlModel(model)) {
        monaco.editor.setModelLanguage(model, "toml");
      }
    });

    attached = true;
  }
}
