open Std

let escape_html = fun input ->
  String.fold_left
    ~fn:(fun acc ch ->
      acc ^ (
        match ch with
        | '<' -> "&lt;"
        | '>' -> "&gt;"
        | '&' -> "&amp;"
        | '"' -> "&quot;"
        | '\'' -> "&#39;"
        | _ -> String.make ~len:1 ~char:ch
      ))
    ~init:""
    input

let default_css =
  {css|
:root {
  --jr-color-red-50: #fff0f3;
  --jr-color-red-100: #ffdce3;
  --jr-color-red-200: #ffb8c5;
  --jr-color-red-300: #ff8aa0;
  --jr-color-red-400: #ff6f87;
  --jr-color-red-500: #ef233c;
  --jr-color-red-600: #c91f38;
  --jr-color-red-700: #9f172c;
  --jr-color-red-800: #8f1b2d;
  --jr-color-red-900: #5f1421;
  --jr-color-rust-50: #fff7ef;
  --jr-color-rust-100: #f4e7d8;
  --jr-color-rust-200: #e3c7aa;
  --jr-color-rust-300: #d3a676;
  --jr-color-rust-400: #c4773a;
  --jr-color-rust-500: #b14a14;
  --jr-color-rust-600: #99461c;
  --jr-color-rust-700: #8a3a10;
  --jr-color-rust-800: #6f2d0c;
  --jr-color-rust-900: #4a1d08;
  --jr-color-ink-50: #f3f0f5;
  --jr-color-ink-100: #ddd8e2;
  --jr-color-ink-200: #bdb4c5;
  --jr-color-ink-300: #9aa0aa;
  --jr-color-ink-400: #9aa0aa;
  --jr-color-ink-500: #5b5462;
  --jr-color-ink-600: #413a48;
  --jr-color-ink-700: #2b2630;
  --jr-color-ink-800: #1d1a21;
  --jr-color-ink-900: #151317;
  --jr-color-ink-950: #0e0d10;
  --jr-color-paper-50: #fffdf7;
  --jr-color-paper-100: #fff8ed;
  --jr-color-paper-200: #f5ecdc;
  --jr-color-paper-300: #e8dcc8;
  --jr-color-paper-400: #d8cbb7;
  --jr-color-paper-500: #b8a78e;
  --jr-color-paper-600: #927f66;
  --jr-color-paper-700: #6f5f4d;
  --jr-color-paper-800: #4d4236;
  --jr-color-paper-900: #302920;
  --jr-color-mint-300: #65e7bd;
  --jr-color-mint-500: #24c08d;
  --jr-color-mint-700: #0f7354;
  --jr-color-amber-300: #ffc43d;
  --jr-color-amber-500: #f0b429;
  --jr-color-amber-800: #654600;
  --jr-color-blue-300: #7bb8ff;
  --jr-color-blue-500: #2777ff;
  --jr-color-blue-700: #0d459f;
  --jr-color-green-600: #247e45;
  --jr-color-brand: var(--jr-color-red-500);
  --jr-color-brand-hover: var(--jr-color-red-600);
  --jr-color-brand-active: var(--jr-color-red-700);
  --jr-color-brand-muted: rgba(239, 35, 60, 0.10);
  --jr-color-brand-subtle: rgba(239, 35, 60, 0.06);
  --jr-color-bg: var(--jr-color-paper-100);
  --jr-color-bg-subtle: var(--jr-color-paper-200);
  --jr-color-bg-raised: rgba(255, 253, 247, 0.62);
  --jr-color-bg-inset: rgba(255, 253, 247, 0.46);
  --jr-color-bg-code: rgba(255, 253, 247, 0.54);
  --jr-color-bg-terminal: var(--jr-color-ink-950);
  --jr-color-text: var(--jr-color-ink-900);
  --jr-color-text-strong: var(--jr-color-ink-950);
  --jr-color-text-muted: var(--jr-color-ink-500);
  --jr-color-text-subtle: var(--jr-color-ink-400);
  --jr-color-text-inverse: var(--jr-color-paper-50);
  --jr-color-text-inverse-muted: rgba(251, 250, 245, 0.62);
  --jr-color-link: var(--jr-color-rust-500);
  --jr-color-link-hover: var(--jr-color-rust-700);
  --jr-color-success: var(--jr-color-mint-500);
  --jr-color-success-muted: rgba(36, 192, 141, 0.14);
  --jr-color-warning: var(--jr-color-amber-500);
  --jr-color-warning-muted: rgba(240, 180, 41, 0.18);
  --jr-color-danger: var(--jr-color-red-500);
  --jr-color-danger-muted: rgba(239, 35, 60, 0.12);
  --jr-color-info: var(--jr-color-blue-500);
  --jr-color-info-muted: rgba(39, 119, 255, 0.12);
  --jr-color-border: rgba(21, 19, 23, 0.14);
  --jr-color-border-strong: rgba(21, 19, 23, 0.22);
  --jr-color-border-heavy: rgba(21, 19, 23, 0.34);
  --jr-color-border-inverse: rgba(251, 250, 245, 0.16);
  --jr-color-selection-bg: var(--jr-color-brand);
  --jr-color-selection-text: white;
  --jr-color-syntax-text: #e6e2d6;
  --jr-color-syntax-muted: rgba(230, 226, 214, 0.68);
  --jr-color-syntax-keyword: var(--jr-color-blue-300);
  --jr-color-syntax-string: var(--jr-color-amber-300);
  --jr-color-syntax-number: var(--jr-color-mint-300);
  --jr-color-syntax-type: #b58cff;
  --jr-color-syntax-function: var(--jr-color-blue-300);
  --jr-color-syntax-comment: rgba(230, 226, 214, 0.46);
  --jr-color-syntax-error: var(--jr-color-red-300);
  --jr-color-syntax-warning: var(--jr-color-amber-300);
  --jr-color-syntax-success: var(--jr-color-mint-300);
  --jr-font-display: "Martian Mono", "JetBrains Mono", "IBM Plex Mono", "SFMono-Regular", ui-monospace, Menlo, Consolas, monospace;
  --jr-font-mono: "JetBrains Mono", "IBM Plex Mono", "SFMono-Regular", ui-monospace, Menlo, Consolas, monospace;
  --jr-font-sans: "Atkinson Hyperlegible", -apple-system, BlinkMacSystemFont, "Segoe UI", system-ui, sans-serif;
  --jr-font-weight-regular: 400;
  --jr-font-weight-medium: 500;
  --jr-font-weight-bold: 700;
  --jr-font-weight-heavy: 700;
  --jr-font-weight-black: 800;
  --jr-font-size-00: 10px;
  --jr-font-size-0: 11px;
  --jr-font-size-1: 12px;
  --jr-font-size-2: 13px;
  --jr-font-size-3: 14px;
  --jr-font-size-4: 15px;
  --jr-font-size-5: 16px;
  --jr-font-size-6: 18px;
  --jr-font-size-8: 24px;
  --jr-font-size-10: 36px;
  --jr-line-height-solid: 1;
  --jr-line-height-tight: 1.12;
  --jr-line-height-heading: 1.18;
  --jr-line-height-ui: 1.35;
  --jr-line-height-body: 1.58;
  --jr-line-height-code: 1.55;
  --jr-space-0: 0;
  --jr-space-1: 5px;
  --jr-space-2: 10px;
  --jr-space-3: 15px;
  --jr-space-4: 20px;
  --jr-space-5: 25px;
  --jr-space-6: 30px;
  --jr-space-8: 40px;
  --jr-space-10: 50px;
  --jr-space-12: 60px;
  --jr-space-14: 70px;
  --jr-radius-none: 0;
  --jr-radius-xs: 5px;
  --jr-radius-control: 5px;
  --jr-radius-panel: 5px;
  --jr-radius-status: 5px;
  --jr-radius-terminal: 5px;
  --jr-border-default: 1px solid var(--jr-color-border);
  --jr-border-strong: 1px solid var(--jr-color-border-strong);
  --jr-border-heavy: 1px solid var(--jr-color-border-heavy);
  --jr-border-brand: 2px solid var(--jr-color-brand);
  --jr-shadow-none: none;
  --jr-shadow-soft-sm: 0 10px 35px rgba(15, 13, 18, 0.08);
  --jr-layout-wide-width: 1320px;
  --jr-layout-page-width: 1080px;
  --jr-layout-prose-width: 68ch;
  --jr-layout-reading-width: 760px;
  --jr-size-control-xs: 25px;
  --jr-size-control-sm: 30px;
  --jr-size-control-md: 35px;
  --jr-size-control-lg: 40px;
  --jr-docs-bg: var(--jr-color-bg);
  --jr-docs-sidebar-width: 240px;
  --jr-docs-sidebar-bg: rgba(21, 19, 23, 0.035);
  --jr-docs-sidebar-border: var(--jr-border-default);
  --jr-docs-content-width: var(--jr-layout-page-width);
  --jr-docs-prose-width: var(--jr-layout-prose-width);
  --jr-docs-heading-color: var(--jr-color-text-strong);
  --jr-docs-body-color: var(--jr-color-text);
  --jr-docs-muted-color: var(--jr-color-text-muted);
  --jr-docs-anchor-color: var(--jr-color-brand-active);
  --jr-docs-breadcrumb-font-family: var(--jr-font-mono);
  --jr-docs-breadcrumb-font-size: var(--jr-font-size-1);
  --jr-docs-breadcrumb-color: var(--jr-color-text-muted);
  --jr-panel-bg: var(--jr-color-bg-raised);
  --jr-panel-border: var(--jr-border-default);
  --jr-panel-radius: var(--jr-radius-panel);
  --jr-panel-shadow: var(--jr-shadow-none);
  --jr-panel-padding: var(--jr-space-5);
  --jr-badge-height: var(--jr-size-control-xs);
  --jr-badge-padding-x: var(--jr-space-2);
  --jr-badge-radius: var(--jr-radius-status);
  --jr-badge-border: var(--jr-border-default);
  --jr-badge-font-family: var(--jr-font-mono);
  --jr-badge-font-size: var(--jr-font-size-0);
  --jr-terminal-bg: var(--jr-color-bg-terminal);
  --jr-terminal-border: 1px solid var(--jr-color-border-inverse);
  --jr-terminal-radius: var(--jr-radius-terminal);
  --jr-terminal-body-padding: var(--jr-space-4);
  --jr-terminal-body-font-family: var(--jr-font-mono);
  --jr-terminal-body-font-size: var(--jr-font-size-1);
  --jr-terminal-body-line-height: var(--jr-line-height-code);
  --jr-focus-ring: 0 0 0 2px var(--jr-color-bg), 0 0 0 4px var(--jr-color-brand);
  --bg: var(--jr-docs-bg);
  --bg-2: var(--jr-color-bg-subtle);
  --surface: var(--jr-color-bg-raised);
  --code-bg: var(--jr-terminal-bg);
  --code-text: var(--jr-color-syntax-text);
  --code-muted: var(--jr-color-syntax-muted);
  --ink: var(--jr-color-text);
  --ink-2: var(--jr-color-ink-700);
  --muted: var(--jr-color-text-muted);
  --muted-2: var(--jr-color-text-subtle);
  --rule: var(--jr-color-border);
  --rule-soft: var(--jr-color-border);
  --brand: var(--jr-color-brand);
  --brand-soft: var(--jr-color-brand-muted);
  --accent: var(--jr-docs-anchor-color);
  --accent-soft: var(--jr-color-brand-subtle);
  --accent-deep: var(--jr-color-brand-active);
  --k-type: var(--jr-color-brand-active);
  --k-record: #b58cff;
  --k-variant: var(--jr-color-mint-700);
  --k-val: var(--jr-color-rust-700);
  --k-fn: var(--jr-color-blue-700);
  --k-module: var(--jr-color-green-600);
  --sidebar-w: var(--jr-docs-sidebar-width);
  --content-max: var(--jr-docs-content-width);
  --sans: var(--jr-font-sans);
  --display: var(--jr-font-display);
  --mono: var(--jr-font-mono);
}

* { box-sizing: border-box; }
html { -webkit-text-size-adjust: 100%; scroll-behavior: smooth; }
body {
  margin: var(--jr-space-0);
  background: var(--bg);
  color: var(--ink);
  font-family: var(--sans);
  font-size: var(--jr-font-size-4);
  line-height: var(--jr-line-height-body);
  -moz-osx-font-smoothing: grayscale;
  -webkit-font-smoothing: antialiased;
  text-rendering: optimizeLegibility;
}

a { color: var(--accent); text-decoration: none; }
a:hover { text-decoration: underline; text-underline-offset: 2px; text-decoration-thickness: 1px; }
code, pre { font-family: var(--mono); }
::selection { background: var(--jr-color-selection-bg); color: var(--jr-color-selection-text); }
*:focus-visible { outline: 2px solid var(--brand); outline-offset: 2px; }

.docs-shell {
  display: grid;
  grid-template-columns: var(--sidebar-w) 1fr;
  max-width: var(--jr-layout-wide-width);
  margin: var(--jr-space-0) auto;
  min-height: 100vh;
}

.sidebar {
  position: sticky;
  top: 0;
  align-self: start;
  max-height: 100vh;
  overflow-y: auto;
  padding: var(--jr-space-5) var(--jr-space-4) var(--jr-space-8) var(--jr-space-5);
  background: var(--jr-docs-sidebar-bg);
  border-right: var(--jr-docs-sidebar-border);
  font-size: var(--jr-font-size-2);
  scrollbar-width: thin;
  scrollbar-color: var(--rule) transparent;
}
.sidebar::-webkit-scrollbar { width: var(--jr-space-1); }
.sidebar::-webkit-scrollbar-thumb { background: var(--rule); border-radius: 3px; }

.sidebar-brand {
  display: inline-flex;
  align-items: center;
  gap: 0.5rem;
  color: var(--muted);
  font-family: var(--display);
  font-size: var(--jr-font-size-0);
  font-weight: var(--jr-font-weight-medium);
  letter-spacing: 0;
  margin-bottom: var(--jr-space-4);
}
.sidebar-brand::before { content: "\2190"; transition: transform 180ms cubic-bezier(.2,.7,.3,1); }
.sidebar-brand:hover { color: var(--accent); text-decoration: none; }
.sidebar-brand:hover::before { transform: translateX(-3px); }

.sidebar-title {
  font-family: var(--display);
  font-weight: var(--jr-font-weight-bold);
  font-size: var(--jr-font-size-6);
  color: var(--ink);
  letter-spacing: 0;
  line-height: var(--jr-line-height-heading);
  margin-bottom: var(--jr-space-1);
}
.sidebar-meta {
  font-family: var(--display);
  font-size: var(--jr-font-size-0);
  color: var(--muted-2);
  letter-spacing: 0;
  padding-bottom: var(--jr-space-3);
  margin-bottom: var(--jr-space-3);
  border-bottom: var(--jr-border-default);
}

.sidebar-filter { position: relative; margin-bottom: var(--jr-space-4); }
.sidebar-filter input {
  width: 100%;
  min-height: var(--jr-size-control-md);
  padding: var(--jr-space-0) var(--jr-space-8) var(--jr-space-0) var(--jr-space-6);
  font-family: var(--mono);
  font-size: var(--jr-font-size-1);
  color: var(--ink);
  background: rgba(255, 253, 247, 0.64);
  border: var(--jr-border-strong);
  border-radius: var(--jr-radius-control);
  outline: none;
}
.sidebar-filter input:focus { border-color: var(--brand); box-shadow: var(--jr-focus-ring); }
.sidebar-filter input::placeholder { color: var(--muted-2); }
.sidebar-filter::before {
  content: "/";
  position: absolute;
  left: var(--jr-space-3);
  top: 50%;
  transform: translateY(-50%);
  font-family: var(--mono);
  font-size: var(--jr-font-size-1);
  color: var(--muted-2);
  pointer-events: none;
}
.sidebar-filter kbd {
  position: absolute;
  right: var(--jr-space-2);
  top: 50%;
  transform: translateY(-50%);
  font-family: var(--mono);
  font-size: var(--jr-font-size-00);
  color: var(--muted-2);
  background: var(--bg-2);
  border: var(--jr-border-default);
  border-radius: var(--jr-radius-xs);
  padding: 1px 5px;
  pointer-events: none;
}

.sidebar-group { margin-bottom: var(--jr-space-5); }
.sidebar-group h2 {
  font-family: var(--display);
  font-size: var(--jr-font-size-00);
  font-weight: var(--jr-font-weight-bold);
  letter-spacing: 0.04em;
  text-transform: uppercase;
  color: var(--muted-2);
  margin: var(--jr-space-0) var(--jr-space-0) var(--jr-space-2);
}
.sidebar-group ul { list-style: none; margin: var(--jr-space-0); padding: var(--jr-space-0); }
.sidebar-group a {
  display: flex;
  align-items: center;
  min-height: var(--jr-size-control-xs);
  gap: var(--jr-space-2);
  padding: var(--jr-space-0) var(--jr-space-2);
  color: var(--ink-2);
  font-family: var(--display);
  font-size: var(--jr-font-size-0);
  border-left: 2px solid transparent;
  border-radius: var(--jr-radius-none);
  margin-left: -2px;
}
.sidebar-group a:hover,
.sidebar-group a.is-active {
  background: var(--brand-soft);
  color: var(--accent-deep);
  text-decoration: none;
}
.sidebar-group a.is-active { border-left-color: var(--brand); }
.sidebar-group a.is-hidden { display: none; }
.sidebar-group .empty-result {
  display: none;
  font-family: var(--mono);
  font-size: var(--jr-font-size-0);
  color: var(--muted-2);
  padding: var(--jr-space-1) var(--jr-space-2);
  font-style: italic;
}
.sidebar-group.has-no-matches .empty-result { display: block; }

.content {
  padding: var(--jr-space-6) var(--jr-space-6) var(--jr-space-14);
  max-width: calc(var(--content-max) + var(--jr-space-12));
  width: 100%;
}

.page-header { margin-bottom: var(--jr-space-8); }
.breadcrumbs {
  font-family: var(--mono);
  font-size: var(--jr-docs-breadcrumb-font-size);
  color: var(--muted);
  margin-bottom: var(--jr-space-2);
}
.breadcrumbs a { color: var(--muted); }
.breadcrumbs a:hover { color: var(--accent); }
.page-header > .section-header,
.section-header {
  display: flex;
  align-items: baseline;
  justify-content: space-between;
  gap: var(--jr-space-4);
  flex-wrap: wrap;
}
.eyebrow { display: none; }
.page-title {
  font-family: var(--display);
  font-weight: var(--jr-font-weight-bold);
  font-size: var(--jr-font-size-10);
  letter-spacing: 0;
  line-height: var(--jr-line-height-tight);
  color: var(--jr-docs-heading-color);
  margin: var(--jr-space-0);
}
.page-header .section-header > a {
  font-family: var(--mono);
  font-size: var(--jr-font-size-1);
  color: var(--muted);
}
.page-header .section-header > a::before { content: "["; color: var(--muted-2); }
.page-header .section-header > a::after { content: "]"; color: var(--muted-2); }
.page-header .section-header > a:hover { color: var(--accent); text-decoration: none; }

.summary-block {
  margin-top: var(--jr-space-5);
  padding-top: var(--jr-space-4);
  border-top: var(--jr-border-default);
}
.summary-toggle { list-style: none; cursor: default; font-size: 0; margin-bottom: 0; }
.summary-toggle::-webkit-details-marker { display: none; }

.module-docstring { max-width: var(--jr-docs-prose-width); }
.module-docstring h1 {
  font-family: var(--display);
  font-weight: var(--jr-font-weight-bold);
  font-size: var(--jr-font-size-6);
  color: var(--ink);
  margin: var(--jr-space-0) var(--jr-space-0) var(--jr-space-2);
}
.module-docstring h2 {
  font-family: var(--display);
  font-weight: var(--jr-font-weight-bold);
  font-size: var(--jr-font-size-3);
  color: var(--ink);
  margin: var(--jr-space-5) var(--jr-space-0) var(--jr-space-2);
}
.module-docstring p,
.module-docstring li,
.item-docstring p,
.item-docstring li {
  font-size: var(--jr-font-size-3);
  line-height: var(--jr-line-height-body);
  color: var(--ink-2);
}
.module-docstring p,
.item-docstring p { margin: var(--jr-space-0) var(--jr-space-0) var(--jr-space-3); }
.module-docstring ul,
.item-docstring ul { padding-left: var(--jr-space-4); margin: var(--jr-space-0) var(--jr-space-0) var(--jr-space-3); }
.module-docstring li::marker,
.item-docstring li::marker { color: var(--accent); }
.module-docstring pre,
.item-docstring pre,
.item-subitem-docstring pre {
  background: var(--code-bg);
  border: var(--jr-terminal-border);
  border-radius: var(--jr-terminal-radius);
  padding: var(--jr-terminal-body-padding);
  margin: var(--jr-space-2) var(--jr-space-0) var(--jr-space-3);
  overflow-x: auto;
}
.module-docstring pre code,
.item-docstring pre code,
.item-subitem-docstring pre code {
  font-size: var(--jr-terminal-body-font-size);
  line-height: var(--jr-terminal-body-line-height);
  color: var(--code-text);
  background: transparent;
  border: var(--jr-space-0);
  padding: var(--jr-space-0);
  white-space: pre;
}
.token.keyword,
.token.boolean,
.token.builtin { color: var(--jr-color-syntax-keyword); font-weight: var(--jr-font-weight-bold); }
.token.function,
.token.class-name { color: var(--jr-color-syntax-type); }
.token.string,
.token.char { color: var(--jr-color-syntax-string); }
.token.comment { color: var(--code-muted); font-style: italic; }
.token.number,
.token.constant { color: var(--jr-color-syntax-number); }
.token.symbol,
.token.variable { color: var(--jr-color-syntax-success); }
.token.operator,
.token.punctuation { color: var(--code-text); }
p code,
li code,
.item-docstring code,
.item-subitem-docstring code {
  font-family: var(--mono);
  font-size: 0.85em;
  color: var(--accent-deep);
  background: var(--bg-2);
  border: var(--jr-border-default);
  border-radius: var(--jr-radius-xs);
  padding: 0.05em 0.35em;
}

.section-card { margin: var(--jr-space-10) var(--jr-space-0); }
.section-card > .section-header {
  padding-bottom: var(--jr-space-2);
  margin-bottom: var(--jr-space-4);
  border-bottom: var(--jr-border-heavy);
}
.section-card > .section-header h2 {
  font-family: var(--display);
  font-weight: var(--jr-font-weight-bold);
  font-size: var(--jr-font-size-1);
  letter-spacing: 0.04em;
  text-transform: uppercase;
  margin: var(--jr-space-0);
  color: var(--ink);
}
.section-note {
  font-family: var(--mono);
  font-size: var(--jr-font-size-0);
  color: var(--muted-2);
  margin-left: auto;
}
.section-note::before { content: "// "; color: var(--muted-2); }

.item-list { list-style: none; padding: var(--jr-space-0); margin: var(--jr-space-0); }
.item-row {
  display: grid;
  grid-template-columns: 180px 1fr;
  gap: var(--jr-space-5);
  min-height: var(--jr-size-control-lg);
  padding: var(--jr-space-2) var(--jr-space-0);
  border-bottom: var(--jr-border-default);
  align-items: baseline;
}
.item-row:last-child { border-bottom: var(--jr-space-0); }
.item-row .item-name {
  font-family: var(--mono);
  font-size: var(--jr-font-size-2);
  font-weight: var(--jr-font-weight-medium);
  color: var(--accent);
}
.item-row .item-summary {
  font-size: var(--jr-font-size-2);
  color: var(--muted);
  line-height: var(--jr-line-height-ui);
}

.item-detail-list { display: flex; flex-direction: column; gap: var(--jr-space-6); }
.item-detail {
  scroll-margin-top: var(--jr-space-4);
  padding-top: var(--jr-space-5);
  border-top: var(--jr-border-default);
}
.item-detail:first-child { border-top: var(--jr-space-0); padding-top: var(--jr-space-0); }
.item-detail-title {
  font-family: var(--mono);
  font-weight: var(--jr-font-weight-medium);
  font-size: var(--jr-font-size-3);
  margin: var(--jr-space-0) var(--jr-space-0) var(--jr-space-2);
  display: flex;
  align-items: center;
  gap: var(--jr-space-2);
  flex-wrap: wrap;
}
.item-detail-title a { color: var(--ink); font-weight: var(--jr-font-weight-medium); }
.item-detail-title a:hover { color: var(--accent); text-decoration: none; }
.item-detail-title .anchor {
  color: var(--muted-2);
  font-weight: var(--jr-font-weight-regular);
  opacity: 0;
  transition: opacity 140ms ease;
  margin-left: calc(var(--jr-space-1) * -1);
}
.item-detail:hover .anchor { opacity: 1; }
.anchor:hover { color: var(--accent); text-decoration: none; }
.kind {
  min-height: var(--jr-badge-height);
  display: inline-flex;
  align-items: center;
  font-family: var(--jr-badge-font-family);
  font-size: var(--jr-badge-font-size);
  font-weight: var(--jr-font-weight-medium);
  letter-spacing: 0;
  text-transform: none;
  padding: var(--jr-space-0) var(--jr-badge-padding-x);
  border-radius: var(--jr-badge-radius);
  background: var(--jr-color-bg);
  border: 1px solid currentColor;
  line-height: 1;
}
.kind-type { color: var(--k-type); }
.kind-record { color: var(--k-record); }
.kind-variant { color: var(--k-variant); }
.kind-val { color: var(--k-val); }
.kind-fn { color: var(--k-fn); }
.kind-module { color: var(--k-module); }

.item-detail-signature {
  color: var(--ink-2);
  font-weight: var(--jr-font-weight-medium);
}

.item-snippet {
  background: var(--code-bg);
  border: var(--jr-terminal-border);
  border-radius: var(--jr-terminal-radius);
  padding: var(--jr-terminal-body-padding);
  margin: var(--jr-space-0) var(--jr-space-0) var(--jr-space-3);
  overflow-x: auto;
  box-shadow: var(--jr-shadow-none);
}
.item-snippet code {
  display: block;
  font-size: var(--jr-terminal-body-font-size);
  line-height: var(--jr-terminal-body-line-height);
  color: var(--code-text);
  background: transparent;
  padding: var(--jr-space-0);
  border: var(--jr-space-0);
  white-space: pre;
  tab-size: 2;
}

.item-docstring { margin-top: var(--jr-space-2); }
.item-docstring > :first-child,
.module-docstring > :first-child,
.item-subitem-docstring > :first-child { margin-top: var(--jr-space-0); }
.item-docstring > :last-child,
.module-docstring > :last-child,
.item-subitem-docstring > :last-child { margin-bottom: var(--jr-space-0); }

.item-subsections {
  margin-top: var(--jr-space-3);
  padding-left: var(--jr-space-3);
  border-left: 2px solid var(--rule);
}
.item-subsection + .item-subsection { margin-top: var(--jr-space-3); }
.item-subsection h4 {
  font-family: var(--display);
  font-size: var(--jr-font-size-00);
  font-weight: var(--jr-font-weight-bold);
  letter-spacing: 0.04em;
  text-transform: uppercase;
  color: var(--muted);
  margin: var(--jr-space-0) var(--jr-space-0) var(--jr-space-2);
}
.item-subitem-list { display: flex; flex-direction: column; gap: var(--jr-space-2); }
.item-subitem {
  padding: var(--jr-space-3);
  background: var(--surface);
  border: var(--jr-border-default);
  border-radius: var(--jr-radius-panel);
}
.item-subitem-signature {
  font-family: var(--mono);
  font-size: var(--jr-font-size-1);
  line-height: var(--jr-line-height-code);
  color: var(--ink);
  white-space: pre-wrap;
  margin: var(--jr-space-0);
  font-weight: var(--jr-font-weight-medium);
}
.item-subitem-docstring {
  margin-top: var(--jr-space-2);
  padding-top: var(--jr-space-2);
  border-top: var(--jr-border-default);
}
.item-subitem-docstring p {
  font-size: var(--jr-font-size-2);
  line-height: var(--jr-line-height-code);
  color: var(--muted);
  margin: var(--jr-space-0);
}
.item-subitem-docstring p + p { margin-top: var(--jr-space-1); }
.item-subitem-docstring ul { padding-left: var(--jr-space-4); margin: var(--jr-space-1) var(--jr-space-0) var(--jr-space-0); }
.item-subitem-docstring li { font-size: var(--jr-font-size-2); color: var(--muted); }

.empty-state {
  font-family: var(--display);
  font-size: var(--jr-font-size-1);
  color: var(--muted-2);
  padding: var(--jr-space-2) var(--jr-space-0);
}
.empty-state::before { content: "// "; }

.item-detail.is-filter-hidden,
.item-row.is-filter-hidden { display: none; }

@media (max-width: 880px) {
  .docs-shell { grid-template-columns: 1fr; }
  .sidebar {
    position: relative;
    max-height: none;
    border-right: var(--jr-space-0);
    border-bottom: var(--jr-border-default);
    padding: var(--jr-space-4) var(--jr-space-5);
  }
  .content { padding: var(--jr-space-5) var(--jr-space-5) var(--jr-space-12); }
  .item-row { grid-template-columns: 1fr; gap: var(--jr-space-1); }
}
|css}

let assets = [
  ("doc.css", String.trim default_css);
  ("prism-LICENSE.txt", String.trim Vendor_assets.prism_license);
  ("prism-core.min.js", String.trim Vendor_assets.prism_core);
  ("prism-ocaml.min.js", String.trim Vendor_assets.prism_ocaml);
]

let render_empty_state = fun message ->
  "<div class=\"empty-state\">" ^ escape_html message ^ "</div>"

let render_sidebar_group = fun ?(filterable = false) ~title links ->
  if links = [] then
    ""
  else
    "<section class=\"sidebar-group\"" ^ (
      if filterable then
        " data-filterable"
      else
        ""
    ) ^ ">\n" ^ "  <h2>" ^ escape_html title ^ "</h2>\n" ^ "  <ul>\n" ^ (
      links
      |> List.map
        ~fn:(fun (href, label) ->
          "    <li><a href=\"" ^ href ^ "\">" ^ escape_html label ^ "</a></li>")
      |> String.concat "\n"
    ) ^ "\n  </ul>\n" ^ "</section>\n"

type doc_block =
  | Text of string
  | Code of string

let render_code_block = fun snippet ->
  if String.equal snippet "" then
    ""
  else
    "<pre class=\"item-snippet\"><code class=\"language-ocaml\">"
    ^ escape_html snippet
    ^ "</code></pre>\n"

let render_docstring x =
  match x with
  | Some md -> Markdown.compile_gfm md
  | None -> ""

let render_docstring_block = fun ~class_name docstring ->
  match render_docstring docstring with
  | "" -> ""
  | html -> "<div class=\"" ^ class_name ^ "\">" ^ html ^ "</div>\n"

let first_doc_line = fun __tmp1 ->
  match __tmp1 with
  | Some docstring ->
      docstring
      |> String.split ~by:"\n"
      |> List.find ~fn:(fun line -> not (String.equal (String.trim line) ""))
      |> Option.map ~fn:String.trim
  | None -> None

let summary_text = fun ~meta ~signature ~docstring ->
  match first_doc_line docstring with
  | Some line when not (String.equal line "") -> line
  | _ when not (String.equal meta "") -> meta
  | _ -> signature

let item_kind_name = fun __tmp1 ->
  match __tmp1 with
  | Doctree.Module_item -> "Module"
  | Doctree.Type_item -> "Type"
  | Doctree.Value_item -> "Value"
  | Doctree.Function_item -> "Function"

let item_detail_kind = fun (item: Doctree.item) definition ->
  match item.kind with
  | Doctree.Value_item -> "val"
  | Doctree.Function_item -> "fn"
  | Doctree.Module_item -> "module"
  | Doctree.Type_item ->
      let compact =
        definition
        |> String.split ~by:"\n"
        |> List.map ~fn:String.trim
        |> String.concat " "
      in
      if String.contains compact "= {" then
        "record"
      else if String.contains compact "= |" || String.contains compact " | " then
        "variant"
      else
        "type"

let item_uses_inline_signature = fun item ->
  match item.Doctree.kind with
  | Doctree.Value_item
  | Doctree.Function_item -> true
  | Doctree.Module_item
  | Doctree.Type_item -> false

let item_inline_signature = fun (item: Doctree.item) ->
  if not (item_uses_inline_signature item) then
    ""
  else
    let signature = String.trim item.signature in
    if String.equal signature "" then
      ""
    else
      let name = item.name in
      if String.starts_with ~prefix:name signature then
        String.sub
          signature
          ~offset:(String.length name)
          ~len:(String.length signature - String.length name)
        |> String.trim
      else
        signature

let render_item_inline_signature = fun item ->
  match item_inline_signature item with
  | "" -> ""
  | signature -> "<span class=\"item-detail-signature\">" ^ escape_html signature ^ "</span>"

let render_item_row = fun ~href ~name ~kind_label ~meta ~signature ~snippet ~docstring ~anchor ->
  let summary = summary_text ~meta ~signature ~docstring in
  "<li class=\"item-row\"" ^ (
    match anchor with
    | Some id -> " id=\"" ^ id ^ "\""
    | None -> ""
  ) ^ ">\n" ^ "  <a class=\"item-name\" href=\"" ^ href ^ "\">" ^ escape_html name ^ "</a>\n" ^ (
    if String.equal summary "" then
      ""
    else
      "  <div class=\"item-summary\">" ^ escape_html summary ^ "</div>\n"
  ) ^ "</li>"

let render_kind_section = fun ~section_id ~title ~note rows ->
  "<section id=\""
  ^ section_id
  ^ "\" class=\"section-card\">\n"
  ^ "  <div class=\"section-header\">\n"
  ^ "    <h2>"
  ^ escape_html title
  ^ "</h2>\n"
  ^ "    <span class=\"section-note\">"
  ^ escape_html note
  ^ "</span>\n"
  ^ "  </div>\n" ^ (
    if rows = [] then
      render_empty_state ("No " ^ String.lowercase_ascii title ^ " were discovered yet.")
    else
      "<ul class=\"item-list\">\n" ^ String.concat "\n" rows ^ "\n</ul>"
  ) ^ "\n</section>\n"

let render_detail_section = fun ~section_id ~title ~note details ->
  "<section id=\""
  ^ section_id
  ^ "\" class=\"section-card\">\n"
  ^ "  <div class=\"section-header\">\n"
  ^ "    <h2>"
  ^ escape_html title
  ^ "</h2>\n"
  ^ "    <span class=\"section-note\">"
  ^ escape_html note
  ^ "</span>\n"
  ^ "  </div>\n" ^ (
    if details = [] then
      render_empty_state ("No " ^ String.lowercase_ascii title ^ " were discovered yet.")
    else
      "<div class=\"item-detail-list\">\n" ^ String.concat "\n" details ^ "\n</div>"
  ) ^ "\n</section>\n"

let render_dependency_section = fun dependencies ->
  let rows =
    dependencies
    |> List.map
      ~fn:(fun (dep: Doctree.dependency_link) ->
        render_item_row
          ~href:dep.url
          ~name:dep.name
          ~kind_label:"dependency"
          ~meta:("linked docs: " ^ Option.unwrap_or ~default:"dev" dep.version)
          ~signature:""
          ~snippet:""
          ~docstring:None
          ~anchor:None)
  in
  render_kind_section ~section_id:"dependencies" ~title:"Dependencies" ~note:"" rows

let render_package_entry_row = fun (entry: Doctree.package_entry) ->
  let summary =
    match (entry.summary, entry.meta) with
    | (Some summary, _) when not (String.equal summary "") -> summary
    | (_, Some meta) when not (String.equal meta "") -> meta
    | _ -> ""
  in
  "<li class=\"item-row\">\n" ^ (
    match entry.href with
    | Some href ->
        "  <a class=\"item-name\" href=\""
        ^ escape_html href
        ^ "\">"
        ^ escape_html entry.name
        ^ "</a>\n"
    | None -> "  <span class=\"item-name\">" ^ escape_html entry.name ^ "</span>\n"
  ) ^ (
    if String.equal summary "" then
      ""
    else
      "  <div class=\"item-summary\">" ^ escape_html summary ^ "</div>\n"
  ) ^ "</li>"

let render_package_entry_section = fun ~section_id ~title entries ->
  entries
  |> List.map ~fn:render_package_entry_row
  |> render_kind_section ~section_id ~title ~note:""

let render_module_rows = fun ~from_module modules ->
  modules
  |> List.map
    ~fn:(fun (module_doc: Doctree.module_doc) ->
      let href =
        match from_module with
        | Some from_module -> Doctree.relative_module_href ~from_module ~to_module:module_doc
        | None -> Doctree.module_href module_doc
      in
      render_item_row
        ~href
        ~name:(Doctree.module_display_name module_doc)
        ~kind_label:"module"
        ~meta:""
        ~signature:""
        ~snippet:""
        ~docstring:module_doc.docstring
        ~anchor:None)

let render_item_detail = fun (item: Doctree.item) ->
  let definition =
    if String.equal item.snippet "" then
      item.signature
    else
      item.snippet
  in
  let kind = item_detail_kind item definition in
  let render_detail_group (group: Doctree.item_detail_group) =
    "<section class=\"item-subsection\">\n"
    ^ "  <h4>"
    ^ escape_html group.title
    ^ "</h4>\n"
    ^ "  <div class=\"item-subitem-list\">\n" ^ (
      group.details
      |> List.map
        ~fn:(fun (detail: Doctree.item_detail) ->
          "<div class=\"item-subitem\">\n"
          ^ "  <div class=\"item-subitem-signature\">"
          ^ escape_html detail.signature
          ^ "</div>\n" ^ (
            match detail.docstring with
            | Some docstring when not (String.equal docstring "") ->
                "  " ^ render_docstring_block ~class_name:"item-subitem-docstring" (Some docstring)
            | _ -> ""
          ) ^ "</div>")
      |> String.concat "\n"
    ) ^ "\n  </div>\n" ^ "</section>"
  in
  "<article class=\"item-detail\" data-kind=\""
  ^ escape_html kind
  ^ "\" id=\""
  ^ escape_html item.anchor
  ^ "\">\n"
  ^ "  <h3 class=\"item-detail-title\"><span class=\"kind kind-"
  ^ escape_html kind
  ^ "\">"
  ^ escape_html kind
  ^ "</span><a href=\"#"
  ^ escape_html item.anchor
  ^ "\">"
  ^ escape_html item.name
  ^ "</a>"
  ^ render_item_inline_signature item
  ^ "<a class=\"anchor\" href=\"#"
  ^ escape_html item.anchor
  ^ "\" title=\"Permalink\">#</a></h3>\n" ^ (
    if item_uses_inline_signature item then
      ""
    else
      render_code_block definition
  ) ^ render_docstring_block ~class_name:"item-docstring" item.docstring ^ (
    match item.detail_groups with
    | [] -> ""
    | groups ->
        "<div class=\"item-subsections\">\n"
        ^ String.concat "\n" (List.map groups ~fn:render_detail_group)
        ^ "\n</div>\n"
  ) ^ "</article>"

let package_module_name = fun package_name ->
  package_name
  |> String.map
    ~fn:(fun ch ->
      match ch with
      | '-' -> '_'
      | _ -> ch)
  |> String.capitalize_ascii

let package_summary_module = fun (package_doc: Doctree.package_doc) ->
  let expected = package_module_name package_doc.package in
  match List.find
    package_doc.modules
    ~fn:(fun (module_doc: Doctree.module_doc) -> String.equal module_doc.name expected) with
  | Some module_doc -> Some module_doc
  | None -> (
      match package_doc.modules with
      | head :: _ -> Some head
      | [] -> None
    )

let render_module_breadcrumbs = fun package (module_doc: Doctree.module_doc) ->
  let package_link =
    "<a href=\""
    ^ Doctree.relative_href ~from_segments:module_doc.path ~to_segments:[]
    ^ "\">"
    ^ escape_html package
    ^ "</a>"
  in
  let rec loop prefix = fun __tmp1 ->
    match __tmp1 with
    | [] -> []
    | [ last ] -> [ escape_html last ]
    | segment :: rest ->
        let target = prefix @ [ segment ] in
        ("<a href=\""
        ^ Doctree.relative_href ~from_segments:module_doc.path ~to_segments:target
        ^ "\">"
        ^ escape_html segment
        ^ "</a>")
        :: loop target rest
  in
  String.concat " / " (package_link :: loop [] module_doc.path)

let render_module_docstring doc =
  let docstring = render_docstring_block ~class_name:"module-docstring" doc in
  if docstring != "" then
    "<details class=\"summary-block\" open>\n"
    ^ "  <summary class=\"summary-toggle\">Summary</summary>\n"
    ^ docstring
    ^ "</details>\n"
  else
    ""

let rec prefix_segments = fun count acc ->
  if count <= 0 then
    acc
  else
    prefix_segments (count - 1) ("../" ^ acc)

let asset_prefix = fun (module_doc: Doctree.module_doc) ->
  prefix_segments
    (List.length module_doc.Doctree.path)
    ""

let package_shared_asset_prefix = "../../_shared/"

let module_shared_asset_prefix = fun module_doc ->
  asset_prefix module_doc ^ package_shared_asset_prefix

let render_common_head = fun css_href title ->
  "<!doctype html>\n"
  ^ "<html lang=\"en\">\n"
  ^ "<head>\n"
  ^ "  <meta charset=\"utf-8\" />\n"
  ^ "  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\" />\n"
  ^ "  <title>"
  ^ escape_html title
  ^ "</title>\n"
  ^ "  <link rel=\"preconnect\" href=\"https://fonts.googleapis.com\" />\n"
  ^ "  <link rel=\"preconnect\" href=\"https://fonts.gstatic.com\" crossorigin />\n"
  ^ "  <link href=\"https://fonts.googleapis.com/css2?family=Atkinson+Hyperlegible:wght@400;700&family=JetBrains+Mono:wght@400;500;600;700&family=Martian+Mono:wght@400;500;600;700;800&display=swap\" rel=\"stylesheet\" />\n"
  ^ "  <link rel=\"stylesheet\" href=\""
  ^ css_href
  ^ "\" />\n"
  ^ "</head>\n"

let render_common_scripts = fun ~asset_prefix () ->
  "  <script src=\""
  ^ asset_prefix
  ^ "prism-core.min.js\"></script>\n"
  ^ "  <script src=\""
  ^ asset_prefix
  ^ "prism-ocaml.min.js\"></script>\n"
  ^ "  <script>\n"
  ^ "  (function () {\n"
  ^ "    'use strict';\n"
  ^ "    function highlightCode() {\n"
  ^ "      if (window.Prism && window.Prism.highlightAll) window.Prism.highlightAll();\n"
  ^ "    }\n"
  ^ "    function buildFilter() {\n"
  ^ "      const sidebar = document.querySelector('.sidebar');\n"
  ^ "      if (!sidebar) return;\n"
  ^ "      const meta = sidebar.querySelector('.sidebar-meta');\n"
  ^ "      if (!meta || sidebar.querySelector('.sidebar-filter')) return;\n"
  ^ "      const wrap = document.createElement('div');\n"
  ^ "      wrap.className = 'sidebar-filter';\n"
  ^ "      wrap.innerHTML = '<input type=\"text\" placeholder=\"filter symbols&hellip;\" aria-label=\"Filter symbols\" autocomplete=\"off\" spellcheck=\"false\" /><kbd>/</kbd>';\n"
  ^ "      meta.insertAdjacentElement('afterend', wrap);\n"
  ^ "      const input = wrap.querySelector('input');\n"
  ^ "      const groups = sidebar.querySelectorAll('.sidebar-group[data-filterable]');\n"
  ^ "      groups.forEach(group => {\n"
  ^ "        if (!group.querySelector('.empty-result')) {\n"
  ^ "          const empty = document.createElement('div');\n"
  ^ "          empty.className = 'empty-result';\n"
  ^ "          empty.textContent = 'no matches';\n"
  ^ "          group.appendChild(empty);\n"
  ^ "        }\n"
  ^ "      });\n"
  ^ "      function applyFilter(value) {\n"
  ^ "        const needle = value.trim().toLowerCase();\n"
  ^ "        groups.forEach(group => {\n"
  ^ "          const links = group.querySelectorAll('a');\n"
  ^ "          let visible = 0;\n"
  ^ "          links.forEach(link => {\n"
  ^ "            const match = !needle || link.textContent.toLowerCase().includes(needle);\n"
  ^ "            link.classList.toggle('is-hidden', !match);\n"
  ^ "            if (match) visible++;\n"
  ^ "          });\n"
  ^ "          group.classList.toggle('has-no-matches', visible === 0 && needle !== '');\n"
  ^ "        });\n"
  ^ "        document.querySelectorAll('.item-detail, .item-row').forEach(item => {\n"
  ^ "          const nameNode = item.querySelector('.item-detail-title a, .item-name');\n"
  ^ "          const name = nameNode ? nameNode.textContent.toLowerCase() : '';\n"
  ^ "          item.classList.toggle('is-filter-hidden', !!needle && !name.includes(needle));\n"
  ^ "        });\n"
  ^ "      }\n"
  ^ "      input.addEventListener('input', event => applyFilter(event.target.value));\n"
  ^ "      input.addEventListener('keydown', event => {\n"
  ^ "        if (event.key === 'Escape') { input.value = ''; applyFilter(''); input.blur(); }\n"
  ^ "      });\n"
  ^ "      document.addEventListener('keydown', event => {\n"
  ^ "        if (event.target.matches('input, textarea')) return;\n"
  ^ "        if (event.key === '/') { event.preventDefault(); input.focus(); input.select(); }\n"
  ^ "      });\n"
  ^ "    }\n"
  ^ "    function trackActiveSection() {\n"
  ^ "      const sections = document.querySelectorAll('section[id], article[id]');\n"
  ^ "      const linkMap = new Map();\n"
  ^ "      document.querySelectorAll('.sidebar-group a[href^=\"#\"]').forEach(link => linkMap.set(link.getAttribute('href').slice(1), link));\n"
  ^ "      if (!('IntersectionObserver' in window)) return;\n"
  ^ "      const observer = new IntersectionObserver(entries => {\n"
  ^ "        entries.forEach(entry => {\n"
  ^ "          const link = linkMap.get(entry.target.id);\n"
  ^ "          if (!link || !entry.isIntersecting) return;\n"
  ^ "          linkMap.forEach(other => other.classList.remove('is-active'));\n"
  ^ "          link.classList.add('is-active');\n"
  ^ "        });\n"
  ^ "      }, { rootMargin: '-25% 0px -65% 0px' });\n"
  ^ "      sections.forEach(section => observer.observe(section));\n"
  ^ "    }\n"
  ^ "    document.addEventListener('DOMContentLoaded', function () { highlightCode(); buildFilter(); trackActiveSection(); });\n"
  ^ "  })();\n"
  ^ "  </script>\n"

let render_index = fun (package_doc: Doctree.package_doc) ->
  let summary_module = package_summary_module package_doc in
  let section_links = [
    ("#modules", "Modules");
    ("#commands", "Commands");
    ("#executables", "Executables");
    ("#lint-rules", "Lint Rules");
    ("#examples", "Examples");
    ("#dependencies", "Dependencies");
  ]
  in
  let package_modules =
    match summary_module with
    | Some module_doc -> [ module_doc ]
    | None -> []
  in
  let (sidebar_modules, module_rows) =
    (
      package_modules
      |> List.map
        ~fn:(fun (module_doc: Doctree.module_doc) -> (
          Doctree.module_href module_doc,
          module_doc.name
        )),
      render_module_rows ~from_module:None package_modules
    )
  in
  render_common_head (package_shared_asset_prefix ^ "doc.css") (package_doc.package ^ " — docs")
  ^ "<body>\n"
  ^ "  <div class=\"docs-shell\">\n"
  ^ "    <aside class=\"sidebar\">\n"
  ^ "      <a class=\"sidebar-brand\" href=\"index.html\">Riot Docs</a>\n"
  ^ "      <div class=\"sidebar-title\">"
  ^ escape_html package_doc.package
  ^ "</div>\n"
  ^ "      <div class=\"sidebar-meta\">v"
  ^ escape_html package_doc.version
  ^ "</div>\n"
  ^ render_sidebar_group ~title:"Package Items" section_links
  ^ render_sidebar_group ~title:"Modules" sidebar_modules
  ^ "    </aside>\n"
  ^ "    <main class=\"content\">\n"
  ^ "      <header class=\"page-header\">\n"
  ^ "        <h1 class=\"page-title\">"
  ^ escape_html package_doc.package
  ^ "</h1>\n" ^ (
    match summary_module with
    | Some summary_module -> render_module_docstring summary_module.docstring
    | None -> ""
  ) ^ "      </header>\n" ^ render_kind_section
    ~section_id:"modules"
    ~title:"Modules"
    ~note:""
    module_rows ^ render_package_entry_section
    ~section_id:"commands"
    ~title:"Commands"
    package_doc.commands ^ render_package_entry_section
    ~section_id:"executables"
    ~title:"Executables"
    package_doc.executables ^ render_package_entry_section
    ~section_id:"lint-rules"
    ~title:"Lint Rules"
    package_doc.lint_rules ^ render_package_entry_section
    ~section_id:"examples"
    ~title:"Examples"
    package_doc.examples ^ render_dependency_section package_doc.dependencies ^ "    </main>\n" ^ "  </div>\n" ^ render_common_scripts
    ~asset_prefix:package_shared_asset_prefix
    () ^ "</body>\n" ^ "</html>\n"

let render_module = fun (package_doc: Doctree.package_doc) (module_doc: Doctree.module_doc) ->
  let render_item_section kind =
    let details =
      Doctree.items_of_kind kind module_doc.items
      |> List.map ~fn:render_item_detail
    in
    render_detail_section
      ~section_id:(Doctree.item_kind_slug kind)
      ~title:(Doctree.item_kind_title kind)
      ~note:(Doctree.module_full_name module_doc)
      details
  in
  let sidebar_items kind =
    Doctree.items_of_kind kind module_doc.items
    |> List.map ~fn:(fun (item: Doctree.item) -> ("#" ^ item.anchor, item.name))
  in
  let sidebar_modules =
    module_doc.modules
    |> List.map
      ~fn:(fun child_module -> (
        Doctree.relative_module_href ~from_module:module_doc ~to_module:child_module,
        child_module.name
      ))
  in
  render_common_head
    (module_shared_asset_prefix module_doc ^ "doc.css")
    (Doctree.module_full_name module_doc ^ " — docs")
  ^ "<body>\n"
  ^ "  <div class=\"docs-shell\">\n"
  ^ "    <aside class=\"sidebar\">\n"
  ^ "      <a class=\"sidebar-brand\" href=\""
  ^ Doctree.relative_href ~from_segments:module_doc.path ~to_segments:[]
  ^ "\">Back to "
  ^ escape_html package_doc.package
  ^ "</a>\n"
  ^ "      <div class=\"sidebar-title\">"
  ^ escape_html (Doctree.module_full_name module_doc)
  ^ "</div>\n"
  ^ "      <div class=\"sidebar-meta\">"
  ^ escape_html package_doc.version
  ^ " · "
  ^ escape_html (Path.to_string module_doc.source_path)
  ^ "</div>\n"
  ^ render_sidebar_group
    ~title:"Overview"
    [
      ("#modules", "Modules");
      ("#types", "Types");
      ("#values", "Values");
      ("#functions", "Functions");
    ]
  ^ render_sidebar_group ~title:"Modules" sidebar_modules
  ^ render_sidebar_group ~filterable:true ~title:"Types" (sidebar_items Doctree.Type_item)
  ^ render_sidebar_group ~filterable:true ~title:"Values" (sidebar_items Doctree.Value_item)
  ^ render_sidebar_group ~filterable:true ~title:"Functions" (sidebar_items Doctree.Function_item)
  ^ "    </aside>\n"
  ^ "    <main class=\"content\">\n"
  ^ "      <header class=\"page-header\">\n"
  ^ "        <div class=\"breadcrumbs\">"
  ^ render_module_breadcrumbs package_doc.package module_doc
  ^ "</div>\n"
  ^ "        <div class=\"section-header\">\n"
  ^ "          <div>\n"
  ^ "            <div class=\"eyebrow\">Module page</div>\n"
  ^ "            <h1 class=\"page-title\">"
  ^ escape_html (Doctree.module_full_name module_doc)
  ^ "</h1>\n"
  ^ "          </div>\n"
  ^ "          <a href=\"source.html\">src</a>\n"
  ^ "        </div>\n"
  ^ render_module_docstring module_doc.docstring
  ^ "      </header>\n"
  ^ render_kind_section
    ~section_id:"modules"
    ~title:"Modules"
    ~note:(Doctree.module_full_name module_doc)
    (render_module_rows ~from_module:(Some module_doc) module_doc.modules)
  ^ render_item_section Doctree.Type_item
  ^ render_item_section Doctree.Value_item
  ^ render_item_section Doctree.Function_item
  ^ "    </main>\n"
  ^ "  </div>\n"
  ^ render_common_scripts ~asset_prefix:(module_shared_asset_prefix module_doc) ()
  ^ "</body>\n"
  ^ "</html>\n"

let render_module_source = fun
  (package_doc: Doctree.package_doc) (module_doc: Doctree.module_doc) ->
  render_common_head
    (module_shared_asset_prefix module_doc ^ "doc.css")
    (Doctree.module_full_name module_doc ^ " source")
  ^ "<body>\n"
  ^ "  <div class=\"docs-shell\">\n"
  ^ "    <aside class=\"sidebar\">\n"
  ^ "      <a class=\"sidebar-brand\" href=\"index.html\">Back to "
  ^ escape_html (Doctree.module_full_name module_doc)
  ^ "</a>\n"
  ^ "      <div class=\"sidebar-title\">"
  ^ escape_html (Doctree.module_full_name module_doc)
  ^ "</div>\n"
  ^ "      <div class=\"sidebar-meta\">"
  ^ escape_html package_doc.version
  ^ " · "
  ^ escape_html (Path.to_string module_doc.source_path)
  ^ "</div>\n"
  ^ render_sidebar_group ~title:"Pages" [ ("index.html", "Docs"); ("source.html", "Source"); ]
  ^ "    </aside>\n"
  ^ "    <main class=\"content\">\n"
  ^ "      <header class=\"page-header\">\n"
  ^ "        <div class=\"breadcrumbs\">"
  ^ render_module_breadcrumbs package_doc.package module_doc
  ^ " / source</div>\n"
  ^ "        <div class=\"section-header\">\n"
  ^ "          <div>\n"
  ^ "            <div class=\"eyebrow\">Source</div>\n"
  ^ "            <h1 class=\"page-title\">"
  ^ escape_html (Doctree.module_full_name module_doc)
  ^ "</h1>\n"
  ^ "          </div>\n"
  ^ "          <a href=\"index.html\">docs</a>\n"
  ^ "        </div>\n"
  ^ "      </header>\n"
  ^ "<section class=\"section-card\">\n"
  ^ "  <div class=\"section-header\">\n"
  ^ "    <h2>Source</h2>\n"
  ^ "    <span class=\"section-note\">"
  ^ escape_html (Path.to_string module_doc.source_path)
  ^ "</span>\n"
  ^ "  </div>\n"
  ^ render_code_block module_doc.snippet
  ^ "</section>\n"
  ^ "    </main>\n"
  ^ "  </div>\n"
  ^ render_common_scripts ~asset_prefix:(module_shared_asset_prefix module_doc) ()
  ^ "</body>\n"
  ^ "</html>\n"
