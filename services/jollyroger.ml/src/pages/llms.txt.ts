import type { APIRoute } from "astro";

export const prerender = true;

const llmsTxt = `# Riot's Jolly Roger

> Riot's Jolly Roger is the design system for RiotML: the flag, voice, foundations, components, and application rules for web, CLI, docs, packages, diagnostics, reports, and releases.

Canonical:
- Human page: https://jollyroger.ml/
- Agent page: https://jollyroger.ml/llms.txt
- Scope: design, writing, components, documentation, registry surfaces, CLI output, diagnostics, install flows, static reports

Core thesis:
- There are many OCaml stacks. This one is mine.
- Riot is an opinionated ML-family stack built for shipping: unified tooling, actor-oriented by default, multi-core ready, type-safe, documented, packaged, and designed as one piece.
- Jolly Roger translates Riot's values into readable, visible, navigable surfaces.

Principles:
- Optimize for Developer Joy: reduce friction without hiding truth. Make information clear, feedback actionable, and iteration fast.
- Design as One Piece: make every Riot surface feel like one coherent stack, not a toolkit of unrelated options.
- Build for Shipping: help people understand, install, try, build, test, publish, debug, and keep going.
- Human-led, agent-ready: keep human taste and judgment in charge, but expose stable structure for tools.
- Trust Builders With Power: show mechanism, signatures, generated artifacts, runtime behavior, platform caveats, and deeper paths.

Golden paths:
- A golden path is the preferred route designed, tested, documented, and supported end to end.
- Make golden paths visible: install this, run this, add this package, read these docs, fix this error, publish this way.
- A golden path is not a prison; users can step off it, but the core experience should not require assembling workflows from pieces.

Developer joy in practice:
- CLI commands always give prompt feedback with enough information to understand what is happening.
- Failures always include a clear next step: fix, report, inspect, or run a command.
- Search results include adjacent information: package name, version, docs status, freshness, owner, related modules, and context.

Voice:
- Direct: say the thing. Avoid hedging and vague platform language.
- Opinionated: state the preferred path instead of offering every possible workflow.
- Helpful: provide contextual help and concrete next steps.
- Joyful: be bold, precise, and human; remove friction, slowness, bugginess, vague feedback, and dead ends.

Agent-ready voice:
- Prefer stable labels and predictable structure.
- Useful labels: package, source, wanted, found, expected, available, fix, next, docs.
- Provide llms.txt, uniform --json output where relevant, stable anchors, machine-readable diagnostics, and copyable commands.
- Do not hide messy data or error details in prose.

Agent-ready example:
package: std
source:  packages/app/riot.toml
wanted:  std
found:   no matching dependency
fix:     riot add std

Agent-ready JSON shape:
{
  "event": "dependency.resolve.failed",
  "package": "std",
  "source": "packages/app/riot.toml",
  "wanted": "std",
  "found": null,
  "fix": "riot add std"
}

Coding voice:
- Understandability first: code is read more than written. Name, structure, document, and design for comprehension.
- Malleability second: code should be easy to change, especially in agentic development.
- Correctness third: use tests, properties, fuzzing, sanitizers, and clear contracts.
- Performance fourth: code should be fast and honest about cost.

Foundations:
- Grid: base unit is 5px. Use the scale for spacing, rhythm, controls, rows, cards, hero spacing, and examples.
- Color: Paper is reading. Ink is text and structure. Coal is terminal and code gravity. Action red is identity, danger, and decisive emphasis, not routine button fill. Link rust is prose navigation, source links, anchors, and quiet utility navigation. Success mint is completion. Warning amber is caution. Reference blue is technical references, syntax-like navigation, and informational states.
- Typography: Martian Mono for headings, labels, buttons, and interface chrome. Atkinson Hyperlegible for prose. JetBrains Mono for code, commands, JSON, signatures, versions, paths, and terminal output.

Consumable artifacts:
- Generated API docs should vendor /jolly-roger-docs.css into _shared/.
- The docs artifact is intentionally quiet: no grid background, square borders, compact rows, terminal-dark code blocks, docs sidebar, module header, item row, item detail, signature, search, callout, code snippet, Prism mappings, and API kind labels.
- Fonts are not imported by the artifact. Host pages may bundle or link Martian Mono, JetBrains Mono, and Atkinson Hyperlegible; otherwise the fallback stacks apply.

Generated API docs policy:
- Dense technical docs use compact rows, stable anchors, mono labels, and terminal-dark code blocks.
- Do not use decorative cards for repeated API items.
- Syntax highlighting uses Prism token mappings from /jolly-roger-docs.css.
- Kind labels are square structural markers, not rounded badges: lowercase fn, val, type, module, variant, record, constructor, field; transparent background; 1px currentColor border; radius 0; display font; 10px; 700 weight; line-height 1.4; horizontal padding 5px.
- Static docs generators should use the docs artifact's minimal subset instead of copying the full design-system token file.

Foundation token values:
- Identity: --jr-name="Riot’s Jolly Roger"; --jr-version="0.1.0".
- Red scale: red-50 #fff0f3; red-100 #ffdce3; red-200 #ffb8c5; red-300 #ff8aa0; red-400 #ff6f87; red-500 #ef233c; red-600 #c91f38; red-700 #9f172c; red-800 #8f1b2d; red-900 #5f1421.
- Rust scale: rust-50 #fff7ef; rust-100 #f4e7d8; rust-200 #e3c7aa; rust-300 #d3a676; rust-400 #c4773a; rust-500 #b14a14; rust-600 #99461c; rust-700 #8a3a10; rust-800 #6f2d0c; rust-900 #4a1d08.
- Ink scale: ink-50 #f3f0f5; ink-100 #ddd8e2; ink-200 #bdb4c5; ink-300 #9aa0aa; ink-400 #9aa0aa; ink-500 #5b5462; ink-600 #413a48; ink-700 #2b2630; ink-800 #1d1a21; ink-900 #151317; ink-950 #0e0d10.
- Paper scale: paper-50 #fffdf7; paper-100 #fff8ed; paper-200 #f5ecdc; paper-300 #e8dcc8; paper-400 #d8cbb7; paper-500 #b8a78e; paper-600 #927f66; paper-700 #6f5f4d; paper-800 #4d4236; paper-900 #302920.
- Mint scale: mint-50 #eafff6; mint-100 #ccfce9; mint-200 #9cf5d5; mint-300 #65e7bd; mint-400 #36d19f; mint-500 #24c08d; mint-600 #16986e; mint-700 #0f7354; mint-800 #0b4e3a; mint-900 #073125.
- Amber scale: amber-50 #fff8e6; amber-100 #ffe9ad; amber-200 #ffd66f; amber-300 #ffc43d; amber-400 #f6b91f; amber-500 #f0b429; amber-600 #c78b00; amber-700 #936600; amber-800 #654600; amber-900 #3c2a00.
- Blue scale: blue-50 #eef6ff; blue-100 #d7eaff; blue-200 #acd3ff; blue-300 #7bb8ff; blue-400 #4d98ff; blue-500 #2777ff; blue-600 #135bd1; blue-700 #0d459f; blue-800 #0a306d; blue-900 #071d42.
- Green scale: green-50 #ecfff0; green-100 #d1f8da; green-200 #a6ecb5; green-300 #78dc8e; green-400 #4bc96a; green-500 #2f9f58; green-600 #247e45; green-700 #1b5f35; green-800 #124024; green-900 #0b2917.
- Light mode semantic colors: action #ff6f87; success #36d19f; warning #f6b91f; reference #4d98ff; link #b14a14; link-hover #8a3a10; brand/action #ff6f87; brand-hover #ef233c; brand-active #c91f38; background #fff8ed; background-subtle #f5ecdc; raised rgba(255,253,247,0.62); inset rgba(255,253,247,0.46); code rgba(255,253,247,0.54); coal #0e0d10; terminal #0e0d10; text #151317; text-strong #0e0d10; text-muted #5b5462; text-subtle #9aa0aa; inverse #fffdf7; border rgba(21,19,23,0.14); border-strong rgba(21,19,23,0.22); border-heavy rgba(21,19,23,0.34).
- Dark mode semantic colors: action #ff8aa0; success #65e7bd; warning #ffc43d; reference #7bb8ff; link #c4773a; link-hover #d3a676; brand/action #ff8aa0; background #0e0d10; background-subtle #151317; raised rgba(255,248,237,0.045); inset rgba(255,248,237,0.06); code rgba(255,248,237,0.08); coal #2b2630; terminal #2b2630; text #fff8ed; text-strong #fffdf7; text-muted rgba(255,248,237,0.64); text-subtle rgba(255,248,237,0.46); inverse #0e0d10; border rgba(255,248,237,0.14); border-strong rgba(255,248,237,0.28); border-heavy rgba(255,248,237,0.52).
- Syntax colors: text #e6e2d6; muted rgba(230,226,214,0.68); keyword #7bb8ff; string #ffc43d; number #65e7bd; type #b58cff; function #7bb8ff; comment rgba(230,226,214,0.46); error #ff8aa0; warning #ffc43d; success #65e7bd; terminal prompt #65e7bd.
- Typefaces: display "Martian Mono", "JetBrains Mono", "IBM Plex Mono", "SFMono-Regular", ui-monospace, Menlo, Consolas, monospace. Mono "JetBrains Mono", "IBM Plex Mono", "SFMono-Regular", ui-monospace, Menlo, Consolas, monospace. Sans "Atkinson Hyperlegible", -apple-system, BlinkMacSystemFont, "Segoe UI", system-ui, sans-serif.
- Font weights: regular 400; medium 500; bold 700; heavy 700; black 800; h1 900.
- Font sizes: 00 10px; 0 11px; 1 12px; 2 13px; 3 14px; 4 15px; 5 16px; 6 18px; 7 20px; 8 24px; 9 30px; 10 36px; 11 44px; 12 56px; 13 72px; 14 84px.
- Line heights: solid 1; tight 1.12; heading 1.18; ui 1.35; body 1.58; code 1.55; loose 1.7.
- Letter spacing: none 0; tight 0; heading-1 0; heading-2 0; heading-3 0; display 0; label 0.04em; wide 0.08em.
- Heading tokens: h1 clamp(36px, 5vw, 72px), weight 900, line-height 1, letter-spacing 0; h2 clamp(30px, 3vw, 44px), line-height 1.12, letter-spacing 0; h3 18px, line-height 1.18, letter-spacing 0; h4 12px, line-height 1.25, letter-spacing 0.
- Body tokens: body family Atkinson Hyperlegible stack, size 15px, line-height 1.58, color #151317. Code family JetBrains Mono stack, size 12px, line-height 1.55. Label family Martian Mono stack, size 11px, line-height 1.35, letter-spacing 0.04em, weight 700.
- Spacing scale: unit 5px; space-0 0; space-1 5px; space-2 10px; space-3 15px; space-4 20px; space-5 25px; space-6 30px; space-7 35px; space-8 40px; space-9 45px; space-10 50px; space-11 55px; space-12 60px; space-14 70px; space-16 80px; space-20 100px; space-24 120px.
- Control sizes: xs 25px; sm 30px; md 35px; lg 40px; xl 45px.
- Row sizes: compact 30px; dense 35px; default 40px; roomy 50px.
- Icon sizes: xs 15px; sm 20px; md 25px; lg 35px; xl 50px.
- Mark sizes: sm 30px; md 40px; lg 60px.
- Radius: none 0; hairline 0; xs 0; sm 0; md 0; status 0; terminal 0; control 0; card 0; panel 0.
- Borders: width-0 0; width-1 1px; width-2 2px; width-3 3px. Default border 1px solid rgba(21,19,23,0.14). Strong border 1px solid rgba(21,19,23,0.22). Heavy border 1px solid rgba(21,19,23,0.34). Brand/danger/warning/success/info borders are 2px solid their semantic color.
- Layout sizes: page width 1080px; wide width 1320px; reading width 760px; prose width 68ch; sidebar 280px; toc 320px; local nav 170px; chapter gap 40px; section padding-y 60px; content padding-x 30px; topbar 50px; grid size 25px.
- Hero tokens: min-height 360px; padding-y 50px; content width 900px; title max width 900px; display size clamp(44px, 5vw, 72px); lead max width 52ch; lead size 15px.

Component rules:
- Buttons: direct, compact, imperative. Use primary for the main action, default/secondary for normal actions, dark for source-adjacent technical actions, tertiary/link for quiet commands.
- Tables: use when comparison is the task. Keep rows compact. Include captions. Use mono for names, versions, paths, and numeric metadata.
- Forms and inputs: keep labels close to values. Group controls by the decision they support. Do not hide labels behind placeholders.
- Badges: use for compact status and metadata: category, version, stability, readiness, caution.
- Callouts: interrupt only when the information changes what the reader should do. Variants: note, caution, danger, success.
- Cards: use for one bounded object. Do not use cards to replace comparison tables.
- Code: examples are product UI. Keep commands real, copyable, structured, and close to the explanation.

Applications:
- Web: show the stack, real commands, real code, and the path to install, learn, and ship. Avoid startup SaaS air.
- Registry: help users find, compare, evaluate, and install packages. Show name, version, docs status, freshness, owner, install command, and compatibility context.
- Docs: show package, version, module, breadcrumbs, search, local navigation, signatures, examples, and stability markers.
- CLI output: plain text still carries the brand. Show status, package, version, source when relevant, diagnostic IDs, and copyable next commands.
- Error pages: diagnostics in web form. Show problem, context, possible cause, fix, and fallback.

Diagnostics:
- Name the problem first.
- Show package, source, wanted, found or expected, why it happened, and the fix.
- Use stable labels.
- End with one concrete command, edit, or docs link.
- Keep the diagnostic readable as plain text.

Anti-patterns:
- startup SaaS air
- corporate devrel filler
- academic beige
- mascot-core
- fake terminal theater
- generic developer-platform copy
- spreadsheet UI
- configuration worship
- compatibility as a cage
- cleverness over clarity
- vague errors
- decorative dashboards
- hidden metadata
- pages with no next action
- choice as a substitute for design
- wrapping old tools and calling it a new stack

Review checks:
- Does this optimize for developer joy?
- Does this feel like one designed stack?
- Does this help someone ship?
- Is it human-led and agent-ready?
- Does it give power without hiding mechanism?
- Can a human understand it quickly?
- Can an agent extract important fields without guessing?
`;

export const GET: APIRoute = () =>
  new Response(llmsTxt, {
    headers: {
      "Content-Type": "text/plain; charset=utf-8",
      "Cache-Control": "public, max-age=3600",
    },
  });
