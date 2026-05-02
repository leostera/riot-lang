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
  --bg: #f7f6f1;
  --bg-2: #efece2;
  --surface: #fbfaf5;
  --code-bg: #15171b;
  --ink: #15171b;
  --ink-2: #2c2f37;
  --muted: #6a6e78;
  --muted-2: #9aa0aa;
  --rule: #dfdbcc;
  --rule-soft: #ebe7d8;
  --accent: #b14a14;
  --accent-soft: #f4e7d8;
  --accent-deep: #8a3a10;
  --k-type: #8a4a14;
  --k-record: #6a3a86;
  --k-variant: #146b6e;
  --k-fn: #1f4d8f;
  --k-module: #2a6a3e;
  --k-macro: #99461c;
  --sidebar-w: 260px;
  --content-max: 880px;
  --sans: "IBM Plex Sans", -apple-system, BlinkMacSystemFont, "Segoe UI", system-ui, sans-serif;
  --mono: "JetBrains Mono", "IBM Plex Mono", "SF Mono", ui-monospace, Menlo, Consolas, monospace;
}

* { box-sizing: border-box; }
html { -webkit-text-size-adjust: 100%; scroll-behavior: smooth; }
body {
  margin: 0;
  background: var(--bg);
  color: var(--ink);
  font-family: var(--sans);
  font-size: 15px;
  line-height: 1.55;
  -webkit-font-smoothing: antialiased;
  text-rendering: optimizeLegibility;
}

a { color: var(--accent); text-decoration: none; }
a:hover { text-decoration: underline; text-underline-offset: 2px; text-decoration-thickness: 1px; }
code, pre { font-family: var(--mono); }

.docs-shell {
  display: grid;
  grid-template-columns: var(--sidebar-w) 1fr;
  max-width: 1320px;
  margin: 0 auto;
  min-height: 100vh;
}

.sidebar {
  position: sticky;
  top: 0;
  align-self: start;
  max-height: 100vh;
  overflow-y: auto;
  padding: 1.25rem 1rem 2rem 1.25rem;
  border-right: 1px solid var(--rule);
  font-size: 13px;
  scrollbar-width: thin;
  scrollbar-color: var(--rule) transparent;
}
.sidebar::-webkit-scrollbar { width: 6px; }
.sidebar::-webkit-scrollbar-thumb { background: var(--rule); border-radius: 3px; }

.sidebar-brand {
  display: inline-flex;
  align-items: center;
  gap: 0.4em;
  color: var(--muted);
  font-size: 12px;
  letter-spacing: 0.02em;
  margin-bottom: 1rem;
}
.sidebar-brand::before { content: "\2190"; transition: transform 180ms cubic-bezier(.2,.7,.3,1); }
.sidebar-brand:hover { color: var(--accent); text-decoration: none; }
.sidebar-brand:hover::before { transform: translateX(-3px); }

.sidebar-title {
  font-family: var(--mono);
  font-weight: 600;
  font-size: 16px;
  color: var(--ink);
  letter-spacing: -0.01em;
  margin-bottom: 1px;
}
.sidebar-meta {
  font-family: var(--mono);
  font-size: 11px;
  color: var(--muted-2);
  letter-spacing: 0.01em;
  padding-bottom: 0.85rem;
  margin-bottom: 0.75rem;
  border-bottom: 1px solid var(--rule-soft);
}

.sidebar-filter { position: relative; margin-bottom: 1rem; }
.sidebar-filter input {
  width: 100%;
  padding: 0.45rem 2rem 0.45rem 1.85rem;
  font-family: var(--mono);
  font-size: 12px;
  color: var(--ink);
  background: var(--surface);
  border: 1px solid var(--rule);
  border-radius: 3px;
  outline: none;
}
.sidebar-filter input:focus { border-color: var(--accent); box-shadow: 0 0 0 3px var(--accent-soft); }
.sidebar-filter input::placeholder { color: var(--muted-2); }
.sidebar-filter::before {
  content: "/";
  position: absolute;
  left: 0.65rem;
  top: 50%;
  transform: translateY(-50%);
  font-family: var(--mono);
  font-size: 12px;
  color: var(--muted-2);
  pointer-events: none;
}
.sidebar-filter kbd {
  position: absolute;
  right: 0.5rem;
  top: 50%;
  transform: translateY(-50%);
  font-family: var(--mono);
  font-size: 10px;
  color: var(--muted-2);
  background: var(--bg-2);
  border: 1px solid var(--rule);
  border-radius: 2px;
  padding: 1px 5px;
  pointer-events: none;
}

.sidebar-group { margin-bottom: 1.1rem; }
.sidebar-group h2 {
  font-size: 10px;
  font-weight: 600;
  letter-spacing: 0.16em;
  text-transform: uppercase;
  color: var(--muted-2);
  margin: 0 0 0.4rem;
}
.sidebar-group ul { list-style: none; margin: 0; padding: 0; }
.sidebar-group a {
  display: flex;
  align-items: center;
  gap: 0.5rem;
  padding: 3px 0.5rem 3px 0.6rem;
  color: var(--ink-2);
  font-family: var(--mono);
  font-size: 12.5px;
  border-left: 2px solid transparent;
  border-radius: 0 2px 2px 0;
  margin-left: -2px;
}
.sidebar-group a:hover,
.sidebar-group a.is-active {
  background: var(--accent-soft);
  color: var(--accent-deep);
  text-decoration: none;
}
.sidebar-group a.is-active { border-left-color: var(--accent); }
.sidebar-group a.is-hidden { display: none; }
.sidebar-group .empty-result {
  display: none;
  font-family: var(--mono);
  font-size: 11.5px;
  color: var(--muted-2);
  padding: 3px 0.6rem;
  font-style: italic;
}
.sidebar-group.has-no-matches .empty-result { display: block; }

.content {
  padding: 2rem 2.25rem 5rem;
  max-width: calc(var(--content-max) + 4.5rem);
  width: 100%;
}

.page-header { margin-bottom: 2rem; }
.breadcrumbs {
  font-family: var(--mono);
  font-size: 12px;
  color: var(--muted);
  margin-bottom: 0.6rem;
}
.breadcrumbs a { color: var(--muted); }
.breadcrumbs a:hover { color: var(--accent); }
.page-header > .section-header,
.section-header {
  display: flex;
  align-items: baseline;
  justify-content: space-between;
  gap: 1rem;
  flex-wrap: wrap;
}
.eyebrow { display: none; }
.page-title {
  font-family: var(--mono);
  font-weight: 600;
  font-size: 28px;
  letter-spacing: -0.015em;
  line-height: 1.1;
  color: var(--ink);
  margin: 0;
}
.page-header .section-header > a {
  font-family: var(--mono);
  font-size: 12px;
  color: var(--muted);
}
.page-header .section-header > a::before { content: "["; color: var(--muted-2); }
.page-header .section-header > a::after { content: "]"; color: var(--muted-2); }
.page-header .section-header > a:hover { color: var(--accent); text-decoration: none; }

.summary-block {
  margin-top: 1.25rem;
  padding-top: 1rem;
  border-top: 1px solid var(--rule);
}
.summary-toggle { list-style: none; cursor: default; font-size: 0; margin-bottom: 0; }
.summary-toggle::-webkit-details-marker { display: none; }

.module-docstring { max-width: 72ch; }
.module-docstring h1 {
  font-family: var(--sans);
  font-weight: 600;
  font-size: 18px;
  color: var(--ink);
  margin: 0 0 0.6rem;
}
.module-docstring h2 {
  font-family: var(--sans);
  font-weight: 600;
  font-size: 14px;
  color: var(--ink);
  margin: 1.5rem 0 0.5rem;
}
.module-docstring p,
.module-docstring li,
.item-docstring p,
.item-docstring li {
  font-size: 14px;
  line-height: 1.6;
  color: var(--ink-2);
}
.module-docstring p,
.item-docstring p { margin: 0 0 0.7rem; }
.module-docstring ul,
.item-docstring ul { padding-left: 1.15rem; margin: 0 0 0.8rem; }
.module-docstring li::marker,
.item-docstring li::marker { color: var(--accent); }
.module-docstring pre,
.item-docstring pre,
.item-subitem-docstring pre {
  background: var(--surface);
  border: 1px solid var(--rule);
  border-left: 2px solid var(--accent);
  border-radius: 2px;
  padding: 0.55rem 0.75rem;
  margin: 0.5rem 0 0.7rem;
  overflow-x: auto;
}
.module-docstring pre code,
.item-docstring pre code,
.item-subitem-docstring pre code {
  font-size: 12px;
  line-height: 1.55;
  color: var(--ink);
  background: transparent;
  border: 0;
  padding: 0;
  white-space: pre;
}
p code,
li code,
.item-docstring code,
.item-subitem-docstring code {
  font-family: var(--mono);
  font-size: 0.85em;
  color: var(--accent-deep);
  background: var(--bg-2);
  border: 1px solid var(--rule-soft);
  border-radius: 2px;
  padding: 0.05em 0.35em;
}

.section-card { margin: 2.5rem 0; }
.section-card > .section-header {
  padding-bottom: 0.4rem;
  margin-bottom: 1rem;
  border-bottom: 1px solid var(--ink);
}
.section-card > .section-header h2 {
  font-family: var(--sans);
  font-weight: 600;
  font-size: 13px;
  letter-spacing: 0.16em;
  text-transform: uppercase;
  margin: 0;
  color: var(--ink);
}
.section-note {
  font-family: var(--mono);
  font-size: 11.5px;
  color: var(--muted-2);
  margin-left: auto;
}
.section-note::before { content: "// "; color: var(--muted-2); }

.item-list { list-style: none; padding: 0; margin: 0; }
.item-row {
  display: grid;
  grid-template-columns: 180px 1fr;
  gap: 1.25rem;
  padding: 0.55rem 0;
  border-bottom: 1px solid var(--rule-soft);
  align-items: baseline;
}
.item-row:last-child { border-bottom: 0; }
.item-row .item-name {
  font-family: var(--mono);
  font-size: 13.5px;
  font-weight: 500;
  color: var(--accent);
}
.item-row .item-summary {
  font-size: 13.5px;
  color: var(--muted);
  line-height: 1.5;
}

.item-detail-list { display: flex; flex-direction: column; gap: 1.6rem; }
.item-detail {
  scroll-margin-top: 1rem;
  padding-top: 1.25rem;
  border-top: 1px dashed var(--rule);
}
.item-detail:first-child { border-top: 0; padding-top: 0; }
.item-detail-title {
  font-family: var(--mono);
  font-weight: 500;
  font-size: 14.5px;
  margin: 0 0 0.55rem;
  display: flex;
  align-items: center;
  gap: 0.55rem;
  flex-wrap: wrap;
}
.item-detail-title a { color: var(--ink); font-weight: 600; }
.item-detail-title a:hover { color: var(--accent); text-decoration: none; }
.item-detail-title .anchor {
  color: var(--muted-2);
  font-weight: 400;
  opacity: 0;
  transition: opacity 140ms ease;
  margin-left: -0.2rem;
}
.item-detail:hover .anchor { opacity: 1; }
.anchor:hover { color: var(--accent); text-decoration: none; }
.kind {
  font-family: var(--mono);
  font-size: 10px;
  font-weight: 600;
  letter-spacing: 0.06em;
  text-transform: uppercase;
  padding: 1px 6px;
  border-radius: 2px;
  background: var(--surface);
  border: 1px solid currentColor;
  line-height: 1.4;
}
.kind-type { color: var(--k-type); }
.kind-record { color: var(--k-record); }
.kind-variant { color: var(--k-variant); }
.kind-fn { color: var(--k-fn); }
.kind-module { color: var(--k-module); }
.kind-macro { color: var(--k-macro); }

.item-snippet {
  background: var(--code-bg);
  border-radius: 4px;
  padding: 0.7rem 0.95rem;
  margin: 0 0 0.6rem;
  overflow-x: auto;
  box-shadow: inset 0 0 0 1px rgba(255, 255, 255, 0.04);
}
.item-snippet code {
  display: block;
  font-size: 12.5px;
  line-height: 1.55;
  color: #e6e2d6;
  background: transparent;
  padding: 0;
  border: 0;
  white-space: pre;
  tab-size: 2;
}

.item-docstring { margin-top: 0.5rem; }
.item-docstring > :first-child,
.module-docstring > :first-child,
.item-subitem-docstring > :first-child { margin-top: 0; }
.item-docstring > :last-child,
.module-docstring > :last-child,
.item-subitem-docstring > :last-child { margin-bottom: 0; }

.item-subsections {
  margin-top: 0.75rem;
  padding-left: 0.85rem;
  border-left: 2px solid var(--rule);
}
.item-subsection + .item-subsection { margin-top: 0.85rem; }
.item-subsection h4 {
  font-family: var(--mono);
  font-size: 10.5px;
  font-weight: 500;
  letter-spacing: 0.16em;
  text-transform: uppercase;
  color: var(--muted);
  margin: 0 0 0.5rem;
}
.item-subitem-list { display: flex; flex-direction: column; gap: 0.5rem; }
.item-subitem {
  padding: 0.45rem 0.65rem;
  background: var(--surface);
  border: 1px solid var(--rule-soft);
  border-radius: 2px;
}
.item-subitem-signature {
  font-family: var(--mono);
  font-size: 12.5px;
  line-height: 1.55;
  color: var(--ink);
  white-space: pre-wrap;
  margin: 0;
  font-weight: 500;
}
.item-subitem-docstring {
  margin-top: 0.35rem;
  padding-top: 0.35rem;
  border-top: 1px solid var(--rule-soft);
}
.item-subitem-docstring p {
  font-size: 13px;
  line-height: 1.55;
  color: var(--muted);
  margin: 0;
}
.item-subitem-docstring p + p { margin-top: 0.3rem; }
.item-subitem-docstring ul { padding-left: 1rem; margin: 0.25rem 0 0; }
.item-subitem-docstring li { font-size: 13px; color: var(--muted); }

.empty-state {
  font-family: var(--mono);
  font-size: 12.5px;
  color: var(--muted-2);
  padding: 0.5rem 0;
}
.empty-state::before { content: "// "; }

.item-detail.is-filter-hidden,
.item-row.is-filter-hidden { display: none; }

@media (max-width: 880px) {
  .docs-shell { grid-template-columns: 1fr; }
  .sidebar {
    position: relative;
    max-height: none;
    border-right: 0;
    border-bottom: 1px solid var(--rule);
    padding: 1rem 1.25rem;
  }
  .content { padding: 1.5rem 1.25rem 4rem; }
  .item-row { grid-template-columns: 1fr; gap: 0.2rem; }
}

::selection { background: var(--accent); color: #fbfaf5; }
|css}

let assets = [ ("assets/doc.css", String.trim default_css); ]

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
  | Doctree.Function_item -> "Function"
  | Doctree.Macro_item -> "Macro"

let item_detail_kind = fun (item: Doctree.item) definition ->
  match item.kind with
  | Doctree.Function_item -> "fn"
  | Doctree.Module_item -> "module"
  | Doctree.Macro_item -> "macro"
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
  ^ "</a><a class=\"anchor\" href=\"#"
  ^ escape_html item.anchor
  ^ "\" title=\"Permalink\">#</a></h3>\n"
  ^ render_code_block definition
  ^ render_docstring_block ~class_name:"item-docstring" item.docstring ^ (
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
  ^ "  <link href=\"https://fonts.googleapis.com/css2?family=IBM+Plex+Sans:ital,wght@0,400;0,500;0,600;0,700;1,400&family=JetBrains+Mono:ital,wght@0,400;0,500;0,600;1,400&display=swap\" rel=\"stylesheet\" />\n"
  ^ "  <link rel=\"stylesheet\" href=\""
  ^ css_href
  ^ "\" />\n"
  ^ "</head>\n"

let render_common_scripts = fun () ->
  "  <script>\n"
  ^ "  (function () {\n"
  ^ "    'use strict';\n"
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
  ^ "    document.addEventListener('DOMContentLoaded', function () { buildFilter(); trackActiveSection(); });\n"
  ^ "  })();\n"
  ^ "  </script>\n"

let render_index = fun (package_doc: Doctree.package_doc) ->
  let summary_module = package_summary_module package_doc in
  let section_links =
    match summary_module with
    | Some summary_module ->
        let links = [ ("#modules", "Modules"); ] in
        let links =
          if Doctree.items_of_kind Doctree.Type_item summary_module.items = [] then
            links
          else
            links @ [ ("#types", "Types"); ]
        in
        let links =
          if Doctree.items_of_kind Doctree.Function_item summary_module.items = [] then
            links
          else
            links @ [ ("#functions", "Functions"); ]
        in
        let links =
          if Doctree.items_of_kind Doctree.Macro_item summary_module.items = [] then
            links
          else
            links @ [ ("#macros", "Macros"); ]
        in
        if package_doc.dependencies = [] then
          links
        else
          links @ [ ("#dependencies", "Dependencies"); ]
    | None ->
        if package_doc.dependencies = [] then
          [ ("#modules", "Modules"); ]
        else
          [ ("#modules", "Modules"); ("#dependencies", "Dependencies"); ]
  in
  let (sidebar_modules, module_rows) =
    match summary_module with
    | Some summary_module ->
        let children = summary_module :: summary_module.modules in
        (
          children
          |> List.map
            ~fn:(fun (module_doc: Doctree.module_doc) -> (
              Doctree.module_href module_doc,
              module_doc.name
            )),
          render_module_rows ~from_module:None children
        )
    | None -> (
      package_doc.modules
      |> List.map
        ~fn:(fun (module_doc: Doctree.module_doc) -> (
          Doctree.module_href module_doc,
          module_doc.name
        )),
      render_module_rows ~from_module:None package_doc.modules
    )
  in
  let render_item_section kind =
    match summary_module with
    | None -> ""
    | Some summary_module ->
        let rows =
          Doctree.items_of_kind kind summary_module.items
          |> List.map
            ~fn:(fun (item: Doctree.item) ->
              render_item_row
                ~href:(Doctree.module_href summary_module ^ "#" ^ item.anchor)
                ~name:item.name
                ~kind_label:(Doctree.item_kind_label item.kind)
                ~meta:""
                ~signature:item.signature
                ~snippet:""
                ~docstring:item.docstring
                ~anchor:None)
        in
        render_kind_section
          ~section_id:(Doctree.item_kind_slug kind)
          ~title:(Doctree.item_kind_title kind)
          ~note:""
          rows
  in
  render_common_head "assets/doc.css" (package_doc.package ^ " — docs")
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
    module_rows ^ render_item_section Doctree.Type_item ^ render_item_section Doctree.Function_item ^ render_item_section
    Doctree.Macro_item ^ render_dependency_section package_doc.dependencies ^ "    </main>\n" ^ "  </div>\n" ^ render_common_scripts
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
    (asset_prefix module_doc ^ "assets/doc.css")
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
      ("#functions", "Functions");
      ("#macros", "Macros");
    ]
  ^ render_sidebar_group ~title:"Modules" sidebar_modules
  ^ render_sidebar_group ~filterable:true ~title:"Types" (sidebar_items Doctree.Type_item)
  ^ render_sidebar_group ~filterable:true ~title:"Functions" (sidebar_items Doctree.Function_item)
  ^ render_sidebar_group ~filterable:true ~title:"Macros" (sidebar_items Doctree.Macro_item)
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
  ^ render_item_section Doctree.Function_item
  ^ render_item_section Doctree.Macro_item
  ^ "    </main>\n"
  ^ "  </div>\n"
  ^ render_common_scripts ()
  ^ "</body>\n"
  ^ "</html>\n"

let render_module_source = fun
  (package_doc: Doctree.package_doc) (module_doc: Doctree.module_doc) ->
  render_common_head
    (asset_prefix module_doc ^ "assets/doc.css")
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
  ^ render_common_scripts ()
  ^ "</body>\n"
  ^ "</html>\n"
