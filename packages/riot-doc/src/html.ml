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

let assets = [
  (
    "assets/doc.css",
    [
      "@import url('https://fonts.googleapis.com/css2?family=Source+Sans+3:wght@400;500;600;700&family=Space+Grotesk:wght@400;500;700&display=swap');";
      ":root {";
      "  --bg: oklch(1 0 0);";
      "  --surface: oklch(1 0 0);";
      "  --surface-soft: #fff5f6;";
      "  --surface-strong: #ffe8ec;";
      "  --border: #f2d6dc;";
      "  --border-strong: #e6b8c2;";
      "  --text: oklch(0.145 0.008 326);";
      "  --muted: #7d4b54;";
      "  --accent: #ff354f;";
      "  --accent-soft: #fff4f6;";
      "  --accent-strong: #c41f45;";
      "  --code-string: #0f766e;";
      "  --code-title: #9a3412;";
      "  --code-number: #b45309;";
      "  --code-type: #0369a1;";
      "  --ring: rgba(255, 53, 79, 0.12);";
      "  --radius: 0.7rem;";
      "}";
      "* { box-sizing: border-box; }";
      "html { scroll-behavior: smooth; }";
      "body {";
      "  margin: 0;";
      "  background: var(--bg);";
      "  color: var(--text);";
      "  font-family: 'Source Sans 3', sans-serif;";
      "}";
      "a { color: var(--accent); text-decoration: none; }";
      "a:hover { text-decoration: underline; }";
      "code, pre { font-family: 'SFMono-Regular', Consolas, monospace; }";
      "ul { list-style: none; padding: 0; margin: 0; }";
      ".docs-shell {";
      "  width: min(72rem, calc(100vw - 24px));";
      "  margin: 0 auto;";
      "  padding: 24px 0 40px;";
      "  display: grid;";
      "  grid-template-columns: 248px minmax(0, 1fr);";
      "  gap: 16px;";
      "}";
      ".sidebar {";
      "  position: relative;";
      "  border: 1px solid var(--border);";
      "  border-radius: var(--radius);";
      "  background: var(--surface);";
      "  padding: 16px;";
      "}";
      ".sidebar-brand { display: block; font-size: 0.76rem; font-weight: 700; letter-spacing: 0.14em; text-transform: uppercase; color: var(--muted); margin-bottom: 16px; }";
      ".sidebar-title { font-family: 'Space Grotesk', sans-serif; font-size: 1.02rem; font-weight: 500; margin: 0 0 4px; }";
      ".sidebar-meta { color: var(--muted); font-size: 0.9rem; margin: 0 0 14px; }";
      ".sidebar-group + .sidebar-group { margin-top: 18px; padding-top: 18px; border-top: 1px solid var(--border); }";
      ".sidebar-group h2 { margin: 0 0 10px; font-size: 0.8rem; letter-spacing: 0.08em; text-transform: uppercase; color: var(--muted); }";
      ".sidebar-group li + li { margin-top: 8px; }";
      ".sidebar-group a { color: var(--text); }";
      ".sidebar-group a:hover { color: var(--accent); }";
      ".content { min-width: 0; }";
      ".page-header, .section-card { border: 1px solid var(--border); border-radius: var(--radius); background: var(--surface); }";
      ".page-header { padding: 20px 22px; }";
      ".section-card { padding: 18px 22px; margin-top: 12px; }";
      ".breadcrumbs { color: var(--muted); font-size: 0.92rem; margin-bottom: 8px; }";
      ".eyebrow { display: inline-block; padding: 4px 8px; border-radius: 0.35rem; background: var(--surface-soft); color: var(--muted); font-size: 0.72rem; font-weight: 700; letter-spacing: 0.08em; text-transform: uppercase; }";
      ".page-title { margin: 8px 0 0; font-family: 'Space Grotesk', sans-serif; font-size: clamp(1.7rem, 3vw, 2.4rem); line-height: 1.08; font-weight: 500; letter-spacing: -0.03em; }";
      ".page-subtitle { color: var(--muted); font-size: 1rem; line-height: 1.6; max-width: 72ch; margin: 8px 0 0; }";
      ".hero-stats { display: flex; flex-wrap: wrap; gap: 10px; margin: 18px 0 0; }";
      ".pill { display: inline-flex; align-items: center; gap: 6px; padding: 8px 12px; border-radius: 999px; border: 1px solid var(--border); background: var(--surface-soft); color: var(--muted); font-size: 0.9rem; }";
      ".pill strong { color: var(--text); }";
      ".status_ok { background: var(--ok-soft); color: var(--ok); border-color: transparent; }";
      ".status_miss { background: var(--danger-soft); color: var(--danger); border-color: transparent; }";
      ".search-block { margin-top: 22px; }";
      ".search-label { display: block; font-size: 0.84rem; font-weight: 700; color: var(--muted); margin-bottom: 8px; text-transform: uppercase; letter-spacing: 0.08em; }";
      "#search { width: 100%; padding: 14px 16px; border-radius: 14px; border: 1px solid var(--border-strong); background: #ffffff; color: var(--text); font: inherit; }";
      "#search-results { margin-top: 12px; display: grid; gap: 10px; }";
      "#search-results li { padding: 12px 14px; border: 1px solid var(--border); background: var(--surface-soft); border-radius: 14px; }";
      ".section-header { display: flex; justify-content: space-between; align-items: baseline; gap: 12px; margin-bottom: 14px; }";
      ".section-header h2 { margin: 0; font-size: 1.22rem; }";
      ".section-note { color: var(--muted); font-size: 0.92rem; }";
      ".item-list { display: grid; }";
      ".item-row { display: grid; grid-template-columns: minmax(8rem, 14rem) minmax(0, 1fr); gap: 14px; align-items: baseline; border-bottom: 1px solid color-mix(in srgb, var(--border) 70%, transparent); padding: 10px 0; }";
      ".item-row:last-child { border-bottom: 0; padding-bottom: 0; }";
      ".item-row:first-child { padding-top: 0; }";
      ".item-row:target { border-radius: 0.4rem; background: var(--surface-soft); box-shadow: 0 0 0 4px var(--ring); padding-left: 10px; padding-right: 10px; }";
      ".item-name { font-family: 'Space Grotesk', sans-serif; font-size: 1rem; font-weight: 500; color: var(--text); }";
      ".item-kind { display: inline-flex; align-items: center; border-radius: 0.35rem; background: var(--surface-soft); color: var(--muted); font-size: 0.7rem; font-weight: 700; letter-spacing: 0.06em; text-transform: uppercase; padding: 4px 7px; }";
      ".item-summary { color: var(--muted); font-size: 0.96rem; line-height: 1.4; min-width: 0; }";
      ".item-detail-list { display: grid; gap: 0; }";
      ".item-detail + .item-detail { margin-top: 18px; padding-top: 18px; border-top: 1px solid color-mix(in srgb, var(--border) 75%, transparent); }";
      ".item-detail-title { margin: 0; font-family: 'Space Grotesk', sans-serif; font-size: 1.02rem; font-weight: 500; }";
      ".item-detail-title a { color: var(--text); }";
      ".item-detail-title a:hover { color: var(--accent); }";
      ".item-detail-summary { margin-top: 8px; color: var(--muted); font-size: 0.96rem; line-height: 1.5; }";
      ".item-subsections { display: grid; gap: 16px; margin-top: 14px; }";
      ".item-subsection h4 { margin: 0 0 8px; font-size: 0.84rem; letter-spacing: 0.08em; text-transform: uppercase; color: var(--muted); }";
      ".item-subitem-list { display: grid; }";
      ".item-subitem { display: grid; grid-template-columns: minmax(12rem, 22rem) minmax(0, 1fr); gap: 14px; align-items: start; padding: 8px 0; border-top: 1px solid color-mix(in srgb, var(--border) 65%, transparent); }";
      ".item-subitem:first-child { border-top: 0; padding-top: 0; }";
      ".item-subitem-signature { font-family: 'SFMono-Regular', Consolas, monospace; font-size: 0.94rem; line-height: 1.45; white-space: pre-wrap; word-break: break-word; }";
      ".item-subitem-docstring { color: var(--muted); line-height: 1.6; }";
      ".item-meta { margin-top: 4px; color: var(--muted); font-size: 0.92rem; }";
      ".item-signature { margin-top: 10px; padding: 10px 12px; border: 1px solid var(--border); border-radius: 0.45rem; background: var(--surface-soft); overflow-x: auto; }";
      ".item-signature code { white-space: pre-wrap; word-break: break-word; }";
      ".item-docstring, .module-docstring { margin-top: 12px; color: var(--text); line-height: 1.7; }";
      ".item-docstring { color: var(--muted); }";
      ".module-docstring { color: var(--muted); max-width: 72ch; }";
      ".item-docstring > :first-child, .module-docstring > :first-child, .item-subitem-docstring > :first-child { margin-top: 0; }";
      ".item-docstring > :last-child, .module-docstring > :last-child, .item-subitem-docstring > :last-child { margin-bottom: 0; }";
      ".item-docstring p, .module-docstring p, .item-subitem-docstring p { margin: 0.7rem 0 0; }";
      ".item-docstring :is(ul, ol), .module-docstring :is(ul, ol), .item-subitem-docstring :is(ul, ol) { margin: 0.8rem 0 0; padding-left: 1.25rem; list-style: revert; }";
      ".item-docstring li + li, .module-docstring li + li, .item-subitem-docstring li + li { margin-top: 0.3rem; }";
      ".item-docstring :is(h1, h2, h3, h4), .module-docstring :is(h1, h2, h3, h4), .item-subitem-docstring :is(h1, h2, h3, h4) { margin: 1rem 0 0; font-family: 'Space Grotesk', sans-serif; color: var(--text); line-height: 1.2; }";
      ".item-docstring blockquote, .module-docstring blockquote, .item-subitem-docstring blockquote { margin: 0.9rem 0 0; padding-left: 0.9rem; border-left: 3px solid var(--border-strong); color: var(--muted); }";
      ".item-docstring pre, .module-docstring pre, .item-subitem-docstring pre { margin: 0.9rem 0 0; padding: 14px 16px; border: 1px solid var(--border); border-radius: 0.45rem; background: var(--surface-soft); overflow-x: auto; }";
      ".item-docstring pre code, .module-docstring pre code, .item-subitem-docstring pre code { white-space: pre; }";
      ".item-docstring :not(pre) > code, .module-docstring :not(pre) > code, .item-subitem-docstring :not(pre) > code { padding: 0.1rem 0.35rem; border-radius: 0.35rem; background: var(--surface-soft); border: 1px solid var(--border); }";
      ".summary-block { margin-top: 8px; }";
      ".summary-toggle { display: flex; justify-content: flex-end; align-items: center; gap: 0.35rem; cursor: pointer; color: var(--muted); font-size: 0.78rem; user-select: none; text-transform: lowercase; }";
      ".summary-toggle:hover { color: var(--text); }";
      ".summary-block[open] .summary-toggle { color: var(--text); }";
      ".summary-toggle::after { content: '▾'; font-size: 0.7rem; }";
      ".summary-block[open] .summary-toggle::after { content: '▴'; }";
      ".summary-block > summary { list-style: none; }";
      ".summary-block > summary::-webkit-details-marker { display: none; }";
      ".item-snippet { margin-top: 12px; border-radius: 0.45rem; overflow-x: auto; border: 1px solid var(--border); background: var(--surface-soft); }";
      ".item-snippet code { display: block; padding: 18px; white-space: pre; }";
      ".muted { color: var(--muted); }";
      ".empty-state { padding: 14px; border: 1px dashed var(--border-strong); border-radius: 0.45rem; background: var(--surface-soft); color: var(--muted); }";
      ".section-header > a, .page-header a[href=\"source.html\"], .page-header a[href=\"index.html\"] { color: var(--muted); font-size: 0.92rem; }";
      ".page-header a[href=\"source.html\"], .page-header a[href=\"index.html\"]:hover { color: var(--text); }";
      ".item-snippet .hljs { color: var(--text); background: transparent; }";
      ".item-snippet .hljs-comment, .item-snippet .hljs-quote { color: var(--muted); font-style: italic; }";
      ".item-snippet .hljs-keyword, .item-snippet .hljs-selector-tag, .item-snippet .hljs-literal, .item-snippet .hljs-meta .hljs-keyword { color: var(--accent-strong); }";
      ".item-snippet .hljs-string, .item-snippet .hljs-regexp, .item-snippet .hljs-subst { color: var(--code-string); }";
      ".item-snippet .hljs-title, .item-snippet .hljs-title.function_, .item-snippet .hljs-function .hljs-title, .item-snippet .hljs-attr, .item-snippet .hljs-property { color: var(--code-title); }";
      ".item-snippet .hljs-number, .item-snippet .hljs-symbol, .item-snippet .hljs-bullet { color: var(--code-number); }";
      ".item-snippet .hljs-type, .item-snippet .hljs-built_in, .item-snippet .hljs-class .hljs-title { color: var(--code-type); }";
      ".item-snippet .hljs-variable, .item-snippet .hljs-params, .item-snippet .hljs-name { color: var(--text); }";
      "@media (max-width: 980px) { .docs-shell { grid-template-columns: 1fr; width: min(100vw - 20px, 1120px); } .sidebar { position: static; } }";
      "@media (max-width: 640px) { .page-header, .section-card, .sidebar { padding: 16px; } .page-title { font-size: 1.9rem; } .item-row, .item-subitem { grid-template-columns: 1fr; gap: 4px; } }";
    ]
    |> String.concat "\n"
  );
]

let render_empty_state = fun message ->
  "<div class=\"empty-state\">" ^ escape_html message ^ "</div>"

let render_sidebar_group = fun ~title links ->
  if links = [] then
    ""
  else
    "<section class=\"sidebar-group\">\n"
    ^ "  <h2>"
    ^ escape_html title
    ^ "</h2>\n"
    ^ "  <ul>\n"
    ^ (
      links
      |> List.map
        ~fn:(fun (href, label) ->
          "    <li><a href=\"" ^ href ^ "\">" ^ escape_html label ^ "</a></li>")
      |> String.concat "\n"
    )
    ^ "\n  </ul>\n"
    ^ "</section>\n"

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
  "<article class=\"item-detail\" id=\""
  ^ escape_html item.anchor
  ^ "\">\n"
  ^ "  <h3 class=\"item-detail-title\"><a href=\"#"
  ^ escape_html item.anchor
  ^ "\">"
  ^ escape_html item.name
  ^ "</a></h3>\n"
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
  ^ "<html>\n"
  ^ "<head>\n"
  ^ "  <meta charset=\"utf-8\" />\n"
  ^ "  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\" />\n"
  ^ "  <title>"
  ^ escape_html title
  ^ "</title>\n"
  ^ "  <link rel=\"stylesheet\" href=\""
  ^ css_href
  ^ "\" />\n"
  ^ "  <link rel=\"stylesheet\" href=\"https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.11.1/styles/github.min.css\" />\n"
  ^ "</head>\n"

let render_common_scripts = fun () ->
  "  <script defer src=\"https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.11.1/highlight.min.js\"></script>\n"
  ^ "  <script defer src=\"https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.11.1/languages/ocaml.min.js\"></script>\n"
  ^ "  <script>window.addEventListener('DOMContentLoaded', function () { if (window.hljs) hljs.highlightAll(); });</script>\n"

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
  render_common_head "assets/doc.css" (package_doc.package ^ " docs")
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
  ^ "        <div class=\"page-title\">"
  ^ escape_html package_doc.package
  ^ "</div>\n" ^ (
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
    (Doctree.module_full_name module_doc ^ " - docs")
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
  ^ render_sidebar_group ~title:"Types" (sidebar_items Doctree.Type_item)
  ^ render_sidebar_group ~title:"Functions" (sidebar_items Doctree.Function_item)
  ^ render_sidebar_group ~title:"Macros" (sidebar_items Doctree.Macro_item)
  ^ "    </aside>\n"
  ^ "    <main class=\"content\">\n"
  ^ "      <header class=\"page-header\">\n"
  ^ "        <div class=\"breadcrumbs\">"
  ^ render_module_breadcrumbs package_doc.package module_doc
  ^ "</div>\n"
  ^ "        <div class=\"section-header\">\n"
  ^ "          <div>\n"
  ^ "            <div class=\"eyebrow\">Module page</div>\n"
  ^ "            <div class=\"page-title\">"
  ^ escape_html (Doctree.module_full_name module_doc)
  ^ "</div>\n"
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
  ^ "            <div class=\"page-title\">"
  ^ escape_html (Doctree.module_full_name module_doc)
  ^ "</div>\n"
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
