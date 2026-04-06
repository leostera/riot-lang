// @ts-check
import { readdirSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { defineConfig } from "astro/config";
import starlight from "@astrojs/starlight";
import { buildSidebar } from "./docs-nav.mjs";

const currentDir = dirname(fileURLToPath(import.meta.url));
const rfdsDir = resolve(currentDir, "../../docs/rfds");
const rfdSidebarItems = readdirSync(rfdsDir)
  .filter((entry) => /^RFD\d{4}-.+\.md$/i.test(entry))
  .sort((left, right) => left.localeCompare(right))
  .map((entry) => {
    const basename = entry.replace(/\.md$/i, "");
    const [code, ...rest] = basename.split("-");
    const label = rest.join(" ").replace(/\b\w/g, (letter) => letter.toUpperCase());
    return {
      label: `${code.toUpperCase()} · ${label}`,
      slug: `rfds/${basename.toLowerCase()}`,
    };
  });

export default defineConfig({
  site: "https://docs.riot.ml",
  integrations: [
    starlight({
      title: "Riot Docs",
      description: "Reference and guides for Riot’s toolchain, package manager, and test runner.",
      customCss: ["./src/styles/custom.css"],
      components: {
        Header: "./src/components/starlight/Header.astro",
        Sidebar: "./src/components/starlight/Sidebar.astro",
      },
      social: [
        { icon: "github", label: "GitHub", href: "https://github.com/leostera/riot" }
      ],
      sidebar: buildSidebar(rfdSidebarItems)
    })
  ]
});
