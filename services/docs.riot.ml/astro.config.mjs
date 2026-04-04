// @ts-check
import { readdirSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { defineConfig } from "astro/config";
import starlight from "@astrojs/starlight";

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
      description: "Reference and guides for Riot, the OCaml build system, runtime, and registry stack.",
      customCss: ["./src/styles/custom.css"],
      social: [
        { icon: "github", label: "GitHub", href: "https://github.com/leostera/riot" },
      ],
      sidebar: [
        {
          label: "Start Here",
          items: [
            { label: "Introduction", slug: "start-here/introduction" },
            { label: "Installation", slug: "start-here/installation" },
            { label: "Quickstart", slug: "start-here/quickstart" },
          ],
        },
        {
          label: "Tool Reference",
          items: [
            { label: "CLI Overview", slug: "reference/cli" },
            { label: "Common Workflows", slug: "reference/workflows" },
            { label: "Command Reference", slug: "reference/commands" },
            { label: "JSON and Agents", slug: "reference/json-and-agents" },
          ],
        },
        {
          label: "Registry",
          items: [
            { label: "Registry Overview", slug: "registry/overview" },
            { label: "Publishing Packages", slug: "registry/publishing" },
            { label: "API and Sparse Index", slug: "registry/api" },
          ],
        },
        {
          label: "Architecture",
          items: [
            { label: "Runtime and Stack", slug: "architecture/runtime" },
            { label: "The Standard Library", slug: "architecture/std" },
            { label: "Clanker-Friendly Tooling", slug: "architecture/agents" },
          ],
        },
        {
          label: "RFDs",
          items: [
            { label: "Overview", slug: "rfds" },
            ...rfdSidebarItems,
          ],
        },
      ],
    }),
  ],
});
