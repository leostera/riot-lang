// @ts-check
import { defineConfig } from "astro/config";
import starlight from "@astrojs/starlight";

export default defineConfig({
  site: "https://docs.riot.ml",
  integrations: [
    starlight({
      title: "Riot Docs",
      description: "Reference and guides for Riot, the OCaml build system, runtime, and registry stack.",
      social: [
        { icon: "github", label: "GitHub", href: "https://github.com/leostera/riot" },
      ],
      sidebar: [
        {
          label: "Start Here",
          items: [
            { label: "Introduction", slug: "start-here/introduction" },
            { label: "Installation", slug: "start-here/installation" },
          ],
        },
        {
          label: "Tool Reference",
          items: [
            { label: "CLI Overview", slug: "reference/cli" },
            { label: "Command Surface", slug: "reference/commands" },
          ],
        },
        {
          label: "Registry",
          items: [
            { label: "Registry Overview", slug: "registry/overview" },
            { label: "API and Sparse Index", slug: "registry/api" },
          ],
        },
        {
          label: "Architecture",
          items: [
            { label: "Runtime and Stack", slug: "architecture/runtime" },
          ],
        },
      ],
    }),
  ],
});
