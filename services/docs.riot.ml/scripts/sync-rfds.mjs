import { mkdirSync, readdirSync, readFileSync, rmSync, writeFileSync } from "node:fs";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const currentDir = dirname(fileURLToPath(import.meta.url));
const serviceDir = resolve(currentDir, "..");
const sourceDir = resolve(serviceDir, "../../docs/rfds");
const outputDir = resolve(serviceDir, "rfds");
const docsConfigPath = resolve(serviceDir, "docs.json");

mkdirSync(outputDir, { recursive: true });

for (const entry of readdirSync(outputDir)) {
  if (/^rfd\d{4}-.+\.mdx$/i.test(entry)) {
    rmSync(join(outputDir, entry), { force: true });
  }
}

const toSentenceCase = (value) =>
  value
    .replace(/-/g, " ")
    .replace(/\b\w/g, (letter) => letter.toUpperCase());

const escapeMdxText = (value) =>
  value
    .replaceAll("&", "&amp;")
    .replaceAll("<", "&lt;")
    .replaceAll(">", "&gt;");

const links = [];
const pages = ["/rfds/index"];

for (const entry of readdirSync(sourceDir).sort((left, right) => left.localeCompare(right))) {
  if (!/^RFD\d{4}-.+\.md$/i.test(entry)) {
    continue;
  }

  const sourcePath = join(sourceDir, entry);
  const outputName = entry.toLowerCase().replace(/\.md$/i, ".mdx");
  const outputPath = join(outputDir, outputName);
  const source = readFileSync(sourcePath, "utf8");
  const lines = source.split("\n");
  const isTemplate = /^RFD0000-template\.md$/i.test(entry);
  const titleLine = lines.find((line) => line.startsWith("# ")) ?? `# ${entry.replace(/\.md$/i, "")}`;
  const rawTitle = titleLine.replace(/^#\s+/, "").trim();
  const title = isTemplate ? "RFD0000 - Template" : rawTitle;
  const statusLine = lines.find((line) => line.startsWith("- Status:"));
  const rawStatus = statusLine ? statusLine.replace(/^- Status:\s*`?([^`]+)`?/, "$1").trim() : "unknown";
  const status = isTemplate ? "template" : rawStatus;
  const bodyWithoutTitle = lines.slice(lines.indexOf(titleLine) + 1).join("\n").trimStart();
  const body = isTemplate ? escapeMdxText(bodyWithoutTitle) : bodyWithoutTitle;
  const repoPath = `docs/rfds/${entry}`;
  const pagePath = `/rfds/${outputName.replace(/\.mdx$/i, "")}`;

  const frontmatter = [
    "---",
    `title: "${title.replaceAll('"', '\\"')}"`,
    `description: "Riot Request for Discussion · ${status}"`,
    "---",
    "",
  ].join("\n");

  writeFileSync(outputPath, `${frontmatter}\n${body}`);
  links.push(`- [${title}](${pagePath}) · \`${toSentenceCase(status)}\``);
  pages.push(pagePath);
}

const index = [
  "---",
  'title: "RFDs"',
  'description: "Requests for Discussion synced from the Riot repository."',
  "---",
  "",
  "RFDs capture design intent, tradeoffs, and architectural decisions for Riot.",
  "",
  "The pages below are synced from the canonical sources in `docs/rfds`.",
  "",
  ...links,
  "",
].join("\n");

writeFileSync(join(outputDir, "index.mdx"), index);

const docsConfig = JSON.parse(readFileSync(docsConfigPath, "utf8"));
const rfdsTab = docsConfig.navigation.tabs.find((tab) => tab.tab == "RFDs");
const rfdsGroup = rfdsTab?.groups?.find((group) => group.group == "Design Documents");

if (rfdsGroup) {
  rfdsGroup.pages = pages;
  writeFileSync(docsConfigPath, `${JSON.stringify(docsConfig, null, 2)}\n`);
}
