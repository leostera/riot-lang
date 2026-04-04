import { mkdirSync, readdirSync, readFileSync, rmSync, writeFileSync } from "node:fs";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const currentDir = dirname(fileURLToPath(import.meta.url));
const serviceDir = resolve(currentDir, "..");
const sourceDir = resolve(serviceDir, "../../docs/rfds");
const outputDir = resolve(serviceDir, "src/content/docs/rfds");

mkdirSync(outputDir, { recursive: true });

for (const entry of readdirSync(outputDir)) {
  if (/^rfd\d{4}-.+\.md$/i.test(entry)) {
    rmSync(join(outputDir, entry), { force: true });
  }
}

const toSentenceCase = (value) =>
  value
    .replace(/-/g, " ")
    .replace(/\b\w/g, (letter) => letter.toUpperCase());

for (const entry of readdirSync(sourceDir).sort((left, right) => left.localeCompare(right))) {
  if (!/^RFD\d{4}-.+\.md$/i.test(entry)) {
    continue;
  }

  const sourcePath = join(sourceDir, entry);
  const outputName = entry.toLowerCase();
  const outputPath = join(outputDir, outputName);
  const source = readFileSync(sourcePath, "utf8");
  const lines = source.split("\n");
  const titleLine = lines.find((line) => line.startsWith("# ")) ?? `# ${entry.replace(/\.md$/i, "")}`;
  const title = titleLine.replace(/^#\s+/, "").trim();
  const statusLine = lines.find((line) => line.startsWith("- Status:"));
  const status = statusLine ? statusLine.replace(/^- Status:\s*`?([^`]+)`?/, "$1").trim() : "unknown";
  const bodyWithoutTitle = lines.slice(lines.indexOf(titleLine) + 1).join("\n").trimStart();
  const repoPath = `docs/rfds/${entry}`;

  const frontmatter = [
    "---",
    `title: "${title.replaceAll('"', '\\"')}"`,
    `description: "Riot Request for Discussion · ${status}"`,
    "---",
    "",
    `> Canonical source: \`${repoPath}\``,
    "",
    `> Status: **${toSentenceCase(status)}**`,
    "",
  ].join("\n");

  writeFileSync(outputPath, `${frontmatter}\n${bodyWithoutTitle}`);
}
