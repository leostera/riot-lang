export const topTabs = [
  {
    label: "Toolchain",
    href: "/toolchain/installation/",
    prefix: "/toolchain/",
    description: "Installation, daily commands, runtime facilities, and the built-in developer loop.",
  },
  {
    label: "Package Manager",
    href: "/package-manager/",
    prefix: "/package-manager/",
    description: "Dependencies, installs, publishing, docs generation, lockfiles, and caches.",
  },
  {
    label: "Test Runner",
    href: "/test-runner/",
    prefix: "/test-runner/",
    description: "Tests, property testing, snapshots, and benchmarks.",
  },
];

export const toolchainSidebar = [
  {
    label: "Get Started",
    items: [
      { label: "Installation", slug: "toolchain/installation" },
      { label: "Quick Start", slug: "toolchain/quickstart" },
      { label: "Editor Setup", slug: "toolchain/editor-setup" },
      { label: "Visual Studio Code", slug: "toolchain/vscode" },
      { label: "Neovim", slug: "toolchain/neovim" },
      { label: "OCaml 5", slug: "toolchain/ocaml5" },
      { label: "riot init", slug: "toolchain/riot-init" },
      { label: "riot new", slug: "toolchain/riot-new" },
      { label: "Shell Completions", slug: "toolchain/shell-completions" },
    ],
  },
  {
    label: "Core Commands",
    items: [
      { label: "riot run", slug: "toolchain/riot-run" },
      { label: "riot build", slug: "toolchain/build" },
      { label: "riot check", slug: "toolchain/check" },
      { label: "riot fix", slug: "toolchain/fix" },
      { label: "riot fmt", slug: "toolchain/fmt" },
      { label: "ocaml-toolchain.toml and riot toolchain", slug: "toolchain/toolchain-config" },
      { label: "riot clean", slug: "toolchain/clean" },
    ],
  },
  {
    label: "Core Runtime",
    items: [
      { label: "Actor Runtime", slug: "toolchain/actor-runtime" },
      { label: "REPL", slug: "toolchain/repl" },
    ],
  },
  {
    label: "Packages & Modules",
    items: [{ label: "Package Structure", slug: "toolchain/package-structure" }],
  },
  {
    label: "Standard Library",
    items: [{ label: "Collections & Utilities", slug: "toolchain/collections" }],
  },
  {
    label: "Networking",
    items: [
      { label: "Server", slug: "toolchain/server" },
      { label: "TCP", slug: "toolchain/tcp" },
      { label: "UDP", slug: "toolchain/udp" },
    ],
  },
  {
    label: "Data Formats",
    items: [
      { label: "JSON", slug: "toolchain/json" },
      { label: "TOML", slug: "toolchain/toml" },
    ],
  },
  {
    label: "Interop",
    items: [{ label: "Interop", slug: "toolchain/interop" }],
  },
];

export const packageManagerSidebar = [
  { label: "Overview", slug: "package-manager" },
  {
    label: "Core Commands",
    items: [
      { label: "riot build", slug: "package-manager/build" },
      { label: "riot add", slug: "package-manager/add" },
      { label: "riot remove", slug: "package-manager/remove" },
      { label: "riot update", slug: "package-manager/update" },
      { label: "riot search", slug: "package-manager/search" },
      { label: "riot run", slug: "package-manager/run" },
      { label: "riot install", slug: "package-manager/install" },
    ],
  },
  {
    label: "Publishing & Analysis",
    items: [
      { label: "riot login", slug: "package-manager/login" },
      { label: "riot logout", slug: "package-manager/logout" },
      { label: "riot publish", slug: "package-manager/publish" },
      { label: "riot yank", slug: "package-manager/yank" },
    ],
  },
  {
    label: "Documentation Generation",
    items: [{ label: "riot doc", slug: "package-manager/doc" }],
  },
  {
    label: "Advanced Topics",
    items: [
      { label: "Lockfiles", slug: "package-manager/lockfiles" },
      { label: "Global & Local Caches", slug: "package-manager/caches" },
      { label: "Isolated Installs", slug: "package-manager/isolated-installs" },
      { label: "riot clean", slug: "package-manager/clean" },
    ],
  },
];

export const testRunnerSidebar = [
  { label: "Overview", slug: "test-runner" },
  {
    label: "Getting Started",
    items: [
      { label: "riot test", slug: "test-runner/riot-test" },
      { label: "Writing Tests with Std.Test", slug: "test-runner/writing-tests" },
      { label: "Configuring Tests", slug: "test-runner/configuring-tests" },
    ],
  },
  {
    label: "Property Testing",
    items: [{ label: "Property Testing with Propane", slug: "test-runner/property-testing-with-propane" }],
  },
  {
    label: "Snapshot Testing",
    items: [
      { label: "Std.Test.Snapshot", slug: "test-runner/std-test-snapshot" },
      { label: "riot snapshots", slug: "test-runner/riot-snapshot" },
      { label: "How Snapshot Files Work", slug: "test-runner/snapshot-files" },
    ],
  },
  {
    label: "Benchmarks",
    items: [
      { label: "Std.Bench", slug: "test-runner/std-bench" },
      { label: "riot bench", slug: "test-runner/riot-bench" },
    ],
  },
];

export function buildSidebar(rfdSidebarItems) {
  return [
    { label: "Toolchain", items: toolchainSidebar },
    { label: "Package Manager", items: packageManagerSidebar },
    { label: "Test Runner", items: testRunnerSidebar },
    {
      label: "RFDs",
      items: [{ label: "Why RFDs?", slug: "rfds" }, ...rfdSidebarItems],
    },
  ];
}

export function getSectionLabelForPath(pathname) {
  if (pathname === "/" || pathname === "/index.html") return "Toolchain";
  if (pathname.startsWith("/package-manager/")) return "Package Manager";
  if (pathname.startsWith("/test-runner/")) return "Test Runner";
  if (pathname.startsWith("/rfds/")) return "RFDs";
  if (pathname.startsWith("/toolchain/")) return "Toolchain";
  return null;
}

export function getTopTabForPath(pathname) {
  if (pathname === "/" || pathname === "/index.html") {
    return topTabs[0] ?? null;
  }
  return topTabs.find((tab) => pathname.startsWith(tab.prefix)) ?? null;
}
