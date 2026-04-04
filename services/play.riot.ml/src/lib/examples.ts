export function exampleLabel(path: string): string {
  return path.replace(/^examples\//, "").replace(/\.ml$/i, "");
}

export function exampleHref(packageName: string, version: string, path: string): string {
  const encodedPath = path
    .replace(/^examples\//, "")
    .split("/")
    .map((segment) => encodeURIComponent(segment))
    .join("/");

  return `/p/${encodeURIComponent(packageName)}/${encodeURIComponent(version)}/examples/${encodedPath}`;
}

export function examplePathFromRoute(pathnameTail: string): string {
  return `examples/${pathnameTail
    .split("/")
    .filter((segment) => segment.length > 0)
    .map((segment) => decodeURIComponent(segment))
    .join("/")}`;
}
