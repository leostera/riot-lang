import { getConfig } from "./config.ts";
import type { SearchResponse } from "./types.ts";

export async function fetchSearchResults(query: string): Promise<SearchResponse> {
  const trimmed = query.trim();
  const response = await fetch(
    `${getConfig().searchBaseUrl}/?q=${encodeURIComponent(trimmed)}`,
    {
      headers: {
        accept: "application/json",
      },
    },
  );

  if (!response.ok) {
    throw new Error(`Search request failed: ${response.status}`);
  }

  return (await response.json()) as SearchResponse;
}
