import starlightPlugin from "@astrojs/starlight-tailwind";

import starlightPlugin from "@astrojs/starlight-tailwind";

// Generated color palettes
const accent = {
  200: "#feb1b4",
  600: "#a60030",
  900: "#630019",
  950: "#460913",
};
const gray = {
  100: "#f7f6f5",
  200: "#f0edec",
  300: "#c5c1c0",
  400: "#918a88",
  500: "#5d5655",
  700: "#3d3735",
  800: "#2b2524",
  900: "#1a1717",
};

/** @type {import('tailwindcss').Config} */
export default {
  content: ["./src/**/*.{astro,html,js,jsx,md,mdx,svelte,ts,tsx,vue}"],
  theme: {
    extend: { accent, gray },
  },
  plugins: [starlightPlugin()],
};
