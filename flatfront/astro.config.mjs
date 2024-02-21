import { defineConfig } from "astro/config";
import react from "@astrojs/react";

import tailwind from "@astrojs/tailwind";

// https://astro.build/config
export default defineConfig({
  base: `/v2beta/`,
  trailingSlash: `always`,
  server: {
    host: true,
    port: 5432
  },
  integrations: [react(), tailwind()],
  vite: {
    define: {
      "import.meta.env.BUILD_TIME": JSON.stringify(new Date().toLocaleString())
    }
  }
});
