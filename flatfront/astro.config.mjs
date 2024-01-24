import { defineConfig } from "astro/config";
import react from "@astrojs/react";

import tailwind from "@astrojs/tailwind";

// https://astro.build/config
export default defineConfig({
  server: {
    host: true,
    port: 5432
  },
  integrations: [react(), tailwind()]
});
