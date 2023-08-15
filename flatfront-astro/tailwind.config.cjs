const colors = require("tailwindcss/colors");

/** @type {import('tailwindcss').Config} */
module.exports = {
  darkMode: `class`,
  content: ["./src/**/*.{astro,html,js,jsx,md,mdx,svelte,ts,tsx,vue}"],
  theme: {
    extend: {
      colors: {
        "light-text": colors.slate[900],
        "light-0": colors.white,
        "light-1": colors.slate[100],
        "light-2": colors.slate[200],
        "light-3": colors.slate[300],
        "light-4": colors.slate[400],
        "light-5": colors.slate[500],
        "dark-text": colors.slate[50],
        "dark-0": colors.slate[900],
        "dark-1": colors.slate[700],
        "dark-2": colors.slate[600],
        "dark-3": colors.slate[500],
        "dark-4": colors.slate[400],
        "dark-5": colors.slate[300],
      },
    },
  },
  plugins: [require("@headlessui/tailwindcss")],
};
