// @ts-check

const colors = require("tailwindcss/colors");
const defaults = require("tailwindcss/defaultTheme");

/** @type {import('tailwindcss').Config} */
module.exports = {
  darkMode: `class`,
  content: ["./src/**/*.{astro,html,js,jsx,md,mdx,svelte,ts,tsx,vue}"],
  theme: {
    fontSize: {
      xs: ["0.5rem", { lineHeight: "1" }],
      sm: ["0.6rem", { lineHeight: "1" }],
      base: ["0.7rem", { lineHeight: "1" }],
      lg: ["1rem", { lineHeight: "1" }]
    },
    extend: {
      colors: {
        "simons-gray-1": "#4D4D4D",
        "simons-gray-2": "#7F7F7F",
        "simons-gray-3": "#B3B3B3",
        "simons-gray-4": "#F6F6F6",
        "flatiron-blue": "#537EBA",
        "simons-blue": "#1D2954",
        "simons-blue-lighter-1": "#84A5DF",
        "simons-blue-lighter-2": "#D7E4FA",
        "simons-blue-lighter-3": "#F1F3FC",
        "dawn-yellow": "#FFD53D",
        "dawn-yellow-darker": "#B4821D",
        "dawn-yellow-lighter-1": "#EBD9AA",
        "dawn-yellow-lighter-2": "#F8F5D6",
        "ember-orange": "#FF9300",
        "ember-orange-darker": "#A7522F",
        "ember-orange-lighter-1": "#F9D8B6",
        "ember-orange-lighter-2": "#FCF6F1",
        "fire-red": "#FF4102",
        "fire-red-darker": "#901E1D",
        "fire-red-lighter-1": "#F6CAC1",
        "fire-red-lighter-2": "#FFF4F2",
        "cca-theme": "#CE3232",
        "ccb-theme": "#81AD4A",
        "ccm-theme": "#F6862D",
        "ccn-theme": "#007F9D",
        "ccq-theme": "#845B8E",
        "scc-theme": "#8F8F8F"
      },
      screens: {
        desktop: defaults.screens["md"]
      }
    }
  },
  plugins: [require("@tailwindcss/container-queries")]
};
