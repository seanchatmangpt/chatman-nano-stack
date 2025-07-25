// https://nuxt.com/docs/api/configuration/nuxt-config
export default defineNuxtConfig({
  devtools: { enabled: true },

  // TypeScript support
  typescript: {
    strict: true,
    typeCheck: true,
  },

  // CSS framework
  css: ["~/assets/css/main.css"],

  // Modules
  modules: ["@nuxtjs/tailwindcss", "@pinia/nuxt", "@vueuse/nuxt"],

  // Tailwind CSS configuration
  tailwindcss: {
    config: {
      content: [
        "~/components/**/*.{vue,js,ts}",
        "~/layouts/**/*.vue",
        "~/pages/**/*.vue",
        "~/plugins/**/*.{js,ts}",
        "~/app.vue",
      ],
      theme: {
        extend: {
          colors: {
            "cyber-blue": "#00d4ff",
            "cyber-green": "#00ff88",
            "cyber-red": "#ff0040",
            "cyber-yellow": "#ffaa00",
            "cyber-purple": "#aa00ff",
            "cyber-gray": "#1a1a1a",
            "cyber-dark": "#0a0a0a",
          },
          animation: {
            "pulse-slow": "pulse 3s cubic-bezier(0.4, 0, 0.6, 1) infinite",
            glow: "glow 2s ease-in-out infinite alternate",
          },
          keyframes: {
            glow: {
              "0%": { boxShadow: "0 0 5px #00d4ff" },
              "100%": { boxShadow: "0 0 20px #00d4ff, 0 0 30px #00d4ff" },
            },
          },
        },
      },
    },
  },

  // Runtime config
  runtimeConfig: {
    public: {
      wsUrl: process.env.WS_URL || "ws://localhost:3000/api/aegis/ws",
      apiBase: process.env.API_BASE || "http://localhost:3000/api",
    },
  },

  // App configuration
  app: {
    head: {
      title: "Aegis Fabric - Cybersecurity Dashboard",
      meta: [
        { charset: "utf-8" },
        { name: "viewport", content: "width=device-width, initial-scale=1" },
        {
          name: "description",
          content:
            "Real-time cybersecurity threat detection and response dashboard powered by Aegis Fabric",
        },
      ],
      link: [{ rel: "icon", type: "image/x-icon", href: "/favicon.ico" }],
    },
  },

  // Nitro configuration for WebSocket support
  nitro: {
    experimental: {
      websocket: true,
    },
  },

  // Build configuration
  build: {
    transpile: ["vue-toastification"],
  },

  // Vite configuration
  vite: {
    define: {
      "process.env.NODE_ENV": JSON.stringify(process.env.NODE_ENV),
    },
  },
});
