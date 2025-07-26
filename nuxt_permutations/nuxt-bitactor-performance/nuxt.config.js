export default defineNuxtConfig({
  ssr: false, // SPA mode for performance
  
  modules: [
    '@nuxtjs/tailwindcss',
    '@pinia/nuxt'
  ],
  
  plugins: [
    '~/plugins/websocket.client.js'
  ],
  
  runtimeConfig: {
    public: {
      bitactorWsUrl: process.env.BITACTOR_WS_URL || 'ws://localhost:8080'
    }
  }
})