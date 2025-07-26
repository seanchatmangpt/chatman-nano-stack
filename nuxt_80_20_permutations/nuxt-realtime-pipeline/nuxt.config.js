export default defineNuxtConfig({
  ssr: false, // SPA for real-time performance
  
  modules: [
    '@nuxtjs/tailwindcss',
    '@pinia/nuxt',
    '@vueuse/nuxt'
  ],
  
  runtimeConfig: {
    public: {
      pipelineWsUrl: process.env.PIPELINE_WS_URL || 'ws://localhost:9000',
      refreshInterval: 1000
    }
  },
  
  nitro: {
    experimental: {
      websocket: true
    }
  }
})