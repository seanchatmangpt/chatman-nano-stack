export default defineNuxtConfig({
  modules: [
    '@nuxt/ui',
    '@vueuse/nuxt',
    '@pinia/nuxt'
  ],
  
  ui: {
    global: true,
    icons: ['heroicons', 'lucide']
  },
  
  css: ['~/assets/css/command-center.css'],
  
  runtimeConfig: {
    public: {
      pipelineApiUrl: process.env.PIPELINE_API_URL || 'ws://localhost:9000',
      commandCenterMode: 'production'
    }
  },
  
  ssr: false // SPA for real-time performance
})