export default defineNuxtConfig({
  modules: [
    '@nuxt/ui',
    '@vueuse/nuxt',
    '@pinia/nuxt'
  ],
  
  ui: {
    global: true,
    icons: ['heroicons', 'lucide', 'simple-icons']
  },
  
  css: ['~/assets/css/workflow-studio.css'],
  
  runtimeConfig: {
    public: {
      ashApiUrl: process.env.ASH_API_URL || 'http://localhost:4000',
      reactorApiUrl: process.env.REACTOR_API_URL || 'http://localhost:4001'
    }
  }
})