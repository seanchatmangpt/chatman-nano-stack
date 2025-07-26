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
  
  runtimeConfig: {
    public: {
      dataApiUrl: process.env.DATA_API_URL || 'http://localhost:8000'
    }
  }
})