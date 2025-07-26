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
  
  css: ['~/assets/css/semantic-playground.css'],
  
  runtimeConfig: {
    public: {
      typerApiUrl: process.env.TYPER_API_URL || 'http://localhost:7000',
      turtleApiUrl: process.env.TURTLE_API_URL || 'http://localhost:7001',
      ttl2dspyApiUrl: process.env.TTL2DSPY_API_URL || 'http://localhost:7002',
      ashApiUrl: process.env.ASH_API_URL || 'http://localhost:4000'
    }
  }
})