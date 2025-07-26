export default defineNuxtConfig({
  modules: [
    '@nuxtjs/tailwindcss',
    '@pinia/nuxt'
  ],
  
  runtimeConfig: {
    public: {
      pipelineApiUrl: process.env.PIPELINE_API_URL || 'http://localhost:8000'
    }
  }
})