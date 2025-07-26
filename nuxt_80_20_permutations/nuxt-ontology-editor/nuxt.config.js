export default defineNuxtConfig({
  modules: [
    '@nuxtjs/tailwindcss',
    '@pinia/nuxt',
    'nuxt-monaco-editor'
  ],
  
  runtimeConfig: {
    public: {
      typerApiUrl: process.env.TYPER_API_URL || 'http://localhost:8000',
      ashApiUrl: process.env.ASH_API_URL || 'http://localhost:4000'
    }
  }
})