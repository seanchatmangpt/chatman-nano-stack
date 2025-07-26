export default defineNuxtConfig({
  modules: [
    '@nuxtjs/tailwindcss',
    '@pinia/nuxt',
    'nuxt-vue-flow'
  ],
  
  runtimeConfig: {
    public: {
      ashApiUrl: process.env.ASH_API_URL || 'http://localhost:4000',
      reactorApiUrl: process.env.REACTOR_API_URL || 'http://localhost:4001',
      k8sApiUrl: process.env.K8S_API_URL || 'http://localhost:8001'
    }
  }
})