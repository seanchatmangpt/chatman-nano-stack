export default defineNuxtConfig({
  modules: [
    '@nuxtjs/apollo',
    '@pinia/nuxt',
    '@nuxtjs/tailwindcss'
  ],
  
  apollo: {
    clients: {
      default: {
        httpEndpoint: 'http://localhost:4000/api/graphql',
        wsEndpoint: 'ws://localhost:4000/socket'
      }
    }
  },
  
  css: ['~/assets/css/main.css'],
  
  runtimeConfig: {
    public: {
      ashApiUrl: process.env.ASH_API_URL || 'http://localhost:4000'
    }
  }
})