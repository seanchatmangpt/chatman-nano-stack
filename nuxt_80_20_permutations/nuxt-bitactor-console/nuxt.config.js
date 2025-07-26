export default defineNuxtConfig({
  ssr: false,
  
  modules: [
    '@nuxtjs/tailwindcss',
    '@pinia/nuxt'
  ],
  
  runtimeConfig: {
    public: {
      bitactorApiUrl: process.env.BITACTOR_API_URL || 'http://localhost:8080',
      erlangNodeUrl: process.env.ERLANG_NODE_URL || 'http://localhost:9090',
      k8sApiUrl: process.env.K8S_API_URL || 'http://localhost:8001'
    }
  }
})