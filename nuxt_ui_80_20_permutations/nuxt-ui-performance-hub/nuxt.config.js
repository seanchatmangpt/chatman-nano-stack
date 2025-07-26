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
  
  css: ['~/assets/css/performance-hub.css'],
  
  runtimeConfig: {
    public: {
      metricsApiUrl: process.env.METRICS_API_URL || 'http://localhost:9090',
      bitactorApiUrl: process.env.BITACTOR_API_URL || 'http://localhost:8080',
      k8sApiUrl: process.env.K8S_API_URL || 'http://localhost:8001'
    }
  }
})