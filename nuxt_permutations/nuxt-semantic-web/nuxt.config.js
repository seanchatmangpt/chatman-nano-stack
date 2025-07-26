export default defineNuxtConfig({
  nitro: {
    prerender: {
      routes: ['/ontology', '/schema']
    }
  },
  
  modules: [
    '@nuxtjs/robots',
    '@nuxtjs/sitemap',
    '@nuxtjs/tailwindcss'
  ],
  
  robots: {
    UserAgent: '*',
    Disallow: '/admin',
    Sitemap: 'https://example.com/sitemap.xml'
  },
  
  sitemap: {
    hostname: 'https://example.com',
    gzip: true
  },
  
  head: {
    meta: [
      { name: 'format-detection', content: 'telephone=no' }
    ]
  }
})