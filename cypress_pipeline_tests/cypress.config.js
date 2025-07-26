const { defineConfig } = require('cypress')

module.exports = defineConfig({
  "e2e": {
    "baseUrl": "http://localhost:3000",
    "supportFile": "cypress/support/e2e.js",
    "specPattern": "cypress/e2e/**/*.cy.js",
    "viewportWidth": 1920,
    "viewportHeight": 1080,
    "video": true,
    "screenshotOnRunFailure": true,
    "defaultCommandTimeout": 10000,
    "requestTimeout": 10000,
    "responseTimeout": 10000,
    "env": {
      "PIPELINE_API_URL": "http://localhost:9000",
      "ASH_API_URL": "http://localhost:4000",
      "REACTOR_API_URL": "http://localhost:4001",
      "BITACTOR_API_URL": "http://localhost:8080",
      "K8S_API_URL": "http://localhost:8001"
    }
  },
  "component": {
    "devServer": {
      "framework": "nuxt",
      "bundler": "vite"
    }
  }
})